{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecursiveDo                #-}

module Strategy where

import           System.IO                (hPutStr, hPutStrLn, stderr)
import           Control.Exception.Base   (finally)
import           Control.Monad            (void)
import           Control.Monad.State 
import           Data.Maybe
import           Data.List.Extended       (safeHead)
import qualified Data.HashMap.Strict as H

import           Reactive.Banana
import           Reactive.Banana.Frameworks.Extended

import           Market.Types
import           Market.Util
import           Razao.Util

import           Combinator
import           TradingFramework

--------------------------------------------------------------------------------
--                     SIMPLE TRADING STRATEGIES
--------------------------------------------------------------------------------
showBook :: (Coin p, Coin v, Show counter, MonadMoment m)
         => place -> cancel -> fill -> Event (QuoteBook p v qtail counter)
         -> m (Event (StrategyAdvice action))
showBook _ _ _ eNewBook = return (toAdvice <$> eNewBook)
  where
    toAdvice book = Advice (backtrackCursor $ showTopN 3 book, ZipList [])

--------------------------------------------------------------------------------
showAllBooks
    :: ( Coin p1, Coin v1, Show c1, Num c1
       , Coin p2, Coin v2, Show c2, Num c2
       , Coin p3, Coin v3, Show c3, Num c3
       , MonadMoment m)
    =>  Event (TradingE p1 v1 q1 c1)
    ->  Event (TradingE p2 v2 q2 c2)
    ->  Event (TradingE p3 v3 q3 c3)
    -> m (Event (StrategyAdvice a1), Event (StrategyAdvice a2), Event (StrategyAdvice a3))
showAllBooks e1s e2s e3s = do
    let (_, _, _, eb1s) = splitEvents e1s
        (_, _, _, eb2s) = splitEvents e2s
        (_, _, _, eb3s) = splitEvents e3s

    b1 <- accumB (QuoteBook {bids = [], asks = [], counter = 0}) (const <$> eb1s)
    b2 <- accumB (QuoteBook {bids = [], asks = [], counter = 0}) (const <$> eb2s)
    b3 <- accumB (QuoteBook {bids = [], asks = [], counter = 0}) (const <$> eb3s)

    return ( (\y z x -> toAdvice x y z) <$> b2 <*> b3 <@> eb1s   -- only fires on eb1s
           , never  -- (\z x y -> toAdvice x y z) <$> b3 <*> b1 <@> eb2s   -- only fires on eb2s
           , never) -- (\x y z -> toAdvice x y z) <$> b1 <*> b2 <@> eb3s)  -- only fires on eb3s

  where
    toAdvice bk1 bk2 bk3 =
      Advice (backtrackCursor $ replicate 50 '#' ++ "\n"
                              ++ showTopN 3 bk1
                              ++ replicate 50 '#' ++ "\n"
                              ++ showTopN 3 bk2
                              ++ replicate 50 '#' ++ "\n"
                              ++ showTopN 3 bk3
                              ++ replicate 50 '#' ++ "\n"
                              ++ "Asks: " ++ show a1 ++ " - " ++ show a2 ++ " - " ++ show a3 ++ "\n"
                              ++ "Bids: " ++ show b1 ++ " - " ++ show b2 ++ " - " ++ show b3 ++ "\n"
                              ++ replicate 50 '#' ++ "\n"
                              ++ "as':  " ++ show da1 ++ " - " ++ show da2 ++ " - " ++ show da3 ++ " (lower is better)\n"
                              ++ "bs':  " ++ show db1 ++ " - " ++ show db2 ++ " - " ++ show db3 ++ " (higher is better)\n"
             , ZipList []
             )
      where
        a1 = best 99999 (asks bk1)
        a2 = best 99999 (asks bk2)
        a3 = best 99999 (asks bk3)
        b1 = best     0 (bids bk1)
        b2 = best     0 (bids bk2)
        b3 = best     0 (bids bk3)
        da1 = Price $ round2dp ((realToFrac a2 * 1.0025 * 1.0025 / realToFrac b3)   :: USD )
        db1 = Price $ round2dp ((realToFrac b2 / realToFrac a3 / (1.0025 * 1.0025)) :: USD )
        da2 = Price $ round2dp ((realToFrac a3 * 1.0025 * 1.0025 * realToFrac a1)   :: USD )
        db2 = Price $ round2dp ((realToFrac b3 * realToFrac b1 / (1.0025 * 1.0025)) :: USD )
        da3 = Price            ((realToFrac a2 * 1.0025 * 1.0025 / realToFrac b1)   :: BTC )
        db3 = Price            ((realToFrac b2 / realToFrac a1 / (1.0025 * 1.0025)) :: BTC )


    best :: (Coin p, Coin v) => Price p -> [Quote p v q] -> Price p
    best p qs = fromMaybe p (getBestPrice' qs)

--------------------------------------------------------------------------------
cancelAllLimitOrders
    :: (MonadMoment m, Coin p, Coin v)
    => Event (TradingE p v q c)
    -> m (Event (StrategyAdvice (Action p v)))
cancelAllLimitOrders es =
  let (ep, _, _, _) = splitEvents es
   in return (cancelLimitOrders ep)

-- | Issue cancellation for any limit order seen.
cancelLimitOrders :: (Coin p, Coin v) => Event (OrderPlacement p v) -> Event (StrategyAdvice (Action p v))
cancelLimitOrders ePlaced =
  let getOrd (Placement o) = o
      toAdvice a = Advice ("Canceling placed limit order: " ++ show a ++ "\n", ZipList [a])
   in toAdvice . CancelLimitOrder . getOrderID <$> filterE isLimitOrder (getOrd <$> ePlaced)

--------------------------------------------------------------------------------
-- | Places an order and then cancels it. Detects a cancellation, not necessarily its own.
dumbStrategy :: forall m p v q c.
    (MonadMoment m, Coin p, Coin v)
    => Event (TradingE p v q c)
    -> m (Event (StrategyAdvice (Action p v)))
dumbStrategy es = mdo
  let eAny           = void es
      (eP, eC, _, _) = splitEvents  es
      forceCancel    = cancelLimitOrders eP
      noticeCancel   = const (Advice ("Detected a cancellation!\n", ZipList [])) <$> eC
      strat          = Advice ("Placing an ask!\n", ZipList [NewLimitOrder Ask 99555 0.01 Nothing])

  placeOrder <- once strat eAny
  return $ unionWith const (unionWith (error "Conflict!") placeOrder forceCancel) noticeCancel
    
--------------------------------------------------------------------------------
-- | Copies orderbook

data ActionState p v = 
    ActionState 
        { openActionsMap   :: H.HashMap (OrderSide, Price p) [OpenAction p v]
        , nextCOID         :: OrderID  -- ^ next available "Client Order ID"
        , realizedExposure :: Vol v    -- ^ negative = we are oversold, positive = we are overbought
        } deriving Show

type MarketState p v = State (ActionState p v)

data OpenAction price vol
    = OpenOrder
        { oaVolume    :: Vol vol
        , oaClientOID :: OrderID
        , oaExecdVol  :: Vol vol } deriving Show

emptyState :: forall p v. (Coin p, Coin v) => ActionState p v
emptyState = 
    ActionState
        { openActionsMap = H.empty
        , nextCOID = OID 0 0
        , realizedExposure = Vol (0 :: v)
        }

type Target p v = (OrderSide, Price p, Vol v)

copyBookStrategy :: forall m p v q c. (MonadMoment m, Coin p, Coin v)
    => Event (TradingE p v q c) -> Behavior (ActionState p v) -> m (Event (StrategyAdvice (Action p v), ActionState p v))
copyBookStrategy es bSt = return (eUpdateState `invApply` bSt)
  where
    eUpdateState = runState . updateQuoteBook <$> filterE isQuoteBook es

    isQuoteBook :: TradingE p v q c -> Bool
    isQuoteBook (TB book) = True
    isQuoteBook _         = False

    toBook :: TradingE p v q c -> QuoteBook p v q c
    toBook (TB book) = book
    toBook ev        = error "toBook Error: attempting to convert trading event to QuoteBook."

    updateQuoteBook :: TradingE p v q c -> MarketState p v (StrategyAdvice (Action p v))
    updateQuoteBook ev = do
        exposures <- gets realizedExposure
        combineTargets (getTargets exposures ev)

    getTargets :: Vol v -> TradingE p v q c -> [Target p v] 
    getTargets exposure ev = issueTargets maxExposure exposure (toBook ev)

    combineTargets :: [Target p v] -> MarketState p v (StrategyAdvice (Action p v))
    combineTargets targets = do
        a  <- cleanupOldLevels targets
        as <- mapM addTarget targets
        return (foldr mappend a as)

    -- Cancels ALL open actions on ALL price-levels that are not on the new targets list.
    cleanupOldLevels :: [Target p v] -> MarketState p v (StrategyAdvice (Action p v))
    cleanupOldLevels targets = do
        oldState <- get
        let oldActionsMap = openActionsMap oldState
            newKeysMap    = H.fromList $ (\(s, p, v) -> ((s, p),())) <$> targets

            differenceMap   = H.difference   oldActionsMap newKeysMap
            intersectionMap = H.intersection oldActionsMap newKeysMap

            oldActions     = ZipList . concatMap snd . H.toList $ differenceMap
            removalActions = toCancellation <$> oldActions

            newActionMap = intersectionMap
            newAdvice    = Advice ("Remove old unmatched price-levels: "
                                    <> show (removalActions :: ZipList (Action p v)) 
                                    <> "\n", removalActions)

        put oldState{openActionsMap = newActionMap}
        return newAdvice

    -- subtractVol has to subtract *at least* the amount of volume requested. It MAY cancel more, but it should avoid unnecessary cancellations.
    -- TO DO: This is currently a naïve implementation. It just cancels *all* pending orders.
    subtractVol :: OrderSide -> Price p -> Vol v -> MarketState p v (StrategyAdvice (Action p v))
    subtractVol sd p v = do
        state <- get
        let oldActionsMap  = openActionsMap state
            oldActions     = ZipList $ H.lookupDefault [] (sd,p) oldActionsMap
            removalActions = toCancellation <$> oldActions
            newActionMap   = H.delete (sd, p) oldActionsMap
            newAdvice      = Advice ( "Cancelling all actions to subtract volume: " 
                                      <> show (removalActions :: ZipList (Action p v)) 
                                      <> "\n"
                                    , removalActions)
        put state{openActionsMap = newActionMap}
        return newAdvice
        
    toCancellation :: OpenAction p v -> Action p v
    toCancellation = CancelLimitOrder . oaClientOID

    addTarget :: Target p v -> MarketState p v (StrategyAdvice (Action p v))
    addTarget (sd, p, v) = do
        oldVol <- gets getVol
        case compare v oldVol of
            EQ -> return mempty
            GT -> addVol sd p (v - oldVol) -- either creating new price-level or increasing volume in an existing one
            LT -> do
                subAdv <- subtractVol sd p (oldVol - v)
                oldVol' <- gets getVol
                case compare v oldVol' of
                    LT -> error "addTarget: Could not subtract enough volume to go below or match target:"
                    EQ -> return subAdv
                    GT -> do
                        addAdv <- addVol sd p (v - oldVol')
                        return (subAdv <> addAdv)
      where
        getVol state = getStillOpenVol $ H.lookupDefault [] (sd,p) (openActionsMap state)

    addVol :: OrderSide -> Price p -> Vol v -> MarketState p v (StrategyAdvice (Action p v))
    addVol sd p v = do
        state <- get
        let curOID@(OID hw lw) = nextCOID state
            oldActionsMap = openActionsMap state
            newAction     = NewLimitOrder sd p v (Just curOID)
            newOpenAction = OpenOrder v curOID (Vol 0)
            newState      = 
                state { openActionsMap = H.alter (insertOpenAction newOpenAction) (sd,p) oldActionsMap
                      , nextCOID = OID hw (lw+1)}
        put newState
        return $ Advice ("Placing new order: " <> show newAction  <> "\n", ZipList [newAction])

    insertOpenAction :: OpenAction p v -> Maybe [OpenAction p v] -> Maybe [OpenAction p v]
    insertOpenAction newOpenAction (Just as) = Just $ newOpenAction : as 
    insertOpenAction newOpenAction Nothing   = Just [newOpenAction]

    getStillOpenVol :: [OpenAction p v] -> Vol v
    getStillOpenVol oas =
        let requestedVol = sum $ map oaVolume   oas
            executedVol  = sum $ map oaExecdVol oas
         in requestedVol - executedVol

--------------------------------------------------------------------------------
-- | Updates current exposure and profit/loss

exposureControl :: forall m p v q c. (MonadMoment m, Coin p, Coin v)
    => Event (TradingE p v q c) -> Behavior (ActionState p v) -> m (Event (ActionState p v))
exposureControl es bState = return (eUpdateState `invApply` bState)
  where
    eUpdateState = updateExposure <$> filterE isFill es

    isFill :: TradingE p v q c -> Bool
    isFill (TF _) = True
    isFill _      = False

updateExposure :: forall p v q c. (Coin p, Coin v) => TradingE p v q c -> ActionState p v -> ActionState p v
updateExposure es bState = bState


{-
FIX ME!
There's a synchronization bug on this strategy "as is". When an order fill is detected. The open order state is immediately updated,
but we may still have to re-fill the account balances. If, in this interval, another orderbook event is issued, the
strategy will ask for the amount sold to be put on sale *again* even though we have not yet re-filled it.
To avoid this, we need to somehow keep track of the balances that are pending refills (and take these into consideration 
before asking for more placements).

-}


mirroringStrategy
    :: forall m p v q c. (MonadMoment m, Coin p, Coin v)
    => Event (TradingE p v q c) -> Event (TradingE p v q c) 
    -> m (Event (Maybe (StrategyAdvice (Action p v)), Maybe (StrategyAdvice (Action p v)))) 
mirroringStrategy es1 es2 = mdo
    bState <- stepper emptyState $ 
                unionWith errorSimultaneousUpdate (snd <$> eCopy) $
                unionWith errorSimultaneousUpdate (snd <$> eFill) eExpo
    eCopy  <- copyBookStrategy es1 bState
    eFill  <- refillStrategy   es2 bState
    eExpo  <- exposureControl  es1 bState
    return $ unionWith (\p q ->(fst p, snd q)) (toFst . fst <$> eFill) (toSnd . fst <$> eCopy)
  where
    toFst x = (Just x, Nothing) 
    toSnd x = (Nothing, Just x)
    errorSimultaneousUpdate = error "State updates must not have happenned at the same time."

--------------------------------------------------------------------------------
-- | places orders to refill balances that were depleted from executed orders

refillStrategy :: (Coin p, MonadMoment m) => Event (TradingE p v q c) -> Behavior (ActionState p v) -> m (Event (StrategyAdvice (Action p v), ActionState p v))
refillStrategy es bState = return $ (reFill <$> es) `invApply` bState
  where
    reFill :: Coin p => TradingE p v q c -> ActionState p v -> (StrategyAdvice (Action p v), ActionState p v)
    -- reFill (TF (OrderFilled [Fill _ _ fVol fPrice _ oid])) st = (mempty, st{openActionsMap = H.adjust tail (Bid, fPrice) (openActionsMap st)})
    reFill _ st = (mempty, st)


invApply :: Event (a -> b) -> Behavior a -> Event b 
invApply es b = flip ($) <$> b <@> es 


selfUpdateState
    :: forall m p v q c. (MonadMoment m, Coin p, Coin v)
    => (Event (TradingE p v q c) -> Behavior (ActionState p v) -> m (Event (StrategyAdvice (Action p v), ActionState p v)) )
    ->  Event (TradingE p v q c) -> m (Event (StrategyAdvice (Action p v))) 
selfUpdateState strategy es = mdo
    bState <- stepper emptyState (snd <$> ePair)
    ePair  <- strategy es bState
    return (fst <$> ePair)



-- ------------------------------------------------
-- unions :: [Event (a -> a)] -> Event (a -> a)
-- unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
-- stepper :: MonadMoment m => a -> Event a -> m (Behavior a) 
-- accumE :: MonadMoment m => a -> Event (a -> a) -> m (Event a) 
-- accumB :: MonadMoment m => a -> Event (a -> a) -> m (Behavior a) 
-- mapAccum' :: MonadMoment m => acc -> Event (acc -> (x,acc)) -> m (Event x, Behavior acc)

issueTargets :: (Coin p, Coin v) => Vol v -> Vol v -> QuoteBook p v q c -> [Target p v]
issueTargets (Vol maxExposure) (Vol realizedExposure) = maybe [] (:[]) . fmap (\q -> (side q, price q, min (Vol $ maxExposure - realizedExposure) (volume q) )) . safeHead . bids

maxExposure :: Coin v => Vol v
maxExposure = Vol 3