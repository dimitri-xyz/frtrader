{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}


module Strategy where

import           System.IO                (hPutStr, hPutStrLn, stderr)
import           Control.Exception.Base   (finally)
import           Control.Monad            (void)
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
        { openActionsMap :: H.HashMap (OrderSide, Price p) [OpenAction p v]
        , nextCOID   :: OrderID -- next available "Client Order ID"
        } deriving Show

data OpenAction price vol
    = OpenOrder
        { oaVolume    :: Vol vol
        , oaClientOID :: OrderID
        , oaExecdVol  :: Vol vol } deriving Show

emptyState = ActionState {openActionsMap = H.empty, nextCOID = OID 0 0}

type Target p v = (OrderSide, Price p, Vol v)

copyBookStrategy :: forall m p v q c. (MonadMoment m, Coin p, Coin v)
    => Event (TradingE p v q c) -> Behavior (ActionState p v) -> m (Event (StrategyAdvice (Action p v), ActionState p v))
copyBookStrategy es bSt = return $ (updateQuoteBook <$> (filterE isQuoteBook es)) `invApply` bSt
  where
    isQuoteBook :: TradingE p v q c -> Bool
    isQuoteBook (TB book) = True
    isQuoteBook _         = False

    toBook :: TradingE p v q c -> QuoteBook p v q c
    toBook (TB book) = book
    toBook ev        = error ("toBook Error: attempting to convert trading event to QuoteBook.")

    updateQuoteBook :: TradingE p v q c -> ActionState p v -> (StrategyAdvice (Action p v), ActionState p v)
    updateQuoteBook e state = combineTargets (getTargets e) state

    getTargets :: TradingE p v q c -> [Target p v] 
    getTargets e = catMaybes $ fmap ($ toBook e) regions 

    combineTargets :: [Target p v] -> ActionState p v -> (StrategyAdvice (Action p v), ActionState p v)
    combineTargets targets oldState = 
        let (advice', state') = cleanupOldLevels targets oldState
            -- addTarget is independent of other current Advice, depends only on state
            addTarget' t (as, st) = let (as', st') = addTarget t st in (as <> as', st') 

         in foldr addTarget' (advice', state') targets
    
    -- Cancels ALL open actions on ALL price-levels that are not on the new targets list.
    cleanupOldLevels :: [Target p v] -> ActionState p v -> (StrategyAdvice (Action p v), ActionState p v)
    cleanupOldLevels targets oldState@(ActionState {openActionsMap = oldActionsMap}) = 
        (newAdvice, oldState {openActionsMap = newActionMap})
      where
        newKeysMap = H.fromList $ (\(s, p, v) -> ((s, p),())) <$> targets

        differenceMap   = H.difference   oldActionsMap newKeysMap
        intersectionMap = H.intersection oldActionsMap newKeysMap

        oldActions     = ZipList . concat . fmap snd . H.toList $ differenceMap
        removalActions = toCancellation <$> oldActions

        newAdvice    = Advice ("Remove old unmatched price-levels: " <> show (removalActions :: ZipList (Action p v)) <> "\n", removalActions)
        newActionMap = intersectionMap

    -- subtractVol has to subtract *at least* the amount of volume requested. It MAY cancel more, but it should avoid unnecessary cancellations.
    -- TO DO: This is currently a naïve implementation. It just cancels *all* pending orders.
    subtractVol :: OrderSide -> Price p -> Vol v -> ActionState p v -> (StrategyAdvice (Action p v), ActionState p v)
    subtractVol sd p v oldState@(ActionState{openActionsMap = oldActionsMap}) =
        (newAdvice, oldState {openActionsMap = newActionMap})
      where
        oldActions     = ZipList $ H.lookupDefault [] (sd,p) oldActionsMap
        removalActions = toCancellation <$> oldActions

        newAdvice    = Advice ("Cancelling all actions to subtract volume: " <> show (removalActions :: ZipList (Action p v)) <> "\n", removalActions)
        newActionMap = H.delete (sd, p) oldActionsMap


    toCancellation :: OpenAction p v -> Action p v
    toCancellation = CancelLimitOrder . oaClientOID


    addTarget :: Target p v -> ActionState p v -> (StrategyAdvice (Action p v), ActionState p v)
    addTarget (s, p, v) oldState
        = case compare v oldVol of
            EQ -> (mempty, oldState)
            GT -> addVol Bid p (v - oldVol) oldState -- either creating new price-level or increasing volume in an existing one
            LT -> 
                let (subAdv, oldState') = subtractVol Bid p (oldVol - v)  oldState
                    (addAdv, newState ) = addVol      Bid p (v - oldVol') oldState'
                    oldVol' = getStillOpenVol $ H.lookupDefault [] (s,p) (openActionsMap oldState')
                 in case compare v oldVol' of
                        EQ -> (subAdv, oldState')
                        GT -> (subAdv <> addAdv, newState)
                        LT -> error $ "addTarget: Could not subtract enough volume to go below or match target:"
                        
      where
        oldVol  = getStillOpenVol $ H.lookupDefault [] (s,p) (openActionsMap oldState)

    addVol :: OrderSide -> Price p -> Vol v -> ActionState p v -> (StrategyAdvice (Action p v), ActionState p v)
    addVol sd p v oldState@(ActionState{openActionsMap = actionsMap, nextCOID = curOID@(OID hw lw)}) 
        = (Advice ("Placing new order: " <> show newAction  <> "\n", ZipList [newAction]), newState)
      where
        newState = ActionState {openActionsMap = H.alter (insertOpenAction newOpenAction) (sd,p) actionsMap, nextCOID = OID hw (lw+1)}
        newAction     = NewLimitOrder sd p v (Just curOID)
        newOpenAction = OpenOrder v curOID (Vol 0)

    insertOpenAction :: OpenAction p v -> Maybe [OpenAction p v] -> Maybe [OpenAction p v]
    insertOpenAction newOpenAction (Just as) = Just $ newOpenAction : as 
    insertOpenAction newOpenAction Nothing   = Just [newOpenAction]

    getStillOpenVol :: [OpenAction p v] -> Vol v
    getStillOpenVol oas =
        let requestedVol = sum $ map oaVolume   oas
            executedVol  = sum $ map oaExecdVol oas
         in requestedVol - executedVol



regions :: [QuoteBook p v q c -> Maybe (Target p v)]
regions = [fmap (\q -> (side q, price q, volume q)) . safeHead . bids]




mirroringStrategy
    :: forall m p v q c. (MonadMoment m, Coin p, Coin v)
    => Event (TradingE p v q c) -> Event (TradingE p v q c) 
    -> m (Event (Maybe (StrategyAdvice (Action p v)), Maybe (StrategyAdvice (Action p v)))) 
mirroringStrategy es1 es2 = mdo
    bSt <- stepper emptyState $ unionWith (error "State update must not have happenned at the same time.") (snd <$> eCopy) (snd <$> eFill)
    eCopy <- copyBookStrategy es1 bSt
    eFill <- refillStrategy   es2 bSt
    return $ unionWith (\p q ->(fst p, snd q)) (toFst . fst <$> eFill) (toSnd . fst <$> eCopy)
  where
    toFst x = (Just x, Nothing) 
    toSnd x = (Nothing, Just x)


refillStrategy :: MonadMoment m => Event (TradingE p v q c) -> Behavior (ActionState p v) -> m (Event (StrategyAdvice (Action p v), ActionState p v))
refillStrategy _ _ = return never


invApply :: Event (a -> b) -> Behavior a -> Event b 
invApply es b = (flip ($)) <$> b <@> es 


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
