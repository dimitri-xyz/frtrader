{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecursiveDo                #-}

module Strategy where

import           System.IO                (hPutStr, hPutStrLn, stderr)
import           Control.Exception.Base   (finally)
import           Control.Monad            (void)
import           Control.Monad.State 
import           Data.Maybe
import           Data.List.Extended       (safeHead)

import           Reactive.Banana
import           Reactive.Banana.Frameworks.Extended

import           Combinator
import           TradingFramework
import           Interface
import           Market.Types ( Coin(..), StrategyAdvice(..) )

import qualified Data.HashMap.Strict as H

-- --------------------------------------------------------------------------------
-- | Copies orderbook

type OpenActionsMap p v = H.HashMap (OrderSide, Price p) (H.HashMap ClientOID (OpenAction p v))

data ActionState p v = 
    ActionState 
        { openActionsMap   :: OpenActionsMap p v
        , nextCOID         :: ClientOID -- ^ next available "Client Order ID"
        , realizedExposure :: Vol v     -- ^ negative = we are oversold, positive = we are overbought
        } deriving (Show, Eq)

type MarketState p v = State (ActionState p v)

data OpenAction price vol
    = OpenAction
        { oaVolume    :: Vol vol
        , oaExecdVol  :: Vol vol
        , oaCancelled :: Bool -- have we already asked to cancel this order?
        } deriving (Show, Eq)

emptyState :: forall p v. (Coin p, Coin v) => ActionState p v
emptyState = 
    ActionState
        { openActionsMap = H.empty
        , nextCOID = 0
        , realizedExposure = Vol (0 :: v)
        }

type Target p v = (OrderSide, Price p, Vol v)

copyBookStrategy :: forall m p v q c. (MonadMoment m, Coin p, Coin v)
    => Event (TradingEv p v q c) -> Behavior (ActionState p v) -> m (Event (StrategyAdvice (Action p v), ActionState p v))
copyBookStrategy es bSt = return (eUpdateState `invApply` bSt)
  where
    eUpdateState = runState . updateQuoteBook <$> filterE isQuoteBook es

    isQuoteBook :: TradingEv p v q c -> Bool
    isQuoteBook (BookEv _) = True
    isQuoteBook _         = False

    toBook :: TradingEv p v q c -> QuoteBook p v q c
    toBook (BookEv book) = book
    toBook ev            = error $ "toBook Error: attempting to convert trading event to QuoteBook." 

    updateQuoteBook :: TradingEv p v q c -> MarketState p v (StrategyAdvice (Action p v))
    updateQuoteBook ev = do
        exposures <- gets realizedExposure
        combineTargets (getTargets exposures ev)

    getTargets :: Vol v -> TradingEv p v q c -> [Target p v] 
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

            oldActions     = ZipList . fmap fst . concat . fmap H.toList . fmap snd . H.toList $ differenceMap
            removalActions = CancelLimit <$> oldActions

            newActionMap = intersectionMap -- BUG!! FIX ME! I can't immediately delete an action. I don't know if extra fills will execute before the cancellation.
            newAdvice    = Advice ("Remove old unmatched price-levels: "
                                    <> show (removalActions :: ZipList (Action p v)) 
                                    <> "\n", removalActions)

        put oldState{openActionsMap = newActionMap}
        return newAdvice

    -- subtractVol has to subtract *at least* the amount of volume requested. It MAY cancel more, but it should avoid unnecessary cancellations.
    -- TO DO: This is currently a naïve implementation. It just cancels *all* pending orders at given (OrderSide, Price).
    subtractVol :: OrderSide -> Price p -> Vol v -> MarketState p v (StrategyAdvice (Action p v))
    subtractVol sd p v = do
        state <- get
        let oldActionsMap  = openActionsMap state
            oldActions     = ZipList . fmap fst . H.toList $ H.lookupDefault H.empty (sd,p) oldActionsMap
            removalActions = CancelLimit <$> oldActions
            newActionMap   = H.delete (sd, p) oldActionsMap -- BUG!! FIX ME! I can't immediately delete an action. I don't know if extra fills will execute before the cancellation.
            newAdvice      = Advice ( "Cancelling all actions to subtract volume: " 
                                      <> show (removalActions :: ZipList (Action p v)) 
                                      <> "\n"
                                    , removalActions)
        put state{openActionsMap = newActionMap}
        return newAdvice
        
    addTarget :: Target p v -> MarketState p v (StrategyAdvice (Action p v))
    addTarget (sd, p, v) = do
        oldVol <- gets $ getStillOpenVol (sd, p)
        case compare v oldVol of
            EQ -> return mempty
            GT -> addVol sd p (v - oldVol) -- either creating new price-level or increasing volume in an existing one
            LT -> do
                subAdv <- subtractVol sd p (oldVol - v)
                oldVol' <- gets $ getStillOpenVol (sd, p)
                case compare v oldVol' of
                    LT -> error "addTarget: Could not subtract enough volume to go below or match target:"
                    EQ -> return subAdv
                    GT -> do
                        addAdv <- addVol sd p (v - oldVol')
                        return (subAdv <> addAdv)

    addVol :: OrderSide -> Price p -> Vol v -> MarketState p v (StrategyAdvice (Action p v))
    addVol sd p v = do
        state <- get
        let curOID = nextCOID state
            oldActionsMap = openActionsMap state
            newAction     = PlaceLimit sd p v (Just curOID)
            newOpenAction = OpenAction v (Vol 0) False
            newState      = 
                state { openActionsMap = H.alter (insertOpenAction curOID newOpenAction) (sd,p) oldActionsMap
                      , nextCOID = curOID + 1}
        put newState
        return $ Advice ("Placing new order: " <> show newAction  <> "\n", ZipList [newAction])

    insertOpenAction :: ClientOID -> OpenAction p v -> Maybe (H.HashMap ClientOID (OpenAction p v)) -> Maybe (H.HashMap ClientOID (OpenAction p v))
    insertOpenAction oid newOpenAction (Just actionMap) = Just (H.insert    oid newOpenAction actionMap)
    insertOpenAction oid newOpenAction Nothing          = Just (H.singleton oid newOpenAction)

    getStillOpenVol :: Coin p => (OrderSide, Price p) -> ActionState p v -> Vol v
    getStillOpenVol key state =
        let actionsMap   = H.lookupDefault H.empty key (openActionsMap state)
            requestedVol = sum $ fmap oaVolume   actionsMap
            executedVol  = sum $ fmap oaExecdVol actionsMap
         in requestedVol - executedVol

--------------------------------------------------------------------------------
-- | Updates current exposure and profit/loss

exposureControl :: forall m p v q c. (MonadMoment m, Coin p, Coin v)
    => Event (TradingEv p v q c) -> Behavior (ActionState p v) -> m (Event (ActionState p v))
exposureControl es bState = return (eUpdateState `invApply` bState)
  where
    eUpdateState = updateExp <$> filterE isFill es
    updateExp ev = execState (updateAskExposure ev)

    isFill :: TradingEv p v q c -> Bool
    isFill (FillsEv _) = True
    isFill _           = False

updateAskExposure :: forall p v q c. (Coin p, Coin v) => TradingEv p v q c -> MarketState p v ()
updateAskExposure (FillsEv fills) = mapM_ updateAskFill fills
  where
    updateAskFill :: forall p v q c. (Coin p, Coin v) => FillEv p v -> MarketState p v ()
    updateAskFill (FillEv _ _ vol _) = do
        st <- get
        put st {realizedExposure = realizedExposure st - vol}
        return ()

-- FIX FIX ME! Write/modify tests to ensure this now works before removing the comment.
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
    => Event (TradingEv p v q c) -> Event (TradingEv p v q c) 
    -> m (Event (Maybe (StrategyAdvice (Action p v)), Maybe (StrategyAdvice (Action p v)))) 
mirroringStrategy es1 es2 = mdo
    bState <- stepper emptyState $ 
                unionWith errorSimultaneousUpdate (snd <$> eCopy) $
                unionWith errorSimultaneousUpdate (snd <$> eFill) eExpo
    eCopy  <- copyBookStrategy   es1 bState
    eFill  <- refillAsksStrategy es2 bState
    eExpo  <- exposureControl    es1 bState
    return $ unionWith (\p q ->(fst p, snd q)) (toFst . fst <$> eFill) (toSnd . fst <$> eCopy)
  where
    toFst x = (Just x, Nothing) 
    toSnd x = (Nothing, Just x)
    errorSimultaneousUpdate = error "State updates must not have happenned at the same time."

--------------------------------------------------------------------------------
-- | places orders to refill balances that were depleted from executed orders
-- This only refills orders for which it can find a corresponding OpenAction (i.e. matching ClientOID) in the state.
-- Orders placed on destination exchange (by a real person through the web or a different bot) will not be refilled.
refillAsksStrategy
    :: (Coin p, Coin v, MonadMoment m) 
    => Event (TradingEv p v q c) -> Behavior (ActionState p v)
    -> m (Event (StrategyAdvice (Action p v), ActionState p v))
refillAsksStrategy es bState = return $ (refillUpdate <$> es) `invApply` bState
  where
    refillUpdate :: (Coin p, Coin v) => TradingEv p v q c -> ActionState p v -> (StrategyAdvice (Action p v), ActionState p v)
    refillUpdate (FillsEv fills) = runState (foldr (<>) mempty <$> mapM refillAsks fills)
    refillUpdate (CancelEv moid) = runState (cancelOpenAction moid)
    refillUpdate _               = runState (return mempty)

    -- Assumes there will be only one OpenAction with a given ClientOID in the whole market
    cancelOpenAction :: (Coin p, Coin v) => Maybe ClientOID -> MarketState p v (StrategyAdvice (Action p v))
    cancelOpenAction  Nothing    = error "cancelOpenAction expects all orders to have a valid ClientOID on monitored side"
    cancelOpenAction (Just coid) = do
        state <- get
        let newMap          = cleanupOuterMap (H.delete coid <$> openActionsMap state)
            cleanupOuterMap = H.mapMaybe (\hm -> if null hm then Nothing else Just hm)
        put state {openActionsMap = newMap}
        return mempty

    refillAsks :: (Coin p, Coin v) => FillEv p v -> MarketState p v (StrategyAdvice (Action p v))
    refillAsks (FillEv _        _          _       Nothing   ) = error "refillAsks expects all orders to have valid ClientOID on monitored side"
    refillAsks (FillEv sd fp@(Price _) fv@(Vol _) (Just coid)) = do
        state <- get
        case H.lookup (Ask, fp) (openActionsMap state) of
            Nothing       -> return mempty
            Just innerMap -> case H.lookup coid innerMap of
                Nothing -> return mempty
                Just oa -> do
                    let newOrder = PlaceLimit Bid fp fv Nothing
                        oa' = oa {oaExecdVol = oaExecdVol oa + fv}

                        innerUpdater = case compare fv (oaVolume oa - oaExecdVol oa) of
                            GT -> error "WHAAAAATTT!!!! The impossible happened - Filled more than was still open" -- FIX ME!
                            EQ -> const Nothing     -- exact match, remove this entry
                            LT -> const (Just oa')
                    
                        innerMap' = H.update innerUpdater coid innerMap

                        outerUpdater = if null innerMap' then const Nothing else const (Just innerMap')
                        outerMap     = H.update outerUpdater (Ask, fp) (openActionsMap state) 

                    put state {realizedExposure = realizedExposure state + fv, openActionsMap = outerMap}
                    return $ Advice ("Refill " <> show fv <> " from ClientOID: " <> show coid <> "\n", ZipList [newOrder])


invApply :: Event (a -> b) -> Behavior a -> Event b 
invApply es b = flip ($) <$> b <@> es 


selfUpdateState
    :: forall m p v q c. (MonadMoment m, Coin p, Coin v)
    => (Event (TradingEv p v q c) -> Behavior (ActionState p v) -> m (Event (StrategyAdvice (Action p v), ActionState p v)) )
    -> ActionState p v
    -> Event (TradingEv p v q c) 
    -> m (Event (StrategyAdvice (Action p v), ActionState p v))
selfUpdateState strategy initialState es = mdo
    bState <- stepper initialState (snd <$> ePair)
    ePair  <- strategy es bState
    return ePair


-- ------------------------------------------------
-- unions :: [Event (a -> a)] -> Event (a -> a)
-- unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
-- stepper :: MonadMoment m => a -> Event a -> m (Behavior a) 
-- accumE :: MonadMoment m => a -> Event (a -> a) -> m (Event a) 
-- accumB :: MonadMoment m => a -> Event (a -> a) -> m (Behavior a) 
-- mapAccum' :: MonadMoment m => acc -> Event (acc -> (x,acc)) -> m (Event x, Behavior acc)

issueTargets :: (Coin p, Coin v) => Vol v -> Vol v -> QuoteBook p v q c -> [Target p v]
issueTargets (Vol maxExposure) (Vol realizedExposure) = maybe [] (:[]) . fmap (\q -> (side q, price q, min (Vol $ maxExposure - realizedExposure) (volume q) )) . safeHead . asks

maxExposure :: Coin v => Vol v
maxExposure = Vol 3