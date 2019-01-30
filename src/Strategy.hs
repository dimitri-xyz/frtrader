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

import Debug.Trace
-- --------------------------------------------------------------------------------
-- | Copies orderbook

type OpenActionsMap p v = H.HashMap ClientOID (OpenAction p v)

data ActionState p v = 
    ActionState 
        { openActionsMap   :: OpenActionsMap p v
        , nextCOID         :: ClientOID -- ^ next available "Client Order ID"
        , realizedExposure :: Vol v     -- ^ negative = we are overbought, positive = we are oversold, sold too many need to recoup
        } deriving (Show, Eq)

type MarketState p v = State (ActionState p v)

data OpenAction price vol
    = OpenAction
        { oaSide      :: OrderSide
        , oaPrice     :: Price price
        , oaVolume    :: Vol vol
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
    => Vol v -> Event (TradingEv p v q c) -> Behavior (ActionState p v) -> m (Event (StrategyAdvice (Action p v), ActionState p v))
copyBookStrategy maxExposure es bSt = return (eUpdateState `invApply` bSt)
  where
    eUpdateState = runState . updateQuoteBook <$> filterE isQuoteBook es

    isQuoteBook :: TradingEv p v q c -> Bool
    isQuoteBook (BookEv _) = True
    isQuoteBook _         = False

    toBook :: TradingEv p v q c -> QuoteBook p v q c
    toBook (BookEv book) = book
    toBook ev            = error $ "toBook Error: attempting to convert trading event to QuoteBook." 

    updateQuoteBook :: TradingEv p v q c -> MarketState p v (StrategyAdvice (Action p v))
    updateQuoteBook = trackTarget . getAskTarget

    getAskTarget :: TradingEv p v q c -> Target p v
    getAskTarget = maybe (Ask, Price 0, Vol 0) (\q -> (side q, price q, volume q)) . safeHead . asks . toBook 

    -- This procedure is aggressive for price changes, immediately using all available exposure.
    -- It is lazy on volume decreases at the same price when it waits to see cancellation events
    trackTarget :: Target p v -> MarketState p v (StrategyAdvice (Action p v))
    trackTarget target = do
        a  <- cancelOldPriceLevels       target
        a' <- lazyUpdateTargetPriceLevel target
        return (a <> a')

    -- Cancels all open actions on price-levels that do not match the target.
    cancelOldPriceLevels :: Target p v -> MarketState p v (StrategyAdvice (Action p v))
    cancelOldPriceLevels t = trace " Cancelling old levels " $ do
        state <- get 
        let (cancelAction, newActionsMap) = H.traverseWithKey (cancelMismatched t) (openActionsMap state)
            cancelMismatched (sd, p, _) k oa = if sd == oaSide oa && p == oaPrice oa
                then (mempty, oa) -- price-level matches current target, do nothing
                else if oaCancelled oa == True 
                    then (mempty, oa) -- already cancelled
                    else (Advice ("cancelMismatched - Cancelling price-level. ClientOID: " <> show k, ZipList [CancelLimit k]), oa {oaCancelled = True}) 

        put state {openActionsMap = newActionsMap}
        return cancelAction

    subtractAllVolAt :: OrderSide -> Price p -> MarketState p v (StrategyAdvice (Action p v))
    subtractAllVolAt sd p = do
        state <- get
        let (cancelAction, newActionsMap) = H.traverseWithKey (cancelIfMatching sd p) (openActionsMap state)

            cancelIfMatching sd p k oa    = if sd /= oaSide oa || p /= oaPrice oa
                then (mempty, oa) -- price-level does not match, do nothing
                else if oaCancelled oa == True
                    then (mempty, oa) -- already cancelled
                    else (Advice ("cancelMismatched - Cancelling price-level. ClientOID: " <> show k <> "\n", ZipList [CancelLimit k]), oa {oaCancelled = True}) 

        put state {openActionsMap = newActionsMap}
        return cancelAction

    -- This procedure waits for cancellations. Even if we are within the exposure limit, 
    -- When the volume decreases on the same (side, price), this procedure does not 
    -- place new orders until it sees cancellation events (and then waits for a new orderbook).
    -- It "lazily" uses any allowable exposure we currently have.
    -- It is more aggressive for increases in volume immediately placing new orders.
    lazyUpdateTargetPriceLevel :: Target p v -> MarketState p v (StrategyAdvice (Action p v))
    lazyUpdateTargetPriceLevel t@(sd, p, targetVol) = traceShow t $ do
        oldVol <- gets (getOpenVolAtTarget t)
        case traceShow oldVol $ compare targetVol oldVol of
            EQ -> trace " Nothing " $ return mempty
            GT -> trace " Adding! " $ addVol maxExposure sd p (targetVol - oldVol)
            LT -> trace " Subbing " $ subtractVolAt      sd p (oldVol - targetVol)

    getOpenVolAtTarget :: Target p v -> ActionState p v -> Vol v
    getOpenVolAtTarget (sd, p, _) state =
        let matchingActs = H.filter (\oa -> oaSide oa == sd && oaPrice oa == p) (openActionsMap state)
            requestedVol = sum $ fmap oaVolume   matchingActs
            executedVol  = sum $ fmap oaExecdVol matchingActs
         in requestedVol - executedVol

    -- subtractVolAt has to subtract *at least* the amount of volume requested. It MAY cancel more, but it should avoid unnecessary cancellations.
    -- TO DO: This is currently a naïve implementation. It just cancels *all* open orders at given (OrderSide, Price).
    subtractVolAt :: OrderSide -> Price p -> Vol v -> MarketState p v (StrategyAdvice (Action p v))
    subtractVolAt sd p _ = subtractAllVolAt sd p

    -- Add volume, but only at most as maxExposure allows
    -- only outputs `PlaceLimit`s
    addVol :: Vol v -> OrderSide -> Price p -> Vol v -> MarketState p v (StrategyAdvice (Action p v))
    addVol maxExposure sd p v =  do
        state <- get
        let curOID        = nextCOID state
            rExpo         = realizedExposure state
            oExpo         = getTotalOpenVol state
        case compare maxExposure (oExpo + rExpo) of
            LT -> error ("addVol - Maximum exposure " <> show maxExposure <> " is smaller than current exposure: " <> show oExpo <> " + " <> show rExpo)
            EQ -> return mempty
            GT -> do
                let allowedVol    = maxExposure - (oExpo + rExpo)
                    newVol        = min v allowedVol
                    curOID        = nextCOID state

                    newAction     = PlaceLimit sd p newVol (Just curOID)
                    newOpenAction = OpenAction sd p newVol (Vol 0) False

                    oldActionsMap = openActionsMap state

                    newState      = state {openActionsMap = H.insert curOID newOpenAction oldActionsMap, nextCOID = curOID + 1}
                put newState
                return $ Advice ("Placing new order: " <> show newAction  <> "\n", ZipList [newAction])

    getTotalOpenVol :: ActionState p v -> Vol v
    getTotalOpenVol state =
        let requestedVol = sum $ fmap oaVolume   (openActionsMap state)
            executedVol  = sum $ fmap oaExecdVol (openActionsMap state)
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


-- mirroringStrategy
--     :: forall m p v q c. (MonadMoment m, Coin p, Coin v)
--     => Event (TradingEv p v q c) -> Event (TradingEv p v q c) 
--     -> m (Event (Maybe (StrategyAdvice (Action p v)), Maybe (StrategyAdvice (Action p v)))) 
-- mirroringStrategy es1 es2 = mdo
--     bState <- stepper emptyState $ 
--                 unionWith errorSimultaneousUpdate (snd <$> eCopy) $
--                 unionWith errorSimultaneousUpdate (snd <$> eFill) eExpo
--     eCopy  <- copyBookStrategy   es1 bState
--     eFill  <- refillAsksStrategy es2 bState
--     eExpo  <- exposureControl    es1 bState
--     return $ unionWith (\p q ->(fst p, snd q)) (toFst . fst <$> eFill) (toSnd . fst <$> eCopy)
--   where
--     toFst x = (Just x, Nothing) 
--     toSnd x = (Nothing, Just x)
--     errorSimultaneousUpdate = error "State updates must not have happenned at the same time."

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
    refillUpdate (DoneEv   moid) = runState (doneOpenAction moid)
    refillUpdate _               = runState (return mempty)

    -- This procedure and `doneOpenAction` are the only procedures that can delete an OpenAction
    cancelOpenAction :: (Coin p, Coin v) => Maybe ClientOID -> MarketState p v (StrategyAdvice (Action p v))
    cancelOpenAction  Nothing    = error "cancelOpenAction expects all orders to have a valid ClientOID on monitored side"
    cancelOpenAction (Just coid) = do
        state <- get
        put state {openActionsMap = H.delete coid (openActionsMap state)}
        return mempty

    -- This procedure and `cancelOpenAction` are the only procedures that can delete an OpenAction
    doneOpenAction :: (Coin p, Coin v) => Maybe ClientOID -> MarketState p v (StrategyAdvice (Action p v))
    doneOpenAction  Nothing    = error "doneOpenAction expects all orders to have a valid ClientOID on monitored side"
    doneOpenAction (Just coid) = do
        state <- get
        let noUnfilledVol oa = oaVolume oa - oaExecdVol oa == 0
            updater       oa = if noUnfilledVol oa 
                                    then Nothing 
                                    else (error $ "`DoneEv` removed an action that still had unfilled volume. ClientOID:" <> show coid)

        put state {openActionsMap = H.update updater coid (openActionsMap state)}
        return mempty

    refillAsks :: (Coin p, Coin v) => FillEv p v -> MarketState p v (StrategyAdvice (Action p v))
    refillAsks (FillEv _        _          _       Nothing   ) = error "refillAsks expects all orders to have valid ClientOID on monitored side"
    refillAsks (FillEv sd fp@(Price _) fv@(Vol _) (Just coid)) = do
        state <- get
        case H.lookup coid (openActionsMap state) of
            Nothing -> if fv == 0 then return mempty else error ("refillAsks - The impossible happened - Filled unexistent Action " <> show coid)
            Just oa -> do
                let newOrder       = PlaceLimit Bid (oaPrice oa) fv Nothing
                    newMap         = H.adjust (adjuster fv) coid (openActionsMap state)
                    adjuster fv oa = if fv > (oaVolume oa - oaExecdVol oa)
                                        then error ("refillAsks - The impossible happened - Filled more than was still open " <> show coid)
                                        else oa {oaExecdVol = oaExecdVol oa + fv}

                put state {realizedExposure = realizedExposure state + fv, openActionsMap = newMap}
                return $ Advice ("Refill " <> show fv <> " from ClientOID: " <> show coid <> "\n", ZipList [newOrder])

------------------------------------------------
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
