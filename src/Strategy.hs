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
        }

data OpenAction price vol
    = OpenOrder
        { oaVolume    :: Vol vol
        , oaClientOID :: OrderID
        , oaExecdVol  :: Vol vol }

emptyState = ActionState {openActionsMap = H.empty, nextCOID = OID 0 0}

type Target p v = (OrderSide, Price p, Vol v)

copyBookStrategy
    :: forall m p v q c. (MonadMoment m, Coin p, Coin v)
    => Event (TradingE p v q c) -> m (Event (StrategyAdvice (Action p v)))
copyBookStrategy es = mdo
    (eNewAdvice, _) <- mapAccum emptyState (updateQuoteBook <$> filterE isQuoteBook es)
    return eNewAdvice
  where
    updateQuoteBook :: TradingE p v q c -> ActionState p v -> (StrategyAdvice (Action p v), ActionState p v)
    updateQuoteBook e state = combineTargets (getTargets e) state

    insertOpenAction :: OpenAction p v -> [OpenAction p v] -> [OpenAction p v]
    insertOpenAction newOpenAction = (newOpenAction :) 

    getTargets :: TradingE p v q c -> [Target p v] 
    getTargets e = catMaybes $ fmap ($ toBook e) regions 

    getVol :: [OpenAction p v] -> Vol v
    getVol oas = let requestedVol = sum $ map oaVolume   oas
                     executedVol  = sum $ map oaExecdVol oas
                  in requestedVol - executedVol

    toBook :: TradingE p v q c -> QuoteBook p v q c
    toBook (TB book) = book
    toBook ev        = error ("toBook Error: attempting to convert trading event to QuoteBook.")

    isQuoteBook :: TradingE p v q c -> Bool
    isQuoteBook (TB book) = True
    isQuoteBook _         = False

    combineTargets :: [Target p v] -> ActionState p v -> (StrategyAdvice (Action p v), ActionState p v)
    combineTargets targets oldState = 
        let (newAdvice, newState) = foldr addTarget' (mempty, oldState) targets
            addTarget' t st = addTarget t (snd st) -- addTarget is independent of other current Advice
            cleanupAdvice = removeOldLevels oldState newState
         in (newAdvice <> cleanupAdvice, newState)
    
    removeOldLevels :: ActionState p v -> ActionState p v -> StrategyAdvice (Action p v)
    removeOldLevels (ActionState{openActionsMap = old})(ActionState{openActionsMap = new}) =
        let oldLevels = ZipList $ concat $ fmap snd $ H.toList (H.difference old new)
            toCancellation = CancelLimitOrder . oaClientOID
            removalActions = fmap toCancellation oldLevels
         in Advice ("Remove old unmatched price-levels: " <> show (removalActions :: ZipList (Action p v)) , removalActions) 

    addTarget :: Target p v -> ActionState p v -> (StrategyAdvice (Action p v), ActionState p v)
    addTarget (s, p, v) oldState
        = case compare v oldVol of
            EQ -> (mempty, oldState)
            GT -> addVol Bid p (v - oldVol) oldState -- either creating new price-level or increasing volume in an existing one
            LT -> 
                let (subAdv, oldState') = subtractVol Bid p (oldVol - v)  oldState
                    (addAdv, newState ) = addVol      Bid p (v - oldVol') oldState'
                    oldVol' = getVol $ H.lookupDefault [] (s,p) (openActionsMap oldState')
                 in case compare v oldVol' of
                        EQ -> (subAdv, oldState')
                        GT -> (subAdv <> addAdv, newState)
                        LT -> error $ "addTarget: Could not subtract enough volume to go below or match target:"
                        
      where
        oldVol  = getVol $ H.lookupDefault [] (s,p) (openActionsMap oldState)

    addVol :: OrderSide -> Price p -> Vol v -> ActionState p v -> (StrategyAdvice (Action p v), ActionState p v)
    addVol sd p v oldState@(ActionState{openActionsMap = actionsMap, nextCOID = curOID@(OID hw lw)}) 
        = (Advice ("Placing new order: " <> show newAction, ZipList [newAction]), newState)
      where
        newState = ActionState {openActionsMap = H.adjust (insertOpenAction newOpenAction) (sd,p) actionsMap, nextCOID = OID hw (lw+1)}
        newAction     = NewLimitOrder sd p v (Just curOID)
        newOpenAction = OpenOrder v curOID (Vol 0)

    -- FIX ME! "DRY" violation with `removeOldLevels`
    subtractVol :: OrderSide -> Price p -> Vol v -> ActionState p v -> (StrategyAdvice (Action p v), ActionState p v)
    subtractVol sd p v oldState@(ActionState{openActionsMap = actionsMap, nextCOID = curOID@(OID hw lw)}) 
        = (newAdvice, newState)
      where
        oldActions = ZipList $ H.lookupDefault [] (sd,p) actionsMap
        removalActions = fmap toCancellation oldActions
        toCancellation = CancelLimitOrder . oaClientOID
        newAdvice = Advice ("Cancelling actions to subtract volume: " <> show (removalActions :: ZipList (Action p v)) , removalActions)

        newState = oldState {openActionsMap = H.delete (sd, p) actionsMap}

regions :: [QuoteBook p v q c -> Maybe (Target p v)]
regions = []

