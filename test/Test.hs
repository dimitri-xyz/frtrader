{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks.Extended
import Pipes.Concurrent

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent.STM

import TradingFramework
import Strategy
import Market.Types

import qualified Data.HashMap.Strict as H

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ tests (undefined :: Price BTC) (undefined :: Vol ETH)
--------------------------------------------------------------------------------

tests :: forall p v q c. (Coin p, Coin v) => Price p -> Vol v -> TestTree
tests _ _ = testGroup " Trading Strategy Tests"
    [ testCase "cancelAllLimits" $ do
        outputEvents <- interpretFrameworks cancelAllLimitOrders (Just <$> (cancelInEs :: [TradingE p v q c]))  
        assertEqual "Output list does not match" cancelExpectedAs outputEvents

    , testCase "copyBookStrategy" $ do
        outputPairs <- interpretFrameworks (selfUpdateState copyBookStrategy emptyState) (copyInEs :: [Maybe(TradingE p v q c)])  
        let outputActions = fmap fst <$> outputPairs
        assertEqual "Output list does not match" copyExpectedAs (fmap removeReasoning <$> outputActions)

    , testCase "refillAsksStrategy" $ do
        outputPairs <- interpretFrameworks (selfUpdateState refillAsksStrategy refillInitialState) (refillInEs :: [Maybe(TradingE p v q c)])
        let outputActions = fmap fst <$> outputPairs
            outputStates  = fmap snd <$> outputPairs
        assertEqual "Output list does not match"        refillExpectedAs (fmap removeReasoning <$> outputActions)
        assertEqual "Final output state does not match" refillFinalState (last outputStates)

    , testCase "mirroringStrategy" $ do
        outputEvents <- interpretFrameworks (uncurry mirroringStrategy . split) (binaryIns :: [Maybe (Either (TradingE p v q c) (TradingE p v q c))])  
        assertEqual "Output list does not match" binaryExpectedAs (fmap (fmap (fmap removeReasoning)) <$> outputEvents)

    ]

removeReasoning :: StrategyAdvice a -> StrategyAdvice a
removeReasoning (Advice (r, a)) = Advice ("", a)

--------------------------------------------------------------------------------
limOrder :: Order p v (Confirmation p v)
limOrder = LimitOrder
  { oSide          = undefined
  , limitPrice     = undefined
  , limitVolume    = undefined
  , aConfirmation  = confirm
  }

confirm :: Confirmation p v
confirm = Conf
  { orderID      = OID 333 444
  , mTimestamp   = undefined
  , mExecuted    = undefined
  , mOrderStatus = undefined
  }

cancelInEs = 
    [ TF (OrderFilled [])
    , TP (Placement limOrder)
    , TB (QuoteBook {})
    , TC (Cancellation {toOID = OID {hw = 333, lw = 444}})
    , TP (Placement (MarketOrder {}))
    ]

reasonMessage = "Canceling placed limit order: CancelLimitOrder {acClientOID = OID {hw = 333, lw = 444}}\n"
cancelExpectedAs = [Nothing, Just (Advice (reasonMessage, ZipList [CancelLimitOrder {acClientOID = OID 333 444}])), Nothing, Nothing, Nothing]

--------------------------------------------------------------------------------
-- FIX ME! Compiler requires this type signature. Why? Monomorphism?
qb1, qb2, qb3, qb4, qa1 :: forall p v q. (Coin p, Coin v) => Quote p v q 

qb1 = Quote Bid (Price 2000) (Vol 1) undefined
qb2 = Quote Bid (Price 1000) (Vol 1) undefined
qb3 = Quote Bid (Price 2000) (Vol 3) undefined
qb4 = Quote Bid (Price 1500) (Vol 1) undefined

qa1 = Quote Ask (Price 9000) (Vol 1) undefined


-- FIX ME! Compiler requires this type signature. Why? Monomorphism?
bk1, bk2, bk3, bk4 :: forall p v q c. (Coin p, Coin v) => QuoteBook p v q c

bk1 = QuoteBook {bids = [qb1],     asks = [qa1], counter = undefined}
bk2 = QuoteBook {bids = [qb1,qb2], asks = [],    counter = undefined}
bk3 = QuoteBook {bids = [qb3,qb2], asks = [qa1], counter = undefined}
bk4 = QuoteBook {bids = [qb4,qb2], asks = [],    counter = undefined}

-- FIX ME! Compiler requires this type signature. Why? Monomorphism?
copyInEs :: forall p v q c. (Coin p, Coin v) => [Maybe (TradingE p v q c)]
copyInEs = 
    [ Nothing
    , Just $ TF (OrderFilled [])
    , Just $ TB bk1
    , Just $ TP (Placement limOrder)
    , Just $ TB bk2
    , Nothing
    , Nothing
    , Just $ TC (Cancellation {toOID = OID {hw = 333, lw = 444}})
    , Just $ TB bk3
    , Just $ TP (Placement (MarketOrder {}))
    , Just $ TB bk4
    ]

-- FIX ME! Compiler requires this type signature. Why? Monomorphism?
copyExpectedAs :: forall p v. (Coin p, Coin v) => [Maybe (StrategyAdvice (Action p v))]
copyExpectedAs =
    [ Nothing
    , Nothing
    , Just $ Advice ("", ZipList [NewLimitOrder Bid (Price 2000) (Vol 1) (Just(OID 0 0))])
    , Nothing
    , Just $ Advice ("", ZipList [])
    , Nothing
    , Nothing
    , Nothing
    , Just $ Advice ("", ZipList [NewLimitOrder Bid (Price 2000) (Vol 2) (Just(OID 0 1))])
    , Nothing
    , Just $ Advice ("", ZipList [NewLimitOrder Bid (Price 1500) (Vol 1) (Just(OID 0 2)), CancelLimitOrder (OID 0 0), CancelLimitOrder (OID 0 1)])
    ]

--------------------------------------------------------------------------------

refillInEs :: forall p v q c. (Coin p, Coin v) => [Maybe (TradingE p v q c)]
refillInEs = 
    [ Nothing
    , Just $ TB bk2
    , Just $ TF (OrderFilled [Fill 0 Nothing (Vol 1) (Price 1000) (Cost 0.02) (OID 0 0)])
    , Just $ TP (Placement limOrder)
    , Just $ TC (Cancellation {toOID = OID 0 7})
    , Nothing
    , Just $ TF (OrderFilled [Fill 0 Nothing (Vol 1) (Price 1000) (Cost 0.02) (OID 0 2)]) -- unknown ClientOID, thus ignored
    , Just $ TC (Cancellation {toOID = OID {hw = 333, lw = 444}})
    , Just $ TF (OrderFilled [ Fill 1 Nothing (Vol 2) (Price 1500) (Cost 0)    (OID 0 1)
                             , Fill 2 Nothing (Vol 3) (Price 1000) (Cost 0.01) (OID 0 0)])
    , Just $ TC (Cancellation {toOID = OID 0 9})
    ]

refillExpectedAs :: forall p v. (Coin p, Coin v) => [Maybe (StrategyAdvice (Action p v))]
refillExpectedAs =
    [ Nothing
    , Just mempty
    , Just $ Advice ("", ZipList [NewLimitOrder Bid (Price 1000) (Vol 1) (Just $ OID 0 0)])
    , Just mempty
    , Just mempty
    , Nothing
    , Just mempty
    , Just mempty
    , Just $ Advice ("", ZipList [ NewLimitOrder Bid (Price 1500) (Vol 2) (Just $ OID 0 0)
                                 , NewLimitOrder Bid (Price 1000) (Vol 3) (Just $ OID 0 0)])
    , Just mempty
    ]

refillInitialState :: forall p v. (Coin p, Coin v) => ActionState p v
refillInitialState = 
    ActionState
        { openActionsMap = H.fromList 
            [ ((Ask, Price 1000), H.singleton (OID 0 0) (OpenAction {oaVolume = Vol 4, oaCancelled = False, oaExecdVol  = Vol 0}) )
            , ((Ask, Price 1500), H.singleton (OID 0 1) (OpenAction {oaVolume = Vol 5, oaCancelled = False, oaExecdVol  = Vol 1}) )
            , ((Ask, Price 3000), H.fromList[ (OID 0 8,  OpenAction {oaVolume = Vol 5, oaCancelled = False, oaExecdVol  = Vol 1})
                                            , (OID 0 9,  OpenAction {oaVolume = Vol 7, oaCancelled = False, oaExecdVol  = Vol 5})])
            , ((Ask, Price 5000), H.singleton (OID 0 7) (OpenAction {oaVolume = Vol 2, oaCancelled = False, oaExecdVol  = Vol 1}) )
            ]
        , nextCOID = OID 0 10
        , realizedExposure = Vol (0 :: v)
        }

refillFinalState :: forall p v. (Coin p, Coin v) => Maybe (ActionState p v)
refillFinalState = Just $
    ActionState
        { openActionsMap = H.fromList 
            [ ((Ask, Price 1500), H.singleton (OID 0 1) (OpenAction {oaVolume = Vol 5, oaCancelled = False, oaExecdVol  = Vol 3}) )
            , ((Ask, Price 3000), H.singleton (OID 0 8) (OpenAction {oaVolume = Vol 5, oaCancelled = False, oaExecdVol  = Vol 1}) )]
        , nextCOID = OID 0 10
        , realizedExposure = Vol (6 :: v)
        }

--------------------------------------------------------------------------------
binaryIns :: forall p v q c. (Coin p, Coin v) => [Maybe (Either (TradingE p v q c) (TradingE p v q c))]
binaryIns =
    [ Nothing
    , Just $ Left $ TF (OrderFilled [])
    , Just $ Left $ TB bk1
    , Just $ Left $ TP (Placement limOrder)
    , Just $ Left $ TB bk2
    , Nothing
    , Just    $ Right $ TF (OrderFilled [Fill 0 Nothing (Vol 0.2) (Price 2000) (Cost 0.4) (OID 0 0)])
    , Just $ Left $ TC (Cancellation {toOID = OID {hw = 333, lw = 444}})
    , Just $ Left $ TB bk3
    , Just $ Left $ TP (Placement (MarketOrder {}))
    , Just $ Left $ TB bk4
    ]


binaryExpectedAs :: forall p v. (Coin p, Coin v) => [ Maybe ( Maybe (StrategyAdvice (Action p v)), Maybe (StrategyAdvice (Action p v)) )]
binaryExpectedAs = -- fmap (\s -> (Nothing, Just s)) <$> copyExpectedAs
    [ Nothing
    , Nothing
    , Just $ (Nothing, Just $ Advice ("", ZipList [NewLimitOrder Bid (Price 2000) (Vol 1) (Just(OID 0 0))]))
    , Nothing
    , Just $ (Nothing, Just $ Advice ("", ZipList []))
    , Nothing
    , Just $ (Just (Advice ("",ZipList {getZipList = []})),Nothing) --
    , Nothing
    , Just $ (Nothing, Just $ Advice ("", ZipList [NewLimitOrder Bid (Price 2000) (Vol 2) (Just(OID 0 1))]))
    , Nothing
    , Just $ (Nothing, Just $ Advice ("", ZipList [NewLimitOrder Bid (Price 1500) (Vol 1) (Just(OID 0 2)), CancelLimitOrder (OID 0 0), CancelLimitOrder (OID 0 1)]))
    ]

--------------------------------------------------------------------------------
