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
        outputEvents <- interpretFrameworks (selfUpdateState copyBookStrategy) (copyInEs :: [Maybe(TradingE p v q c)])  
        assertEqual "Output list does not match" copyExpectedAs (fmap removeReasoning <$> outputEvents)

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
    , Just $ Advice ("", ZipList [NewLimitOrder Bid (Price 1500) (Vol 1) (Just(OID 0 2)), CancelLimitOrder (OID 0 1), CancelLimitOrder (OID 0 0)])
    ]

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
    , Just $ (Nothing, Just $ Advice ("", ZipList [NewLimitOrder Bid (Price 1500) (Vol 1) (Just(OID 0 2)), CancelLimitOrder (OID 0 1), CancelLimitOrder (OID 0 0)]))
    ]

--------------------------------------------------------------------------------
