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
  [ testCase "cancelAllLimits"  (compareOutputTest cancelAllLimitOrders (cancelInEs :: [TradingE p v q c]) cancelExpectedAs)
  , testCase "copyBookStrategy" (compareOutputTest copyBookStrategy     (copyInEs   :: [TradingE p v q c]) copyExpectedAs)
  ]

--------------------------------------------------------------------------------
compareOutputTest
    :: forall p v q c output. (Eq output, Show output)
    => ( Event (TradingE p v q c) -> MomentIO (Event output) ) 
    -> [TradingE p v q c]
    -> [output]
    -> IO ()

compareOutputTest networkDescription inputs expecteds = do
    (inputHandlers, fireInput) <- newHandlerSet
    -- create TVAR to hold final list of output events
    tv <- newTVarIO []

    network <- compile $ mdo
        es <- fromHandlerSet inputHandlers
        eAdvice <- networkDescription es 
        logEventsInTVar tv eAdvice

    activate network
    sequence_ $ fmap fireInput inputs
    outputEvents <- readTVarIO tv
    assertEqual "Output list does not match" expecteds (reverse outputEvents)

  where
    -- cons successive events onto the head of a list
    logEventsInTVar :: TVar [a] -> Event a -> MomentIO ()
    logEventsInTVar tv e = reactimate . fmap (atomically . modifyTVar tv . (:) ) $ e

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
cancelExpectedAs = [ Advice (reasonMessage, ZipList [CancelLimitOrder {acClientOID = OID 333 444}]) ]

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
copyInEs :: forall p v q c. (Coin p, Coin v) => [TradingE p v q c]
copyInEs = 
    [ TF (OrderFilled [])
    , TB bk1
    , TP (Placement limOrder)
    , TB bk2
    , TC (Cancellation {toOID = OID {hw = 333, lw = 444}})
    , TB bk3
    , TP (Placement (MarketOrder {}))
    , TB bk4
    ]

-- FIX ME! Compiler requires this type signature. Why? Monomorphism?
copyExpectedAs :: forall p v. (Coin p, Coin v) => [StrategyAdvice (Action p v)]
copyExpectedAs =
    [ Advice ("", ZipList [NewLimitOrder Bid (Price 2000) (Vol 1) (Just(OID 0 0))])
    , Advice ("", ZipList [])
      -- non-optimized: cancels and replaces with new order a volume increase on same price level
    , Advice ("", ZipList [CancelLimitOrder (OID 0 0), NewLimitOrder Bid (Price 2000) (Vol 3) (Just(OID 0 1))])
    , Advice ("", ZipList [CancelLimitOrder (OID 0 1), NewLimitOrder Bid (Price 1500) (Vol 1) (Just(OID 0 2))])
    ]

--------------------------------------------------------------------------------
