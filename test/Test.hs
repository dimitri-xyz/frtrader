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
  [ testCase "Compare output cancelAllLimits" (compareOutputTest cancelAllLimitOrders (cancelInEs :: [TradingE p v q c]) cancelExpectedAs)
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
