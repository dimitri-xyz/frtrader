{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Pipes.Concurrent

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent.STM

import TradingFramework
import Market.Types

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ tests (undefined :: Price BTC) (undefined :: Vol ETH)
--------------------------------------------------------------------------------

tests :: forall p v. (Coin p, Coin v) => Price p -> Vol v -> TestTree
tests _ _ = testGroup " Trading Strategy Tests"
  [ testCase "Compare output actions"
        (compareOutputTest cancelAllLimitOrders
          -- input events
         [ TF (OrderFilled [])
         , TP (Placement limOrder :: OrderPlacement p v)
         , TB (QuoteBook {})
         , TC (Cancellation {toOID = OID {hw = 333, lw = 444}})
         , TP (Placement (MarketOrder {}))]
          -- expected output
         [ Advice (reasonMessage, ZipList [CancelLimitOrder {acClientOID = OID 333 444}]) ]
        )

  ]

--------------------------------------------------------------------------------
reasonMessage = "Canceling placed limit order: CancelLimitOrder {acClientOID = OID {hw = 333, lw = 444}}\n"

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
        eAdvs   <- accumIntoListEvents eAdvice
        logLastEventInTVar tv eAdvs

    activate network

    sequence_ $ fmap fireInput inputs
    outputEvents <- readTVarIO tv
    assertEqual "Output list does not match" expecteds (reverse outputEvents)

  where

    logLastEventInTVar :: TVar a -> Event a -> MomentIO ()
    logLastEventInTVar tv e = reactimate . fmap (atomically . writeTVar tv) $ e

    -- cons successive events onto the head of a list
    accumIntoListEvents :: MonadMoment m => Event a -> m (Event [a])
    accumIntoListEvents event = accumE [] (fmap (:) event)
