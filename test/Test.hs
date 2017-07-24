{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Pipes.Concurrent

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent.STM
import Control.Concurrent.Async

import TradingFramework
import Market.Types

import GDAXProducer

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
         [ToDo [CancelLimitOrder {acOrderID = OID 333 444}] reasonMessage])

  ]

--------------------------------------------------------------------------------
reasonMessage = "Canceling placed limit order: CancelLimitOrder {acOrderID = OID {hw = 333, lw = 444}}\n"

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
    => (   Event (OrderPlacement    p v)
        -> Event (OrderCancellation    )
        -> Event (OrderFill         p v)
        -> Event (QuoteBook         p v q c)
        -> (Event output -> MomentIO ())
        -> MomentIO ()
       )
    -> [TradingE p v q c]
    -> [output]
    -> IO ()

compareOutputTest networkDescription inputs expecteds = do
    (inputAHandlers, fireInputA) <- newHandlerSet
    (inputBHandlers, fireInputB) <- newHandlerSet
    (inputCHandlers, fireInputC) <- newHandlerSet
    (inputDHandlers, fireInputD) <- newHandlerSet
    -- create TVAR to hold final list of output events
    tv <- newTVarIO []

    network <- compile $ do
        eA <- fromHandlerSet inputAHandlers
        eB <- fromHandlerSet inputBHandlers
        eC <- fromHandlerSet inputCHandlers
        eD <- fromHandlerSet inputDHandlers
        networkDescription eA eB eC eD
          (\e -> accumIntoListEvents e >>= logLastEventInTVar tv)

    activate network

    let triggerIO :: TradingE p v q c -> IO()
        triggerIO (TP inputA) = fireInputA inputA
        triggerIO (TC inputB) = fireInputB inputB
        triggerIO (TF inputC) = fireInputC inputC
        triggerIO (TB inputD) = fireInputD inputD

    sequence_ $ fmap triggerIO inputs
    outputEvents <- readTVarIO tv
    assertEqual "Output list does not match" expecteds (reverse outputEvents)

  where

    logLastEventInTVar :: TVar a -> Event a -> MomentIO ()
    logLastEventInTVar tv e = reactimate . fmap (atomically . writeTVar tv) $ e

    -- cons successive events onto the head of a list
    accumIntoListEvents :: MonadMoment m => Event a -> m (Event [a])
    accumIntoListEvents event = accumE [] (fmap (:) event)
