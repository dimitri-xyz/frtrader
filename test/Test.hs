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
tests _ _ = testGroup " Trading Framework Tests"
  [ testCase "Output list comparison test"
        (listComparisonTest cancelAllLimitOrders
         [ Left  (QuoteBook {})
         , Right (Placement limOrder :: OrderPlacement p v)
         , Left  (QuoteBook {})
         , Right (Placement (MarketOrder {}))]
          -- expected output
         [ToDo [CancelLimitOrder {acOrderID = OID 333 444}] reasonMessage])

  , testCase "Output execution list comparison test"
        (executionTest
          (repeatOnEveryBookEvent
                [ PANIC "1", CancelLimitOrder {acOrderID = OID 333 444}, PANIC "2"]
                "")
          -- repeat twice, once for each book event
          [ Left (QuoteBook {} :: GDAXQuotebook p v), Left (QuoteBook {})]
          -- expected output
          [ PANIC "1"
          , CancelLimitOrder {acOrderID = OID {hw = 333, lw = 444}}
          , PANIC "2"
          , PANIC "1"
          , CancelLimitOrder {acOrderID = OID {hw = 333, lw = 444}}
          , PANIC "2"])
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
listComparisonTest :: (Eq output, Show output)
                   => (  Event inputA
                      -> Event inputB
                      -> Event inputC
                      -> Event inputD
                      -> (Event output -> MomentIO ())
                      -> MomentIO ()
                      )
                   -> [Either inputA inputB]
                   -> [output]
                   -> IO ()

listComparisonTest networkDescription inputs expecteds = do

  (inputAHandlers, fireInputA) <- newHandlerSet
  (inputBHandlers, fireInputB) <- newHandlerSet

  -- create TVAR to hold final list of output events
  tv <- newTVarIO []

  network <- compile $ do
      eA <- fromHandlerSet inputAHandlers
      eB <- fromHandlerSet inputBHandlers
      networkDescription eA eB never never
        (\e -> accumIntoListEvents e >>= logLastEventInTVar tv)

  activate network

  let triggerIO (Left  inputA) = fireInputA inputA
      triggerIO (Right inputB) = fireInputB inputB

  sequence_ $ fmap triggerIO inputs
  outputEvents <- readTVarIO tv
  assertEqual "Output list does not match" expecteds (reverse outputEvents)


----- Helpers ------

logLastEventInTVar :: TVar a -> Event a -> MomentIO ()
logLastEventInTVar tv e = reactimate . fmap (atomically . writeTVar tv) $ e

-- cons successive events onto the head of a list
accumIntoListEvents :: MonadMoment m => Event a -> m (Event [a])
accumIntoListEvents event = accumE [] (fmap (:) event)
--------------------------------------------------------------------------------

executionTest :: (Coin p, Coin v)
              => (  Event inputA
                 -> Event inputB
                 -> Event inputC
                 -> Event inputD
                 -> (Event (StrategyAdvice p v) -> MomentIO ())
                 -> MomentIO ())
              -> [Either inputA inputB]
              -> [Action p v]
              -> IO ()

executionTest networkDescription inputs expecteds = do

  (inputAHandlers, fireInputA) <- newHandlerSet
  (inputBHandlers, fireInputB) <- newHandlerSet
  -- execution FIFO queue
  (output, input, stopExecutor) <- spawn' unbounded

  -- create TVAR to hold list of outputs
  tv <- newTVarIO []

  network <- compile $ do
      eA <- fromHandlerSet inputAHandlers
      eB <- fromHandlerSet inputBHandlers
      networkDescription eA eB never never
        (reactimate . fmap (logAndExecute output))

  -- start execution thread
  exec <- async $ runExecutor (prependAction tv) input
  link exec

  activate network

  let triggerIO (Left  inputA) = fireInputA inputA
      triggerIO (Right inputB) = fireInputB inputB

  sequence_ $ fmap triggerIO inputs
  wait exec -- ensure execution is done before comparing
  atomically stopExecutor

  outputActions <- readTVarIO tv
  assertEqual "Output list does not match" expecteds (reverse outputActions)


  where
    prependAction :: TVar [Action p v] -> Action p v-> IO ()
    prependAction tv action = atomically $ modifyTVar tv (\as -> action:as)

----------------------------------------
repeatOnEveryBookEvent :: Show b
         => [Action p v]
         -> Reasoning
         -> Event (QuoteBook p v a b)
         -> Event c
         -> Event d
         -> Event e
         -> (Event (StrategyAdvice p v) -> MomentIO ())
         -> MomentIO ()
repeatOnEveryBookEvent actions reason eNewBook _ _ _ runOnOutputEvents = do
  let eAdvice = (\_ -> ToDo actions reason) <$> eNewBook
  runOnOutputEvents eAdvice
