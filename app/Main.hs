module Main where

import Control.Concurrent           (threadDelay)
import Control.Concurrent.Async

import Reactive.Banana
import Reactive.Banana.Frameworks
import Pipes.Concurrent

import Market.Types
import Coinbase.Producer
import Coinbase.Adapter
import Coinbase.Executor

import TradingFramework

{-
This program uses multiple threads with an event network using our "push-pull" model.

1. We have "sensory" threads that produces new orderbooks and fire events from GDAX.
2. A very simple strategy that Shows the orderbook events received.
3. An execution thread that blocks until it has something to do.
4. The frameworks logs the reasoning and executes actions by placing them in the execution FIFO.

-}

main :: IO ()
main = do

  gdaxConfig <- getGDAXConfig

  putStrLn "--------------------------- Starting --------------------------------"
  putStrLn "Type <ENTER> to quit"
  putStrLn "---------------------------------------------------------------------"

  -- event dispatch handlers
  (handlersUSDBTC, fireUSDBTC) <- newHandlerSet
  (handlersUSDLTC, fireUSDLTC) <- newHandlerSet
  (handlersBTCLTC, fireBTCLTC) <- newHandlerSet
  -- execution FIFO queues
  (outputUSDBTC, inputUSDBTC, stopExecutorUSDBTC) <- spawn' unbounded
  (outputUSDLTC, inputUSDLTC, stopExecutorUSDLTC) <- spawn' unbounded
  (outputBTCLTC, inputBTCLTC, stopExecutorBTCLTC) <- spawn' unbounded

  network <- compile $ do
      esUSDBTC  <- fromHandlerSet handlersUSDBTC
      esUSDLTC  <- fromHandlerSet handlersUSDLTC
      esBTCLTC  <- fromHandlerSet handlersBTCLTC

      showAllBooks
          (esUSDBTC :: Event (GDAXTradingE USD BTC))
          (esUSDLTC :: Event (GDAXTradingE USD LTC))
          (esBTCLTC :: Event (GDAXTradingE BTC LTC))
          (reactimate . fmap (logAndExecute outputUSDBTC))
          (reactimate . fmap (logAndExecute outputUSDLTC))
          (reactimate . fmap (logAndExecute outputBTCLTC))

  -- start sensory threads
  fromMarketStream gdaxConfig (fireUSDBTC . TB) (fireUSDBTC . TP) (fireUSDBTC . TC) (fireUSDBTC . TF)
  fromMarketStream gdaxConfig (fireUSDLTC . TB) (fireUSDLTC . TP) (fireUSDLTC . TC) (fireUSDLTC . TF)
  fromMarketStream gdaxConfig (fireBTCLTC . TB) (fireBTCLTC . TP) (fireBTCLTC . TC) (fireBTCLTC . TF)

  -- start execution threads
  execUSDBTC <- async $ runExecutor (doAtGDAX gdaxConfig) inputUSDBTC
  link execUSDBTC

  execUSDLTC <- async $ runExecutor (doAtGDAX gdaxConfig) inputUSDLTC
  link execUSDLTC

  execBTCLTC <- async $ runExecutor (doAtGDAX gdaxConfig) inputBTCLTC
  link execBTCLTC

  -- run until users presses <ENTER> key
  activate network
  keyboardWait

  -- Shutdown
  -- 30 seconds timeout for executors to finish their job before terminating
  atomically stopExecutorUSDBTC
  atomically stopExecutorUSDLTC
  atomically stopExecutorBTCLTC
  race_
    (threadDelay (30 * 1000000))
    (mapConcurrently_ wait [execUSDBTC, execUSDLTC, execBTCLTC])


--------------------------------------------------------------------------------

keyboardWait :: IO ()
keyboardWait = getLine >> return () -- wait for <ENTER> to be pressed
