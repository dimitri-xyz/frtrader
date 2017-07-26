module Main where

import Control.Concurrent           (threadDelay)
import Control.Concurrent.Async

import Reactive.Banana
import Reactive.Banana.Frameworks
import Pipes.Concurrent

import Market.Types
import GDAXProducer
import GDAXAdapter
import GDAXExecutor

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
  (handlers, fire) <- newHandlerSet
  -- execution FIFO queue
  (output, input, stopExecutor) <- spawn' unbounded

  network <- compile $ do
      es  <- fromHandlerSet handlers
      dumbStrategy (es :: Event (GDAXTradingE BTC LTC)) (reactimate . fmap (logAndExecute output))

  -- start sensory threads
  fromMarketStream gdaxConfig (fire . TB) (fire . TP) (fire . TC) (fire . TF)

  -- start execution thread
  exec <- async $ runExecutor (doAtGDAX gdaxConfig) input
  link exec

  -- run until users presses <ENTER> key
  activate network
  keyboardWait

  -- Shutdown
  -- 30 seconds timeout for executor to finish its job before terminating
  atomically stopExecutor
  race_
    (threadDelay (30 * 1000000))
    (wait exec)


--------------------------------------------------------------------------------

keyboardWait :: IO ()
keyboardWait = getLine >> return () -- wait for <ENTER> to be pressed
