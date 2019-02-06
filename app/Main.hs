{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent           (threadDelay)
import Control.Concurrent.Async

import Reactive.Banana
import Reactive.Banana.Frameworks.Extended

import Pipes.Concurrent

import Trading.Framework
import Trading.Strategy
import Market.Interface

import Reactive.Banana.Combinators (never) -- FIX ME! remove me!
import Market.Coins (USD(..), BTC(..)) -- FIX ME! remove me!

import Coinbene.Connector

{-
This program uses multiple threads with an event network using our "push-pull" model.
1. We have producer threads that produce and fire "orderbook" and other events.
2. A trading strategy
3. Execution threads that process the Actions put in their respective action queues
-}

type Producer p v q c = IO ()
type Executor p v     = Action p v -> IO ()
type Terminator       = IO ()

---------------------------------------

main :: IO ()
main = do

    putStrLn "--------------------------- Starting --------------------------------"
    putStrLn "Type <ENTER> to quit"
    putStrLn "---------------------------------------------------------------------"

    -- event dispatch handlers
    (handlersUSDTBTC, fireUSDTBTC) <- newHandlerSet

    -- execution FIFO queues
    (outputUSDTBTC, inputUSDTBTC, stopExecutorUSDTBTC) <- spawn' unbounded

    -- Initialize Connectors
    (producer, executor, terminator) <- coinbeneInitializer fireUSDTBTC

    -- Build and start the strategy
    network <- compile $ do
        esUSDTBTC <- fromHandlerSet handlersUSDTBTC
        esAdvice  <- showBook esUSDTBTC
        reactimate $ 
            fmap (logAndQueue outputUSDTBTC)
            (esAdvice :: Event (StrategyAdvice (Action USD BTC)))

    activate network

    -- start execution threads
    execUSDTBTC <- async $ runExecutor inputUSDTBTC executor terminator
    link execUSDTBTC

    -- start producer threads
    prodUSDTBTC <- async producer
    link prodUSDTBTC

    -- run until users presses <ENTER> key
    keyboardWait

    -- Shutdown
    -- 30 seconds timeout for executors to finish their job before terminating
    atomically stopExecutorUSDTBTC
    race_
        (threadDelay (30 * 1000000))
        (mapConcurrently_ wait [execUSDTBTC])


--------------------------------------------------------------------------------
keyboardWait :: IO ()
keyboardWait = getLine >> return () -- wait for <ENTER> to be pressed


showBook 
    :: forall m p v q c. (MonadMoment m, Coin p, Coin v)
    => Event (TradingEv p v q c) 
    -> m (Event (StrategyAdvice (Action p v)))
showBook _ = return never

