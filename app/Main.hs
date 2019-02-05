{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent           (threadDelay)
import Control.Concurrent.Async

import Reactive.Banana
import Reactive.Banana.Frameworks.Extended

import Pipes.Concurrent

import System.IO                    (hPutStr, hPutStrLn, stderr) -- Remove me!

import Trading.Framework
import Trading.Strategy
import Market.Interface

import Reactive.Banana.Combinators (never) -- FIX ME! remove me!
import Market.Coins (USD(..), BTC(..)) -- FIX ME! remove me!

{-
This program uses multiple threads with an event network using our "push-pull" model.
1. We have producer threads that produce and fire "orderbook" and other events.
2. A trading strategy
3. Execution threads that process the Actions put in their respective action queues
-}

showBook 
    :: forall m p v q c. (MonadMoment m, Coin p, Coin v)
    => Event (TradingEv p v q c) 
    -> m (Event (Maybe (StrategyAdvice (Action p v))))
showBook _ = return never


type Producer p v q c = Handler (TradingEv p v q c) -> IO ()
type Executor p v     = Action p v -> IO ()
type Terminator       = IO ()

---------------------------------------
coinbeneInitializer :: forall p v q c. (Coin p, Coin v) => IO (Producer p v q c, Executor p v, Terminator)
coinbeneInitializer = return (prodState undefined undefined, execState undefined undefined, termState undefined undefined)

prodState :: (Coin p, Coin v) => config -> state -> Producer p v q c
prodState config state handler = return ()

execState :: (Coin p, Coin v) => config -> state -> Executor p v
execState _config _state = print

termState :: config -> state -> Terminator
termState _ _ = hPutStrLn stderr "\nExecutor exiting!"
---------------------------------------

main :: IO ()
main = do

    (producer, executor, terminator) <- coinbeneInitializer

    putStrLn "--------------------------- Starting --------------------------------"
    putStrLn "Type <ENTER> to quit"
    putStrLn "---------------------------------------------------------------------"

    -- event dispatch handlers
    (handlersUSDTBTC, fireUSDTBTC) <- newHandlerSet

    -- execution FIFO queues
    (outputUSDTBTC, inputUSDTBTC, stopExecutorUSDTBTC) <- spawn' unbounded

    network <- compile $ do
        esUSDTBTC <- fromHandlerSet handlersUSDTBTC
        esAdvice  <- showBook esUSDTBTC
        reactimate $ 
            fmap (maybe (pure ()) (logAndQueue outputUSDTBTC)) 
            (esAdvice :: Event (Maybe (StrategyAdvice (Action USD BTC))))

    -- start sensory threads
    prodUSDTBTC <- async $ producer fireUSDTBTC
    link prodUSDTBTC

    -- start execution threads
    execUSDTBTC <- async $ runExecutor 
                    inputUSDTBTC 
                    executor
                    terminator
    link execUSDTBTC

    -- run until users presses <ENTER> key
    activate network
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