{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Proxy
import Data.Maybe                   (fromJust, isJust) -- FIX ME!
import Control.Concurrent           (threadDelay)
import Control.Concurrent.Async

import Reactive.Banana
import Reactive.Banana.Frameworks.Extended

import Pipes.Concurrent

import Trading.Framework
import Trading.Strategy             (mirrorStrategy)
import Market.Interface

import Reactive.Banana.Combinators  (never, filterE) -- FIX ME! remove me!
import Market.Coins                 (USD(..), BTC(..), BRL(..)) -- FIX ME! remove me!

import Coinbene.Connector
import Coinbene                     (Coinbene(..), API_ID(..), API_KEY(..)) -- FIX ME! Remove me.

import Network.HTTP.Client          (newManager)
import Network.HTTP.Client.TLS      (tlsManagerSettings)

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

    manager <- newManager tlsManagerSettings
    let coinbeneConfig = Coinbene manager 


    putStrLn "--------------------------- Starting --------------------------------"
    putStrLn "Type <ENTER> to quit"
    putStrLn "---------------------------------------------------------------------"

    -- event dispatch handlers
    (handlers1, fire1) <- newHandlerSet
    (handlers2, fire2) <- newHandlerSet

    -- execution FIFO queues
    (output1, input1, stopExecutor1) <- spawn' unbounded
    (output2, input2, stopExecutor2) <- spawn' unbounded

    -- Initialize Connectors
    let exchangeConfig = undefined
    (producer1, executor1, terminator1) <- coinbeneInit 1000000 coinbeneConfig (Proxy :: Proxy IO) fire1
    (producer2, executor2, terminator2) <- coinbeneInit 1000000 coinbeneConfig (Proxy :: Proxy IO) fire2


    -- Build and start the strategy
    network <- compile $ do
        es1 <- fromHandlerSet handlers1
        es2 <- fromHandlerSet handlers2

        esAdvice <- mirrorStrategy 0.0022 es1 es2

        let esAdv1 = fromJust <$> filterE isJust (fst <$> esAdvice)
        let esAdv2 = fromJust <$> filterE isJust (snd <$> esAdvice)

        reactimate $ 
            fmap (logAndQueue output1)
            (esAdv1 :: Event (StrategyAdvice (Action BRL BTC)))

        reactimate $ 
            fmap (logAndQueue output2)
            (esAdv2 :: Event (StrategyAdvice (Action BRL BTC)))

    activate network

    -- start execution threads
    exec1 <- async $ runExecutor input1 executor1 terminator1
    link exec1
    exec2 <- async $ runExecutor input2 executor2 terminator2
    link exec2

    -- start producer threads
    prod1 <- async producer1
    link prod1
    prod2 <- async producer2
    link prod2

    -- run until users presses <ENTER> key
    keyboardWait

    -- Shutdown
    -- 30 seconds timeout for executors to finish their job before terminating
    atomically stopExecutor1
    atomically stopExecutor2
    race_
        (threadDelay (30 * 1000000))
        (mapConcurrently_ wait [exec1, exec2])


--------------------------------------------------------------------------------
keyboardWait :: IO ()
keyboardWait = getLine >> return () -- wait for <ENTER> to be pressed


showBook 
    :: forall m p v q c. (MonadMoment m, Coin p, Coin v)
    => Event (TradingEv p v q c) 
    -> m (Event (StrategyAdvice (Action p v)))
showBook _ = return never

