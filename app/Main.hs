{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- import Control.Concurrent           (threadDelay)
import Control.Concurrent.Async

import Reactive.Banana
import Reactive.Banana.Frameworks.Extended

import Pipes.Concurrent

import System.IO                (hPutStr, hPutStrLn, stderr) -- Remove me!

-- import Market.Types
-- import GDAXProducer
-- import GDAXAdapter
-- import GDAXExecutor

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

-- mirrorStrategy
--     :: forall m p v q c. (MonadMoment m, Coin p, Coin v)
--     => Vol v -> Event (TradingEv p v q c) -> Event (TradingEv p v q c) 
-- -> m (Event (Maybe (StrategyAdvice (Action p v)), Maybe (StrategyAdvice (Action p v)))) 

showBook 
    :: forall m p v q c. (MonadMoment m, Coin p, Coin v)
    => Event (TradingEv p v q c) 
    -> m (Event (Maybe (StrategyAdvice (Action p v))))
showBook _ = return never



getCoinbeneConfig = return ()

main :: IO ()
main = do

    coinbeneConfig <- getCoinbeneConfig

    putStrLn "--------------------------- Starting --------------------------------"
    putStrLn "Type <ENTER> to quit"
    putStrLn "---------------------------------------------------------------------"

    -- event dispatch handlers
    (handlersUSDTBTC, fireUSDTBTC) <- newHandlerSet
    -- (handlersUSDLTC, fireUSDLTC) <- newHandlerSet
    -- (handlersBTCLTC, fireBTCLTC) <- newHandlerSet
    -- execution FIFO queues
    (outputUSDTBTC, inputUSDTBTC, stopExecutorUSDTBTC) <- spawn' unbounded
    -- (outputUSDLTC, inputUSDLTC, stopExecutorUSDLTC) <- spawn' unbounded
    -- (outputBTCLTC, inputBTCLTC, stopExecutorBTCLTC) <- spawn' unbounded

    network <- compile $ do
        esUSDTBTC  <- fromHandlerSet handlersUSDTBTC
    --     esUSDLTC  <- fromHandlerSet handlersUSDLTC
    --     esBTCLTC  <- fromHandlerSet handlersBTCLTC

        esAdvice <- showBook esUSDTBTC
    --     (eUSDBTC, eUSDLTC, eBTCLTC) <- showAllBooks
    --         (esUSDBTC :: Event (GDAXTradingE USD BTC))
    --         (esUSDLTC :: Event (GDAXTradingE USD LTC))
    --         (esBTCLTC :: Event (GDAXTradingE BTC LTC))

        reactimate $ fmap (maybe (pure ()) (logAndQueue outputUSDTBTC)) (esAdvice :: Event (Maybe (StrategyAdvice (Action USD BTC))))
        -- reactimate $ fmap (logAndQueue outputUSDLTC) eUSDLTC

    --     reactimate $ fmap (logAndQueue outputBTCLTC) eBTCLTC

    -- start sensory threads

    -- fromMarketStream gdaxConfig (fireUSDBTC . TB) (fireUSDBTC . TP) (fireUSDBTC . TC) (fireUSDBTC . TF)
    -- fromMarketStream gdaxConfig (fireUSDLTC . TB) (fireUSDLTC . TP) (fireUSDLTC . TC) (fireUSDLTC . TF)
    -- fromMarketStream gdaxConfig (fireBTCLTC . TB) (fireBTCLTC . TP) (fireBTCLTC . TC) (fireBTCLTC . TF)

    -- start execution threads
    execUSDTBTC <- async $ runExecutor inputUSDTBTC print (hPutStrLn stderr "\nExecutor exiting!") 
    link execUSDTBTC

    -- execUSDLTC <- async $ runExecutor (doAtGDAX gdaxConfig) inputUSDLTC
    -- link execUSDLTC

    -- execBTCLTC <- async $ runExecutor (doAtGDAX gdaxConfig) inputBTCLTC
    -- link execBTCLTC

    -- run until users presses <ENTER> key
    activate network
    keyboardWait

    -- -- Shutdown
    -- -- 30 seconds timeout for executors to finish their job before terminating
    -- atomically stopExecutorUSDBTC
    -- atomically stopExecutorUSDLTC
    -- atomically stopExecutorBTCLTC
    -- race_
    --     (threadDelay (30 * 1000000))
    --     (mapConcurrently_ wait [execUSDBTC, execUSDLTC, execBTCLTC])


--------------------------------------------------------------------------------

keyboardWait :: IO ()
keyboardWait = getLine >> return () -- wait for <ENTER> to be pressed