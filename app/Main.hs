{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#ifdef ETHEREUM
#define D_CURRENCY ETH
#define D_BUCKETSIZE 1
#define D_USAGE_MSG     "Usage: frt-eth pollRate mirrorSide maxExposure slipVol placeVol USDT-SellPrice USDT-BuyPrice"

#elif TETHER
#define D_CURRENCY      USD
#define D_BUCKETSIZE    undefined
#define D_USAGE_MSG     "Usage: frt-usdt pollRate mirrorSide maxExposure slipVol(ignored) placeVol USDT-SellPrice USDT-BuyPrice"

#else
#define D_CURRENCY      BTC
#define D_BUCKETSIZE    20
#define D_USAGE_MSG     "Usage: frt pollRate mirrorSide maxExposure slipVol placeVol USDT-SellPrice USDT-BuyPrice"
#endif

module Main where

import Data.Proxy
import Data.Maybe                   (fromJust, isJust)
import Data.Tuple.Extra             (fst3, snd3, thd3)

import Control.Concurrent           (threadDelay)
import Control.Concurrent.Async
import Control.Monad                (forever)
import Control.Exception            (catch, IOException(..))
import System.IO.Error              (isEOFError)
import System.Environment
import System.Exit

import Reactive.Banana
import Reactive.Banana.Frameworks.Extended

import Pipes.Concurrent

import Trading.Framework
import Trading.Strategy             (mirrorStrategy2, defineTarget, AskSide(..), BidSide(..), mirrorStrategy3, keepOpenStrategy, fixedTarget)
import Market.Interface
import Reactive.Banana.Combinators  (never, filterE) -- FIX ME! remove me!
import Market.Coins

import Coinbene.Connector           (Coinbene(..), API_ID(..), API_KEY(..), Verbosity(..), getCoinbeneConfig, coinbeneInit)

{-
This program uses multiple threads with an event network using our "push-pull" model.
1. We have producer threads that produce and fire "orderbook" and other events.
2. A trading strategy
3. Execution threads that process the Actions put in their respective action queues
-}

type Producer p v q c = IO ()
type Executor p v     = Action p v -> IO ()
type Terminator       = IO ()


main :: IO ()
main = do

    args <- getArgs

    if length args /= 7 then
        error D_USAGE_MSG
    else
        return ()

    let pollingInterval :: Double =                 ((read $ args !! 0) :: Double )
        mirrorSide      :: String =                          args !! 1
        maxExposure :: Vol D_CURRENCY = Vol   $ realToFrac ((read $ args !! 2) :: Double )
        slipVol     :: Vol D_CURRENCY = Vol   $ realToFrac ((read $ args !! 3) :: Double )
        placeVol    :: Vol D_CURRENCY = Vol   $ realToFrac ((read $ args !! 4) :: Double )
        xSellRate :: Price BRL = Price $ realToFrac ((read $ args !! 5) :: Double )
        xBuyRate  :: Price BRL = Price $ realToFrac ((read $ args !! 6) :: Double )

    putStrLn $  "Poll interval:  " <> show pollingInterval
             <> "\nMirror side:  " <> mirrorSide
             <> "\nMax exposure: " <> show maxExposure
             <> "\nSlippage vol: " <> show slipVol
             <> "\ntarget vol:   " <> show placeVol
             <> "\nUSDT sell rate: " <> show xSellRate
             <> "\nUSDT  buy rate: " <> show xBuyRate

    coinbeneConfig <- getCoinbeneConfig Verbose

    putStrLn "--------------------------- Starting --------------------------------"
    putStrLn "Type <ENTER> to quit"
    putStrLn "---------------------------------------------------------------------"

    -- event dispatch handlers
    (ctrlHandlers, fireControl) <- newHandlerSet
#ifndef TETHER
    (handlers1, fire1) <- newHandlerSet
#endif
    (handlers2, fire2) <- newHandlerSet

    -- execution FIFO queues
    (ctrlOutput, ctrlInput, stopCtrlExecutor) <- spawn' unbounded
#ifndef TETHER
    (output1   , input1   , stopExecutor1)    <- spawn' unbounded
#endif
    (output2   , input2   , stopExecutor2)    <- spawn' unbounded

    -- Initialize Connectors
#ifndef TETHER
    (producer1, executor1, terminator1) <- coinbeneInit (truncate $ pollingInterval * 1000000) Verbose coinbeneConfig (Proxy :: Proxy IO) fire1
#endif
    (producer2, executor2, terminator2) <- coinbeneInit (truncate $ pollingInterval * 1000000) Verbose coinbeneConfig (Proxy :: Proxy IO) fire2


    -- Build and start the strategy
    network <- compile $ do
        ctrlEs <- fromHandlerSet ctrlHandlers
#ifndef TETHER
        es1    <- fromHandlerSet handlers1
#endif
        es2    <- fromHandlerSet handlers2

#ifndef TETHER
        triple <- case mirrorSide of
                "ASKS" -> mirrorStrategy3 xSellRate xBuyRate maxExposure (defineTarget D_BUCKETSIZE asks slipVol placeVol) AskSide ctrlEs es1 es2
                "BIDS" -> mirrorStrategy3 xSellRate xBuyRate maxExposure (defineTarget D_BUCKETSIZE bids slipVol placeVol) BidSide ctrlEs es1 es2
                _      -> error $ "Argument 'mirrorSide' must be either ASKS or BIDS (in all caps)."

        let ctrlAs' = fst3 <$> triple
            esAdv1' = snd3 <$> triple
            esAdv2' = thd3 <$> triple
            ctrlAs = fromJust <$> filterE isJust ctrlAs'
            esAdv1 = fromJust <$> filterE isJust esAdv1'
            esAdv2 = fromJust <$> filterE isJust esAdv2'

#else
        pair <- case mirrorSide of
                "ASKS" -> keepOpenStrategy maxExposure (fixedTarget xSellRate placeVol) AskSide ctrlEs es2
                "BIDS" -> keepOpenStrategy maxExposure (fixedTarget xBuyRate  placeVol) BidSide ctrlEs es2
                _      -> error $ "Argument 'mirrorSide' must be either ASKS or BIDS (in all caps)."

        let ctrlAs' = fst <$> pair
            esAdv2' = snd <$> pair
            ctrlAs = fromJust <$> filterE isJust ctrlAs'
            esAdv2 = fromJust <$> filterE isJust esAdv2'

#endif

        reactimate $
            fmap (logAndQueueControl ctrlOutput)
            (ctrlAs :: Event (ControlAction))

#ifndef TETHER
        reactimate $
            fmap (logAndQueueAdvice output1)
            (esAdv1 :: Event (StrategyAdvice (Action USD D_CURRENCY)))
#endif

        reactimate $
            fmap (logAndQueueAdvice output2)
            (esAdv2 :: Event (StrategyAdvice (Action BRL D_CURRENCY)))

    activate network

    -- start execution threads
    ctrlExecThread <- async $ runExecutor ctrlInput (ctrlExecutor stopCtrlExecutor) ctrlFinalizer
    link ctrlExecThread
#ifndef TETHER
    exec1          <- async $ runExecutor input1     executor1 terminator1
    link exec1
#endif
    exec2          <- async $ runExecutor input2     executor2 terminator2
    link exec2

    -- start producer threads
#ifndef TETHER
    prod1 <- async producer1
    link prod1
#endif
    prod2 <- async producer2
    link prod2


    -- shutdown thread
    timeouter <- async $ do
            shutdownTrigger
            logger "Shutdown initiated. Shutting down strategy.\n"
            fireControl ShutdownEv    -- give strategy early warning
            threadDelay (120 * 1000000)
            return () -- we only get here before being "canceled" on a Strategy timeout! Bad, bad!
    link timeouter

    -- Shutdown
    eExitCode <- waitEither
        timeouter
        ctrlExecThread -- this thread terminates on a `ShutdownDone` ControlAction

    -- The strategy either already shutdown or should killed at this point
    -- give 30 seconds (if needed) for executors to finish their job before terminating
#ifndef TETHER
    atomically stopExecutor1
#endif
    atomically stopExecutor2
    race_
        (threadDelay (30 * 1000000))
        (mapConcurrently_ wait [
#ifndef TETHER
            exec1,
#endif
            exec2])

    case eExitCode of
        Left  _ -> exitWith (ExitFailure 89) -- let's just say 89 is strategy timeout
        Right _ -> do
            -- exceptions on ctrlExecThread are propagated to this one and we would never get here
            putStrLn "success!"
            -- FIX ME! This should be exitSuccess, but I am affraid of inadvertent use of
            -- error code zero during error treatment. sticking to this for now.
            exitWith (ExitFailure 55)

--------------------------------------------------------------------------------
keyboardWait :: IO ()
keyboardWait = getLine >> return () -- wait for <ENTER> to be pressed

-- this runs until users presses <ENTER> key or (if we have no stdin, as inside a cron job) a timeout interval
shutdownTrigger :: IO ()
shutdownTrigger = catch keyboardWait
    (\e -> do
        let err = show (e :: IOException)
        if isEOFError e
            then do
                putStrLn ("Warning: isEOFError exception thrown on keyboard input. Running for 1 hour 55 mins. Ctrl-C to abort.")
                threadDelay (115 * 60 * 1000000)
            else
                putStrLn ("Error: Aborting execution. Unknown exception thrown on keyboard input:" <> err )
    )
