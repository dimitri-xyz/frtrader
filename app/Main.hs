{-# LANGUAGE ScopedTypeVariables #-}

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
import Trading.Strategy             (mirrorStrategy2, defineTarget, AskSide(..), BidSide(..), mirrorStrategy3)
import Market.Interface
import Reactive.Banana.Combinators  (never, filterE) -- FIX ME! remove me!
import Market.Coins                 (USD(..), BTC(..), BRL(..)) -- FIX ME! remove me!

import Coinbene.Connector
import Coinbene                     (Coinbene(..), API_ID(..), API_KEY(..), Verbosity(..)) -- FIX ME! Remove me.

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
        error "Usage: frt pollRate mirrorSide maxExposure slipVol placeVol USDT-SellPrice USDT-BuyPrice"
    else
        return ()

    let pollingInterval :: Double =                 ((read $ args !! 0) :: Double )
        mirrorSide      :: String =                          args !! 1
        maxExposure :: Vol BTC = Vol   $ realToFrac ((read $ args !! 2) :: Double )
        slipVol     :: Vol BTC = Vol   $ realToFrac ((read $ args !! 3) :: Double )
        placeVol    :: Vol BTC = Vol   $ realToFrac ((read $ args !! 4) :: Double )
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
    (handlers1, fire1) <- newHandlerSet
    (handlers2, fire2) <- newHandlerSet

    -- execution FIFO queues
    (ctrlOutput, ctrlInput, stopCtrlExecutor) <- spawn' unbounded
    (output1   , input1   , stopExecutor1)    <- spawn' unbounded
    (output2   , input2   , stopExecutor2)    <- spawn' unbounded

    -- Initialize Connectors
    (producer1, executor1, terminator1) <- coinbeneInit (truncate $ pollingInterval * 1000000) Verbose coinbeneConfig (Proxy :: Proxy IO) fire1
    (producer2, executor2, terminator2) <- coinbeneInit (truncate $ pollingInterval * 1000000) Verbose coinbeneConfig (Proxy :: Proxy IO) fire2


    -- Build and start the strategy
    network <- compile $ do
        ctrlEs <- fromHandlerSet ctrlHandlers
        es1    <- fromHandlerSet handlers1
        es2    <- fromHandlerSet handlers2

        triple <- case mirrorSide of
                "ASKS" -> mirrorStrategy3 xSellRate xBuyRate maxExposure (defineTarget asks slipVol placeVol) AskSide ctrlEs es1 es2
                "BIDS" -> mirrorStrategy3 xSellRate xBuyRate maxExposure (defineTarget bids slipVol placeVol) BidSide ctrlEs es1 es2
                _      -> error $ "Argument 'mirrorSide' must be either ASKS or BIDS (in all caps)."

        let ctrlAs' = fst3 <$> triple
            esAdv1' = snd3 <$> triple
            esAdv2' = thd3 <$> triple
            ctrlAs = fromJust <$> filterE isJust ctrlAs'
            esAdv1 = fromJust <$> filterE isJust esAdv1'
            esAdv2 = fromJust <$> filterE isJust esAdv2'

        reactimate $
            fmap (logAndQueueControl ctrlOutput)
            (ctrlAs :: Event (ControlAction))

        reactimate $
            fmap (logAndQueueAdvice output1)
            (esAdv1 :: Event (StrategyAdvice (Action USD BTC)))

        reactimate $
            fmap (logAndQueueAdvice output2)
            (esAdv2 :: Event (StrategyAdvice (Action BRL BTC)))

    activate network

    -- start execution threads
    ctrlExecThread <- async $ runExecutor ctrlInput (ctrlExecutor stopCtrlExecutor) ctrlFinalizer
    link ctrlExecThread
    exec1          <- async $ runExecutor input1     executor1 terminator1
    link exec1
    exec2          <- async $ runExecutor input2     executor2 terminator2
    link exec2

    -- start producer threads
    prod1 <- async producer1
    link prod1
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
    atomically stopExecutor1
    atomically stopExecutor2
    race_
        (threadDelay (30 * 1000000))
        (mapConcurrently_ wait [exec1, exec2])

    case eExitCode of
        Left  _ -> exitWith (ExitFailure 89) -- let's just say 89 is strategy timeout
        Right _ -> do
            -- exceptions on ctrlExecThread are propagated to this one and we would never get here
            putStrLn "success!"
            exitSuccess

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
