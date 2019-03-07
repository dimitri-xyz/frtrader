{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Proxy
import Data.Maybe                   (fromJust, isJust)
import Control.Concurrent           (threadDelay)
import Control.Concurrent.Async
import Control.Monad                (forever)
import Control.Exception            (catch, IOException(..))
import System.IO.Error              (isEOFError)
import System.Environment

import Reactive.Banana
import Reactive.Banana.Frameworks.Extended

import Pipes.Concurrent

import Trading.Framework
import Trading.Strategy             (mirrorStrategy2, AskSide(..), BidSide(..))
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

---------------------------------------
safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (a:as) = Just a

-- returns what price needs to be offered to buy/sell `lim` volume from/to list.
priceExtremum :: (Num v, Ord v) => v -> [(p,v)] -> Maybe p
priceExtremum lim [] = Nothing -- not enough available
priceExtremum lim ((p,v):xs)
    | v >= lim  = Just p
    | otherwise = priceExtremum (lim - v) xs

getAskTarget2 = maybe (Price 0, Vol 0) (\p -> (bucketPrice p, Vol 0.1122)) . priceExtremum 0.2 . fmap toPair . asks
  where
    toPair q = (price q, volume q)
    convertPriceType (Price p) = Price (realToFrac p)
    bucketPrice (Price p) =
        let p' = (*20) . (`div` 20) . ceiling $ p
         in Price (realToFrac p')

-----------
getAskTarget = maybe (Price 0, Vol 0) (\q -> (bucketPrice (price q), Vol 0.0003)) . safeHead . asks
  where
    bucketPrice (Price p) =
        let p' = {- (*50) . (`div` 50) .-} ceiling $ p
         in Price (realToFrac p')

getBidTarget = maybe (Price 0, Vol 0) (\q -> (bucketPrice (price q), Vol 0.0011)) . safeHead . bids
  where
    bucketPrice (Price p) =
        let p' = {- (*50) . (`div` 50) . -} floor $ p
         in Price (realToFrac p')

-----------
defineAskTarget :: (Coin p, Coin v) => Vol v -> Vol v -> QuoteBook p v q c -> (Price p, Vol v)
defineAskTarget slipVol placeVol = maybe (Price 0, Vol 0) (\p -> (bucketPrice p, placeVol)) . priceExtremum slipVol . fmap toPair . asks
  where
    toPair q = (price q, volume q)
    convertPriceType (Price p) = Price (realToFrac p)
    bucketPrice (Price p) =
        let p' = (*20) . (`div` 20) . ceiling $ p
         in Price (realToFrac p')



main :: IO ()
main = do

    args <- getArgs

    if length args /= 5 then
        error "Usage: frt maxExposure slipVol placeVol USDT-SellPrice USDT-BuyPrice"
    else
        return ()

    let maxExposure :: Vol BTC = Vol   $ realToFrac ((read $ args !! 0) :: Double )
        slipVol     :: Vol BTC = Vol   $ realToFrac ((read $ args !! 1) :: Double )
        placeVol    :: Vol BTC = Vol   $ realToFrac ((read $ args !! 2) :: Double )
        xSellRate :: Price BRL = Price $ realToFrac ((read $ args !! 3) :: Double )
        xBuyRate  :: Price BRL = Price $ realToFrac ((read $ args !! 4) :: Double )

    putStrLn $  "Max exposure:"   <> show maxExposure
             <> "\nSlippage vol:" <> show slipVol
             <> "\ntarget vol:"   <> show placeVol
             <> "\nUSDT sell rate:" <> show xSellRate
             <> "\nUSDT buy rate:"  <> show xBuyRate

    coinbeneConfig <- getCoinbeneConfig Verbose

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
    (producer1, executor1, terminator1) <- coinbeneInit 1000000 Verbose coinbeneConfig (Proxy :: Proxy IO) fire1
    (producer2, executor2, terminator2) <- coinbeneInit 1000000 Verbose coinbeneConfig (Proxy :: Proxy IO) fire2


    -- Build and start the strategy
    network <- compile $ do
        es1 <- fromHandlerSet handlers1
        es2 <- fromHandlerSet handlers2

        esAdvice <- mirrorStrategy2 xSellRate xBuyRate maxExposure (defineAskTarget slipVol placeVol) AskSide es1 es2

        let esAdv1 = fromJust <$> filterE isJust (fst <$> esAdvice)
        let esAdv2 = fromJust <$> filterE isJust (snd <$> esAdvice)

        reactimate $
            fmap (logAndQueue output1)
            (esAdv1 :: Event (StrategyAdvice (Action USD BTC)))

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
    catch keyboardWait
        (\e -> do
                let err = show (e :: IOException)
                if isEOFError e
                    then do
                        putStrLn ("Warning: isEOFError exception thrown on keyboard input. running forever. Ctrl-C to abort.")
                        forever $ threadDelay (24 * 60 * 60 * 1000000)
                    else
                        putStrLn ("Error: Aborting execution. Unknown exception thrown on keyboard input:" <> err )
        )

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

