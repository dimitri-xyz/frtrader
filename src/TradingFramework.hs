{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TradingFramework where

import System.IO                (hPutStr, hPutStrLn, stderr)
import Control.Exception.Base   (finally)
import Control.Monad            (void)
import Data.Maybe

import Pipes.Concurrent

import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.ByteString.Char8 as BS

import Market.Types
import Market.Util
import Razao.Util
import Combinator

-- changing the names to better match the semantics
type HandlerSet = AddHandler
newHandlerSet   = newAddHandler
fromHandlerSet  = fromAddHandler
activate        = actuate

--------------------------------------------------------------------------------
--                      FRAMEWORK HELPER FUNCTIONS
--------------------------------------------------------------------------------
showReasoning :: StrategyAdvice action -> IO ()
showReasoning (Advice (reasoning, _)) = putStrLn reasoning

logAndExecute :: Output actions -> StrategyAdvice actions -> IO ()
logAndExecute output (Advice (reasoning, actions)) = do
  -- reasons must explicitly include '\n' if desired.
  -- using bytestring to make output thread safe
  BS.hPutStr stderr (BS.pack reasoning) 
  sequence_ $ atomically . send output <$> actions

runExecutor :: Handler (Action p v) -> Input (Action p v) -> IO ()
runExecutor executor inputQueue =
  whileJustThenFinally_
    (atomically $ recv inputQueue)
    (hPutStrLn stderr "\nExecutor exiting!")
    executor

-- `finally` forces us to stick to the IO monad here when it really should be more general
whileJustThenFinally_ :: IO (Maybe a) -> IO c -> (a -> IO b) -> IO ()
whileJustThenFinally_ p endAction loopAction = finally go endAction
  where go = do
          x <- p
          case x of
            Nothing -> return ()
            Just x  -> do
              loopAction x
              go

splitEvents
    :: Event (TradingE p v q c)
    -> ( Event (OrderPlacement    p v)
       , Event (OrderCancellation    )
       , Event (OrderFill         p v)
       , Event (QuoteBook         p v q c)
       )
splitEvents es =
       ( toPlace  <$> filterE isPlace  es
       , toCancel <$> filterE isCancel es
       , toFill   <$> filterE isFill   es
       , toBook   <$> filterE isBook   es
       )
  where
    isPlace TP{}  = True
    isPlace _     = False

    isCancel TC{} = True
    isCancel _    = False

    isFill TF{}   = True
    isFill _      = False

    isBook TB{}   = True
    isBook _      = False

--------------------------------------------------------------------------------
--                     SIMPLE TRADING STRATEGIES
--------------------------------------------------------------------------------
showBook :: (Coin p, Coin v, Show counter, MonadMoment m)
         => place -> cancel -> fill -> Event (QuoteBook p v qtail counter)
         -> m (Event (StrategyAdvice action))
showBook _ _ _ eNewBook = return (toAdvice <$> eNewBook)
  where
    toAdvice book = Advice (backtrackCursor $ showTopN 3 book, ZipList [])

--------------------------------------------------------------------------------
showAllBooks
    :: ( Coin p1, Coin v1, Show c1, Num c1
       , Coin p2, Coin v2, Show c2, Num c2
       , Coin p3, Coin v3, Show c3, Num c3
       , MonadMoment m)
    =>  Event (TradingE p1 v1 q1 c1)
    ->  Event (TradingE p2 v2 q2 c2)
    ->  Event (TradingE p3 v3 q3 c3)
    -> m (Event (StrategyAdvice a1), Event (StrategyAdvice a2), Event (StrategyAdvice a3))
showAllBooks e1s e2s e3s = do
    let (_, _, _, eb1s) = splitEvents e1s
        (_, _, _, eb2s) = splitEvents e2s
        (_, _, _, eb3s) = splitEvents e3s

    b1 <- accumB (QuoteBook {bids = [], asks = [], counter = 0}) (const <$> eb1s)
    b2 <- accumB (QuoteBook {bids = [], asks = [], counter = 0}) (const <$> eb2s)
    b3 <- accumB (QuoteBook {bids = [], asks = [], counter = 0}) (const <$> eb3s)

    return ( (\y z x -> toAdvice x y z) <$> b2 <*> b3 <@> eb1s   -- only fires on eb1s
           , never  -- (\z x y -> toAdvice x y z) <$> b3 <*> b1 <@> eb2s   -- only fires on eb2s
           , never) -- (\x y z -> toAdvice x y z) <$> b1 <*> b2 <@> eb3s)  -- only fires on eb3s

  where
    toAdvice bk1 bk2 bk3 =
      Advice (backtrackCursor $ replicate 50 '#' ++ "\n"
                              ++ showTopN 3 bk1
                              ++ replicate 50 '#' ++ "\n"
                              ++ showTopN 3 bk2
                              ++ replicate 50 '#' ++ "\n"
                              ++ showTopN 3 bk3
                              ++ replicate 50 '#' ++ "\n"
                              ++ "Asks: " ++ show a1 ++ " - " ++ show a2 ++ " - " ++ show a3 ++ "\n"
                              ++ "Bids: " ++ show b1 ++ " - " ++ show b2 ++ " - " ++ show b3 ++ "\n"
                              ++ replicate 50 '#' ++ "\n"
                              ++ "as':  " ++ show da1 ++ " - " ++ show da2 ++ " - " ++ show da3 ++ " (lower is better)\n"
                              ++ "bs':  " ++ show db1 ++ " - " ++ show db2 ++ " - " ++ show db3 ++ " (higher is better)\n"
             , ZipList []
             )
      where
        a1 = best 99999 (asks bk1)
        a2 = best 99999 (asks bk2)
        a3 = best 99999 (asks bk3)
        b1 = best     0 (bids bk1)
        b2 = best     0 (bids bk2)
        b3 = best     0 (bids bk3)
        da1 = Price $ round2dp ((realToFrac a2 * 1.0025 * 1.0025 / realToFrac b3)   :: USD )
        db1 = Price $ round2dp ((realToFrac b2 / realToFrac a3 / (1.0025 * 1.0025)) :: USD )
        da2 = Price $ round2dp ((realToFrac a3 * 1.0025 * 1.0025 * realToFrac a1)   :: USD )
        db2 = Price $ round2dp ((realToFrac b3 * realToFrac b1 / (1.0025 * 1.0025)) :: USD )
        da3 = Price            ((realToFrac a2 * 1.0025 * 1.0025 / realToFrac b1)   :: BTC )
        db3 = Price            ((realToFrac b2 / realToFrac a1 / (1.0025 * 1.0025)) :: BTC )


    best :: (Coin p, Coin v) => Price p -> [Quote p v q] -> Price p
    best p qs = fromMaybe p (getBestPrice' qs)

--------------------------------------------------------------------------------
cancelAllLimitOrders
    :: (MonadMoment m, Coin p, Coin v)
    => Event (TradingE p v q c)
    -> m (Event (StrategyAdvice (Action p v)))
cancelAllLimitOrders es =
  let (ep, _, _, _) = splitEvents es
   in return (cancelLimitOrders ep)

-- | Issue cancellation for any limit order seen.
cancelLimitOrders :: (Coin p, Coin v) => Event (OrderPlacement p v) -> Event (StrategyAdvice (Action p v))
cancelLimitOrders ePlaced =
  let getOrd (Placement o) = o
      toAdvice a = Advice ("Canceling placed limit order: " ++ show a ++ "\n", ZipList [a])
   in toAdvice . CancelLimitOrder . getOrderID <$> filterE isLimitOrder (getOrd <$> ePlaced)

--------------------------------------------------------------------------------
-- | Places an order and then cancels it. Detects a cancellation, not necessarily its own.
dumbStrategy :: forall m p v q c.
    (MonadMoment m, Coin p, Coin v)
    => Event (TradingE p v q c)
    -> m (Event (StrategyAdvice (Action p v)))
dumbStrategy es = mdo
  let eAny           = void es
      (eP, eC, _, _) = splitEvents  es
      forceCancel    = cancelLimitOrders eP
      noticeCancel   = const (Advice ("Detected a cancellation!\n", ZipList [])) <$> eC
      strat          = Advice ("Placing an ask!\n", ZipList [NewLimitOrder Ask 99555 0.01 Nothing])

  placeOrder <- once strat eAny
  return $ unionWith const (unionWith (error "Conflict!") placeOrder forceCancel) noticeCancel
--------------------------------------------------------------------------------
--                            COMBINATORS
--------------------------------------------------------------------------------

onAny :: (Coin p, Coin v)
       => Event (OrderPlacement    p v)
       -> Event (OrderCancellation    )
       -> Event (OrderFill         p v)
       -> Event (QuoteBook         p v q c)
       -> Event ()
onAny eNewBook eNewPlacement eNewCancels eNewFills =
  let eB = void eNewBook
      eP = void eNewPlacement
      eC = void eNewCancels
      eF = void eNewFills
      eAny = unionWith const
                (unionWith const eF eC)
                (unionWith const eP eB)
   in eAny

