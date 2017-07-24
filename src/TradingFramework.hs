{-# LANGUAGE RecursiveDo #-}

module TradingFramework where

import System.IO                (hPutStr, hPutStrLn, stderr)
import Control.Exception.Base   (finally)

import Pipes.Concurrent

import Reactive.Banana
import Reactive.Banana.Frameworks

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
showReasoning :: StrategyAdvice p v -> IO ()
showReasoning (ToDo _ reasons) = putStrLn reasons

logAndExecute :: Output (Action p v) -> StrategyAdvice p v -> IO ()
logAndExecute output (ToDo actions reasons) = do
  hPutStr stderr reasons -- reasons must explicitly include '\n' if desired.
  sequence_ $ (atomically . send output) <$> actions

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
showBook :: (Coin p, Coin v, Show counter)
         => place
         -> cancel
         -> fill
         -> MomentIO (Event (QuoteBook p v qtail counter))
         -> (Event (StrategyAdvice p v) -> MomentIO ())
         -> MomentIO ()
showBook _ _ _ newBooks runOnOutputEvents = mdo
    eNewBook <- newBooks
    runOnOutputEvents (toAdvice <$> eNewBook)
  where
    toAdvice = \book -> ToDo [] (backtrackCursor $ showTopN 3 book)

--------------------------------------------------------------------------------
cancelAllLimitOrders
    :: (Coin p, Coin v)
    => Event (TradingE p v q c)
    -> (Event (StrategyAdvice p v) -> MomentIO ())
    -> MomentIO ()
cancelAllLimitOrders es runOnOutputEvents =
  let (ep, _, _, _) = splitEvents es
   in runOnOutputEvents (cancelLimitOrders ep)

--------------------------------------------------------------------------------
-- | Places an order and then cancels it. Detects cancellation.
dumbStrategy :: (Coin p, Coin v)
             => Event (OrderPlacement    p v)
             -> Event (OrderCancellation    )
             -> Event (OrderFill         p v)
             -> Event (QuoteBook         p v q c)
             -> (Event (StrategyAdvice p v) -> MomentIO ())
             -> MomentIO ()
dumbStrategy ePlaced eCanceled eFills eBooks outputEvents = mdo
  let eAny         = onAny ePlaced eCanceled eFills eBooks
      forceCancel  = cancelLimitOrders ePlaced
      noticeCancel = const (ToDo [] "Detected cancellation!\n") <$> eCanceled

  placeOrder <- once (ToDo [NewLimitOrder Ask 99555 0.01] "Placing an ask!\n") eAny

  -- This is a next step, Monoid instance!
  outputEvents $ unionWith const (unionWith (error "Conflict!") placeOrder forceCancel) noticeCancel

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
  let eB = const () <$> eNewBook
      eP = const () <$> eNewPlacement
      eC = const () <$> eNewCancels
      eF = const () <$> eNewFills
      eAny = unionWith const
                (unionWith const eF eC)
                (unionWith const eP eB)
   in eAny

-- | Issue cancellation for any limit order seen.
cancelLimitOrders :: (Coin p, Coin v) => Event (OrderPlacement p v) -> Event (StrategyAdvice p v)
cancelLimitOrders ePlaced =
  let getOrd (Placement o) = o
      toAdvice a = ToDo [a] ("Canceling placed limit order: " ++ show a ++ "\n")
   in (toAdvice . CancelLimitOrder . getOrderID) <$> filterE isLimitOrder (getOrd <$> ePlaced)
