{-# LANGUAGE RecursiveDo #-}

module TradingFramework where

import System.IO                (hPutStr, stderr)
import Control.Exception.Base   (finally)

import Pipes.Concurrent

import Reactive.Banana
import Reactive.Banana.Frameworks

import MarketTypes
import MarketBasics
import Util

-- changing the names to better match the semantics
type HandlerSet = AddHandler
newHandlerSet   = newAddHandler
fromHandlerSet  = fromAddHandler
activate        = actuate

--------------------------------------------------------------------------------
--                      FRAMEWORK HELPER FUNCTIONS
--------------------------------------------------------------------------------
showReasoning :: StrategyAdvice -> IO ()
showReasoning (ToDo _ reasons) = putStrLn reasons

logAndExecute :: Output Action -> StrategyAdvice -> IO ()
logAndExecute output (ToDo actions reasons) = do
  hPutStr stderr reasons -- reasons must explicitly include '\n' if desired.
  sequence_ $ (atomically . send output) <$> actions

runExecutor :: Handler Action -> Input Action -> IO ()
runExecutor executor inputQueue =
  whileJustThenFinally_
    (atomically $ recv inputQueue)
    (putStrLn "Executor exiting!")
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

--------------------------------------------------------------------------------
--                     SIMPLE TRADING STRATEGIES
--------------------------------------------------------------------------------
showBook :: Show counter
         => MomentIO (Event (QuoteBook qtail counter))
         -> b
         -> c
         -> d
         -> (Event StrategyAdvice -> MomentIO ())
         -> MomentIO ()
showBook newBooks _ _ _ runOnOutputEvents = mdo
  eNewBook <- newBooks
  runOnOutputEvents (toAdvice <$> eNewBook)
  where
    toAdvice = \book -> ToDo [] (backtrackCursor $ showTopN 3 book)

--------------------------------------------------------------------------------
cancelAllLimitOrders :: a
                     -> Event OrderPlacement
                     -> c
                     -> d
                     -> (Event StrategyAdvice -> MomentIO ())
                     -> MomentIO ()
cancelAllLimitOrders _ ePlaced _ _ runOnOutputEvents = runOnOutputEvents (cancelLimitOrders ePlaced)

--------------------------------------------------------------------------------
-- | Places an order and then cancels it. Detects cancellation.
dumbStrategy :: Show b
             => Event (QuoteBook a b)
             -> Event OrderPlacement
             -> Event OrderCancellation
             -> Event OrderFill
             -> (Event StrategyAdvice -> MomentIO ())
             -> MomentIO ()
dumbStrategy eBooks ePlaced eCanceled eFills outputEvents = mdo
  let eAny         = onAny eBooks ePlaced eCanceled eFills
      forceCancel  = cancelLimitOrders ePlaced
      noticeCancel = const (ToDo [] "Detected cancellation!\n") <$> eCanceled

  placeOrder <- once (ToDo [NewLimitOrder Ask 99555 0.01] "Placing an ask!\n") eAny

  -- This is a next step, Monoid instance!
  outputEvents $ unionWith const (unionWith (error "Conflict!") placeOrder forceCancel) noticeCancel

--------------------------------------------------------------------------------
--                            COMBINATORS
--------------------------------------------------------------------------------

onAny :: Show b
       => Event (QuoteBook a b)
       -> Event OrderPlacement
       -> Event OrderCancellation
       -> Event OrderFill
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

-- | Outputs input value only once, upon the first event seen.
once :: a -> Event b -> MomentIO (Event a)
once val event = do
  e1 <- headE event
  return (const val <$> e1)

-- | Outputs only the first ocurrence of the event.
headE :: MonadMoment m => Event a -> m (Event a)
headE event = do
  notDoneYet <- stepper True (const False <$> event)
  return $ whenE notDoneYet event

-- | Issue cancellation for any limit order seen.
cancelLimitOrders :: Event OrderPlacement -> Event StrategyAdvice
cancelLimitOrders ePlaced =
  let getOrd (Placement o) = o
      toAdvice a = ToDo [a] ("Canceling placed limit order: " ++ show a ++ "\n")
   in (toAdvice . CancelLimitOrder . getOrderID) <$> filterE isLimitOrder (getOrd <$> ePlaced)
