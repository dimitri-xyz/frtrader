{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TradingFramework where

import System.IO                (hPutStr, hPutStrLn, stderr)
import Control.Exception.Base   (finally)
import Control.Monad            (void)
import Data.Maybe

import Pipes.Concurrent

import Reactive.Banana
import Reactive.Banana.Frameworks.Extended
import qualified Data.ByteString.Char8 as BS

import Market.Types
import Market.Util
import Razao.Util
import Combinator

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

