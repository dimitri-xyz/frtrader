{-# LANGUAGE ScopedTypeVariables #-}

module Trading.Framework where

import System.IO                (hPutStr, hPutStrLn, stderr)
import Control.Exception.Base   (finally)
import Control.Monad            (void)
import Data.Maybe

import Pipes.Concurrent

import Reactive.Banana
import Reactive.Banana.Frameworks.Extended
import qualified Data.ByteString.Char8 as BS

import Market.Interface (Action(..), StrategyAdvice(..), ControlAction(..))

----------------------------------------
ctrlExecutor :: STM () -> Handler ControlAction
ctrlExecutor _    (Error code msg)    = return () -- Do nothing. Just logging these for now.
ctrlExecutor stop (ShutdownDone code) = logger "ShutdownDone received.\n" >> atomically stop

ctrlFinalizer :: IO ()
ctrlFinalizer = logger "Control Executor Exiting! This should only happen after receiving `ShutdownDone` from strategy.\n"
--------------------------------------------------------------------------------
--                      FRAMEWORK HELPER FUNCTIONS
--------------------------------------------------------------------------------

showReason :: StrategyAdvice action -> String
showReason (Advice (reasoning, _)) = reasoning

runExecutor :: Input a -> Handler a -> IO () -> IO ()
runExecutor inputQueue executor finalizer =
    whileJustThenFinally_ (atomically $ recv inputQueue) executor finalizer

-- `finally` forces us to stick to the IO monad here when it really should be more general
whileJustThenFinally_ :: IO (Maybe a) -> (a -> IO b) -> IO c -> IO ()
whileJustThenFinally_ p loopAction endAction = finally go endAction
  where
    go = do
        x <- p
        case x of
            Nothing -> return ()
            Just x  -> do
                loopAction x
                go

queue :: Output a -> a -> IO Bool
queue output action = atomically (send output action)

queueMany :: Traversable t => Output a -> t a -> IO ()
queueMany output actions = mapM_ (queue output) actions

-- explicitly include '\n' if desired.
-- using bytestring to make output thread safe
logger :: String -> IO ()
logger message = BS.hPutStr stderr (BS.pack message)

logAndQueueAdvice :: Output actions -> StrategyAdvice actions -> IO ()
logAndQueueAdvice output (Advice (reasoning, actions)) = do
    logger reasoning
    queueMany output actions

logAndQueueControl :: Show action => Output action -> action -> IO ()
logAndQueueControl output ctrAction = do
    logger $ "CONTROL ACTION: " <> show ctrAction <> "\n"
    queue output ctrAction
    return ()
