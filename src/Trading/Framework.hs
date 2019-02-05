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

import Market.Interface (Action(..), StrategyAdvice(..))

--------------------------------------------------------------------------------
--                      FRAMEWORK HELPER FUNCTIONS
--------------------------------------------------------------------------------
showReasoning :: StrategyAdvice action -> IO ()
showReasoning (Advice (reasoning, _)) = putStrLn reasoning

logAndQueue :: Output actions -> StrategyAdvice actions -> IO ()
logAndQueue output (Advice (reasoning, actions)) = do
    -- reasons must explicitly include '\n' if desired.
    -- using bytestring to make output thread safe
    BS.hPutStr stderr (BS.pack reasoning) 
    sequence_ $ atomically . send output <$> actions

runExecutor :: Input (Action p v) -> Handler (Action p v) -> IO () -> IO ()
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
