{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Proxy
import Reactive.Banana

import Test.Tasty
import Test.Tasty.HUnit

import Market.Interface
import Trading.Strategy

import Market.Coins (BTC(..), USD(..))

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ tests (Proxy :: Proxy (Price BTC)) (Proxy :: Proxy (Vol USD))
--------------------------------------------------------------------------------

tests :: forall p v q c. (Coin p, Coin v) => Proxy (Price p) -> Proxy (Vol v) -> TestTree
tests _ _ = testGroup "Trader Tests"
    [ testCase "TO DO!" $ do
        assertEqual "some text" 1 1

    ]

removeReasoning :: StrategyAdvice a -> StrategyAdvice a
removeReasoning (Advice (r, a)) = Advice ("", a)

removeComments
    :: ( Maybe (StrategyAdvice (Action p v)), Maybe (StrategyAdvice (Action p v)))
    -> ( Maybe (StrategyAdvice (Action p v)), Maybe (StrategyAdvice (Action p v)))
removeComments (a,b) = (fmap removeReasoning a, fmap removeReasoning b)

--------------------------------------------------------------------------------
