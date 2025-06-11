{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Internal.Tasty
  ( assertException
  ) where

import Control.Exception (Exception (..), tryJust)
import Control.Monad (when)
import Data.Either (isRight)
import Data.Proxy (Proxy)
import Test.Tasty.HUnit (Assertion, assertFailure)

assertException :: forall e a. Exception e => String -> Proxy e -> IO a -> Assertion
assertException msg _ action = do
  result <- tryJust (\x -> fromException x :: Maybe e) action
  when (isRight result) $ assertFailure msg
