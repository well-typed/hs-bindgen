{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Utilities for checking the results of C functions
module Clang.Internal.Results (
    -- * Failed calls
    CallFailed(..)
  , callFailed
  , callFailedShow
    -- * Specific conditions
  , cToBool
  , ensure
  , ensureOn
  , ensureNotNull
  , checkNotNull
  , ensureNotInRange
    -- * Auxiliary
  , isNullPtr
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Coerce
import Foreign
import GHC.Stack

import Clang.Backtrace
import Clang.Enum.Simple
import Clang.LowLevel.Core.Instances ()

{-------------------------------------------------------------------------------
  Failed calls
-------------------------------------------------------------------------------}

-- | Call to @libclang@ failed
--
-- In @libclang@, being a C framework, errors are returned as values; in order
-- to ensure that we don't forget to check for these error values, we turn them
-- into 'CallFailed' exceptions.
data CallFailed = CallFailed String Backtrace
  deriving stock (Show)
  deriving Exception via CollectedBacktrace CallFailed

callFailed :: (MonadIO m, HasCallStack) => String -> m a
callFailed hint = do
    stack <- collectBacktrace
    liftIO $ throwIO $ CallFailed hint stack

callFailedShow :: (MonadIO m, Show hint, HasCallStack) => hint -> m a
callFailedShow = callFailed . show

{-------------------------------------------------------------------------------
  Specific conditions
-------------------------------------------------------------------------------}

cToBool :: (Num a, Eq a) => a -> Bool
cToBool 0 = False
cToBool _ = True

-- | Check result for error value
ensure :: (HasCallStack, Show a) => (a -> Bool) -> IO a -> IO a
ensure = ensureOn id

-- | Generalization of 'ensure' with an additional translation step
--
-- This is useful in cases where the value that should be included in the
-- exception should not be the original value but the translated one.
ensureOn :: (HasCallStack, Show b)
  => (a -> b)
  -> (b -> Bool)
  -> IO a -> IO a
ensureOn f p call = do
    x <- call
    if p (f x)
      then return x
      else callFailedShow (f x)

-- | Ensure that a function did not return 'nullPtr' (indicating error)
ensureNotNull ::
     (HasCallStack, Coercible a (Ptr x), Show a)
  => IO a -> IO a
ensureNotNull = ensure (not . isNullPtr)

-- | If the result is 'nullPtr', return 'Nothing'
checkNotNull :: Coercible a (Ptr x) => IO a -> IO (Maybe a)
checkNotNull call = do
    ptr <- call
    return $ if isNullPtr ptr
               then Nothing
               else Just ptr

-- | Ensure that the result is not in the range of the specified enum
--
-- This is used for functions which return errors from a specified enum, such as
-- @clang_Type_getSizeOf@, which will return errors from the 'CXTypeLayoutError'
-- enum.
--
-- Intended for use with a type argument:
--
-- > ensureNotInRange @CXTypeLayoutError $
-- >   wrap_Type_getSizeOf typ'
ensureNotInRange :: forall hs a.
     (HasCallStack, Integral a, Show hs, IsSimpleEnum hs)
  => IO a -> IO a
ensureNotInRange = ensureOn conv (not . simpleEnumInRange)
  where
    conv :: a -> SimpleEnum hs
    conv = coerceSimpleEnum . fromIntegral

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

isNullPtr :: Coercible a (Ptr x) => a -> Bool
isNullPtr ptr = ptr' == nullPtr
  where
    ptr' :: Ptr x
    ptr' = coerce ptr
