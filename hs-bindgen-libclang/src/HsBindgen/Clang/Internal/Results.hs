{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Utilities for checking the results of C functions
module HsBindgen.Clang.Internal.Results (
    -- * Failed calls
    CallFailed(..)
  , callFailed
    -- * Specific conditions
  , cToBool
  , ensure
  , ensureOn
  , ensureNotNull
  , checkNotNull
  , ensureNotInRange
  ) where

import Control.Exception
import Data.Typeable
import Foreign
import GHC.Stack

import HsBindgen.Patterns
import Data.Coerce

{-------------------------------------------------------------------------------
  Failed calls
-------------------------------------------------------------------------------}

-- | Call to @libclang@ failed
--
-- In @libclang@, being a C framework, errors are returned as values; in order
-- to ensure that we don't forget to check for these error values, we turn them
-- into 'CallFailed' exceptions.
data CallFailed result = CallFailed Backtrace result
  deriving stock (Show)
  deriving Exception via CollectedBacktrace (CallFailed result)

callFailed :: (Typeable result, Show result, HasCallStack) => result -> IO a
callFailed result = do
    stack <- collectBacktrace
    throwIO $ CallFailed stack result

{-------------------------------------------------------------------------------
  Specific conditions
-------------------------------------------------------------------------------}

cToBool :: (Num a, Eq a) => a -> Bool
cToBool 0 = False
cToBool _ = True

-- | Check result for error value
ensure :: (HasCallStack, Typeable a, Show a) => (a -> Bool) -> IO a -> IO a
ensure = ensureOn id

-- | Generalization of 'ensure' with an additional translation step
--
-- This is useful in cases where the value that should be included in the
-- exception should not be the original value but the translated one.
ensureOn :: (HasCallStack, Typeable b, Show b)
  => (a -> b)
  -> (b -> Bool)
  -> IO a -> IO a
ensureOn f p call = do
    x <- call
    if p (f x)
      then return x
      else callFailed (f x)

-- | Ensure that a function did not return 'nullPtr' (indicating error)
ensureNotNull ::
     (HasCallStack, Coercible a (Ptr x), Typeable a, Show a)
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
  Internal auxiliary
-------------------------------------------------------------------------------}

isNullPtr :: Coercible a (Ptr x) => a -> Bool
isNullPtr ptr = ptr' == nullPtr
  where
    ptr' :: Ptr x
    ptr' = coerce ptr