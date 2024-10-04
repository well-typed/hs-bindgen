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
    -- * Clang version error
  , ClangVersionRequirement(..)
  , ClangVersionError(..)
    -- * Invalid CXType error
  , InvalidCXTypeError(..)
  ) where

import Control.Exception
import Data.Coerce
import Data.Typeable
import Foreign
import GHC.Generics (Generic)
import GHC.Stack

import HsBindgen.Clang.Core.Instances ()
import HsBindgen.Patterns

{-------------------------------------------------------------------------------
  Failed calls
-------------------------------------------------------------------------------}

-- | Call to @libclang@ failed
--
-- In @libclang@, being a C framework, errors are returned as values; in order
-- to ensure that we don't forget to check for these error values, we turn them
-- into 'CallFailed' exceptions.
data CallFailed result = CallFailed result Backtrace
  deriving stock (Show)
  deriving Exception via CollectedBacktrace (CallFailed result)

callFailed :: (Typeable result, Show result, HasCallStack) => result -> IO a
callFailed result = do
    stack <- collectBacktrace
    throwIO $ CallFailed result stack

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
  Clang version error
-------------------------------------------------------------------------------}

-- | Clang version requirement
data ClangVersionRequirement =
    ClangTooOld     -- ^ Clang is too old (older than version 11)
  | Clang11Required -- ^ We need at least Clang version 11
  | Clang12Required -- ^ We need at least Clang version 12
  | Clang13Required -- ^ We need at least Clang version 13
  | Clang14Required -- ^ We need at least Clang version 14
  | Clang15Required -- ^ We need at least Clang version 15
  | Clang16Required -- ^ We need at least Clang version 16
  | Clang17Required -- ^ We need at least Clang version 17
  | Clang18Required -- ^ We need at least Clang version 18
  deriving stock (Show, Eq, Enum, Bounded, Generic)

-- | The version of Clang being used is not supported
data ClangVersionError = ClangVersionError ClangVersionRequirement Backtrace
  deriving stock (Show)
  deriving Exception via CollectedBacktrace ClangVersionError

{-------------------------------------------------------------------------------
  Invalid CXType error
-------------------------------------------------------------------------------}

-- | Haskell-side invalid type error
data InvalidCXTypeError = InvalidCXTypeError Backtrace
  deriving stock (Show)
  deriving Exception via CollectedBacktrace InvalidCXTypeError

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

isNullPtr :: Coercible a (Ptr x) => a -> Bool
isNullPtr ptr = ptr' == nullPtr
  where
    ptr' :: Ptr x
    ptr' = coerce ptr