{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Internal utilities for creating the bindings
module HsBindgen.Clang.Internal.Bindings (
    -- * Pointers
    IsPointer(..)
  , IsForeignPtr(..)
  , DeriveIsForeignPtr(..)
    -- * Dealing with return values
  , CallFailed(..)
  , callFailed
  , cToBool
  , ensure
  , ensureNotNull
  , checkNotNull
  , ensureNotInRange
  ) where

import Control.Exception
import Data.Coerce
import Data.Kind
import Data.Typeable
import Foreign
import Foreign.Concurrent qualified as Concurrent
import GHC.Stack

import HsBindgen.Patterns

{-------------------------------------------------------------------------------
  Pointers
-------------------------------------------------------------------------------}

class Show p => IsPointer p where
  mkNullPtr :: p
  isNullPtr :: p -> Bool
  freePtr   :: p -> IO ()

instance IsPointer (Ptr p) where
  mkNullPtr = nullPtr
  isNullPtr = (== nullPtr)
  freePtr   = free

{-------------------------------------------------------------------------------
  Foreign pointers
-------------------------------------------------------------------------------}

class IsForeignPtr p where
  type UnderlyingPtr p :: Type
  wrapForeignPtr :: UnderlyingPtr p -> IO p
  unwrapForeignPtr :: forall a. p -> (UnderlyingPtr p -> IO a) -> IO a

newtype DeriveIsForeignPtr u p = DeriveIsForeignPtr p

instance (Coercible p (ForeignPtr a), Coercible u (Ptr a))
      => IsForeignPtr (DeriveIsForeignPtr u p) where
  type UnderlyingPtr (DeriveIsForeignPtr u p) = u
  wrapForeignPtr ptr = do
      let ptr' :: Ptr a
          ptr' = coerce ptr
      coerce <$> Concurrent.newForeignPtr ptr' (free ptr')
  unwrapForeignPtr fptr k = do
      let fptr' :: ForeignPtr a
          fptr' = coerce fptr
      withForeignPtr fptr' $ k . coerce

{-------------------------------------------------------------------------------
  Dealing with return values
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
ensureNotNull :: (HasCallStack, IsPointer a, Typeable a) => IO a -> IO a
ensureNotNull = ensure (not . isNullPtr)

-- | If the result is 'nullPtr', return 'Nothing'
checkNotNull :: IsPointer a => IO a -> IO (Maybe a)
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
