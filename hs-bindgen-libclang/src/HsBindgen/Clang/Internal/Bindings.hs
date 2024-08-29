-- | Internal utilities for creating the bindings
module HsBindgen.Clang.Internal.Bindings (
    -- * Pointers
    IsPointer(..)
  , IsForeignPtr(..)
  , DeriveIsForeignPtr(..)
    -- * Dealing with return values
  , CallFailed(..)
  , cToBool
  , ensure
  , ensureNotNull
  ) where

import Control.Exception
import Data.Kind
import Foreign
import Foreign.C
import Foreign.Concurrent qualified as Concurrent
import GHC.Stack

import HsBindgen.Patterns
import Data.Coerce

{-------------------------------------------------------------------------------
  Pointers
-------------------------------------------------------------------------------}

class IsPointer p where
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

cToBool :: CUInt -> Bool
cToBool 0 = False
cToBool _ = True

-- | Check that an (integral) result from @libclang@ function is not an error
ensure ::
     ( HasCallStack
     , IsSimpleEnum hs
     , Exception e
     , Integral c
     )
  => (c -> Bool)
  -> (Backtrace -> CInt -> Maybe hs -> e)
  -> IO c -> IO c
ensure p mkErr call = do
    c <- call
    if p c then
      return c
    else do
      stack <- collectBacktrace
      throwIO $ mkErr stack (fromIntegral c) (simpleFromC $ fromIntegral c)

data CallFailed = CallFailed Backtrace
  deriving stock (Show)
  deriving Exception via CollectedBacktrace CallFailed

-- | Ensure that a function did not return 'nullPtr' (indicating error)
--
-- Throws 'CallFailed' on 'nullPtr'.
ensureNotNull :: (HasCallStack, IsPointer a) => IO a -> IO a
ensureNotNull call = do
    ptr <- call
    if isNullPtr ptr then do
      stack <- collectBacktrace
      throwIO $ CallFailed stack
    else
      return ptr
