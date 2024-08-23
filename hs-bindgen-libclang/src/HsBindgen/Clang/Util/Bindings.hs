-- | Internal utilities for creating the bindings
module HsBindgen.Clang.Util.Bindings (
    -- * Memory management
    attachFinalizer
    -- * Checking return values
  , CallFailed(..)
  , ensure
  , ensureNotNull
  ) where

import Control.Exception
import Foreign
import Foreign.C
import Foreign.Concurrent qualified as Concurrent
import GHC.Stack

import HsBindgen.Patterns

{-------------------------------------------------------------------------------
  Memory management
-------------------------------------------------------------------------------}

attachFinalizer :: Ptr a -> IO (ForeignPtr a)
attachFinalizer ptr = Concurrent.newForeignPtr ptr $ free ptr

{-------------------------------------------------------------------------------
  Check return values
-------------------------------------------------------------------------------}

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
ensureNotNull :: HasCallStack => IO (Ptr a) -> IO (Ptr a)
ensureNotNull call = do
    ptr <- call
    if ptr == nullPtr then do
      stack <- collectBacktrace
      throwIO $ CallFailed stack
    else
      return ptr

