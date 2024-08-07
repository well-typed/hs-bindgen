module HsBindgen.Patterns.SafeForeignPtr (
    SafeForeignPtr -- opaque
  , AccessedFinalizedForeignPtrException
    -- * API
  , newSafeForeignPtr
  , withSafeForeignPtr
  , finalizeSafeForeignPtr
  ) where

import Control.Exception
import Control.Monad
import Data.IORef
import Foreign
import Foreign.Concurrent qualified as Concurrent
import GHC.Stack

import HsBindgen.Patterns.Stack

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Like 'ForeignPtr', but we can detect when the pointer has been freed
--
-- This is only useful when 'finalizeSafeForeignPtr' is explicitly called;
-- otherwise (barring bugs) it should never be possible for a foreign pointer
-- to be deallocated when it's still referenced somewhere.
data SafeForeignPtr a = Wrap {
      unwrap      :: IORef (State a)
    , allocatedAt :: Stack
    }

-- | State of the 'SafeForeignPtr'
--
-- This is an internal API.
data State a =
    Allocated (ForeignPtr a)
  | GarbageCollected
  | ExplicitlyFreedAt Stack

data AccessedFinalizedForeignPtrException =
    -- | An attempt was made to access a 'SafeForeignPtr' that was finalized
    --
    -- We record where the 'SafeForeignPtr' was allocated, where it was freed
    -- if freed explicitly (this will be 'Nothing' if it was GCed), and where
    -- the invalid access happened.
    AccessedFinalizedForeignPtr {
        allocated :: Stack
      , freed     :: Maybe Stack
      , accessed  :: Stack
      }
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Main API
-------------------------------------------------------------------------------}

-- | Construct 'SafeForeignPtr' from plain memory reference and a finalizer
--
-- See 'newForeignPtr' for additional discussion.
newSafeForeignPtr :: HasCallStack => Ptr a -> IO () -> IO (SafeForeignPtr a)
newSafeForeignPtr ptr finalizer = do
    stack <- getStack
    ref   <- newIORef undefined

    let finalizer' :: IO ()
        finalizer' = writeIORef ref GarbageCollected >> finalizer

    foreignPtr <- Concurrent.newForeignPtr ptr finalizer'
    writeIORef ref $ Allocated foreignPtr
    return $ Wrap ref stack

-- | Access the pointer
--
-- Same provisos as for 'withForeignPtr' apply.
withSafeForeignPtr :: SafeForeignPtr a -> (Ptr a -> IO b) -> IO b
withSafeForeignPtr safePtr k = do
    mForeign <- readIORef (unwrap safePtr)
    case mForeign of
      Allocated fptr -> do
        withForeignPtr fptr k
      GarbageCollected -> do
        stack <- getStack
        throwIO $ AccessedFinalizedForeignPtr {
          allocated = allocatedAt safePtr
        , freed     = Nothing
        , accessed  = stack
        }
      ExplicitlyFreedAt freedAt -> do
        stack <- getStack
        throwIO $ AccessedFinalizedForeignPtr {
            allocated = allocatedAt safePtr
          , freed     = Just freedAt
          , accessed  = stack
          }

-- | Finalize the pointer
--
-- This is a no-op if the pointer has already been finalized.
--
-- See also 'finalizeForeignPtr'.
finalizeSafeForeignPtr :: SafeForeignPtr a -> IO ()
finalizeSafeForeignPtr safePtr = do
    stack    <- getStack
    mForeign <- atomicModifyIORef (unwrap safePtr) $ \st ->
     case st of
      Allocated fptr -> (ExplicitlyFreedAt stack, Just fptr)
      _otherwise     -> (st, Nothing)
    forM_ mForeign finalizeForeignPtr
