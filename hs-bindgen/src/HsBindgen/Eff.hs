module HsBindgen.Eff (
  -- * FoldM
  FoldM,
  runFoldIdentity,
  runFoldReader,
  runFoldState,
) where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Unlift
import Data.IORef
import Data.Tuple (swap)

import HsBindgen.Imports

{-------------------------------------------------------------------------------
  'FoldM' monad

  We are limited by the type of 'clang_visitChildren' to functions in @IO@,
  but we can mimick other monads through the @ReaderT IO@ pattern.
-------------------------------------------------------------------------------}

newtype FoldM m a = FoldM {
      getFoldM :: ReaderT (Support m) IO a
    }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

wrapFoldM :: (Support m -> IO a) -> FoldM m a
wrapFoldM = FoldM . ReaderT

unwrapFoldM :: FoldM m a -> Support m -> IO a
unwrapFoldM = runReaderT . getFoldM

-- | 'ReaderT' argument required to support @m@
type family Support (m :: Star -> Star) :: Star

--
-- 'Identity'
--

type instance Support Identity = ()

runFoldIdentity :: FoldM Identity a -> IO a
runFoldIdentity = ($ ()) . unwrapFoldM

--
-- 'Reader'
--

type instance Support (Reader r) = r

deriving newtype instance MonadReader r (FoldM (Reader r))

runFoldReader :: r -> FoldM (Reader r) a -> IO a
runFoldReader env = ($ env) . unwrapFoldM

--
-- 'State'
--

type instance Support (State s) = IORef s

instance MonadState s (FoldM (State s)) where
  state f = wrapFoldM $ \ref -> atomicModifyIORef ref (swap . f)

runFoldState :: s -> FoldM (State s) a -> IO (a, s)
runFoldState s f = do
    ref <- newIORef s
    a   <- unwrapFoldM f ref
    (a,) <$> readIORef ref

