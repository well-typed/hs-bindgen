module HsBindgen.Eff (
  -- * Eff
  Eff,
  Support,
  wrapEff,
  unwrapEff,
  runFoldIdentity,
  runFoldReader,
  runFoldState,
  assertEff,
) where

import Control.Monad.Reader (MonadReader, Reader, ReaderT (..))
import Control.Monad.State (MonadState (state), State)
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef)
import Data.Tuple (swap)

import HsBindgen.Errors
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  'Eff' monad

  We work mostly in @IO@, and limited by @MonadUnliftIO@,
  but sometimes we need more effects: @Eff@ wrapping @ReaderT r IO@
  pattern can mimick many other monads.

  NOTE: This is intentionally /not/ a member of 'MonadCatch'. We use 'Eff'
  primarily for folding, and exception handling in folding requires special
  care. See detailed discussion of 'foldWithHandler' in the @clang@ bindings.
-------------------------------------------------------------------------------}

newtype Eff m a = Eff {
      getEff :: ReaderT (Support m) IO a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    )

wrapEff :: (Support m -> IO a) -> Eff m a
wrapEff = Eff . ReaderT

unwrapEff :: Eff m a -> Support m -> IO a
unwrapEff = runReaderT . getEff

assertEff :: Bool -> String -> Eff m ()
assertEff True  _   = return ()
assertEff False msg = panicIO msg

-- | 'ReaderT' argument required to support @m@
type family Support (m :: Star -> Star) :: Star

--
-- 'Identity'
--

type instance Support Identity = ()

runFoldIdentity :: Eff Identity a -> IO a
runFoldIdentity = ($ ()) . unwrapEff

--
-- 'Reader'
--

type instance Support (Reader r) = r

deriving newtype instance MonadReader r (Eff (Reader r))

runFoldReader :: r -> Eff (Reader r) a -> IO a
runFoldReader env = ($ env) . unwrapEff

--
-- 'State'
--

type instance Support (State s) = IORef s

instance MonadState s (Eff (State s)) where
  state f = wrapEff $ \ref -> atomicModifyIORef ref (swap . f)

runFoldState :: s -> Eff (State s) a -> IO (a, s)
runFoldState s f = do
    ref <- newIORef s
    a   <- unwrapEff f ref
    (a,) <$> readIORef ref
