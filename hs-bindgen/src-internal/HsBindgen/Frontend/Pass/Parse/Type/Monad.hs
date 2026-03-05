-- | Monad for parsing types
--
-- Intended for unqualified import (unless context is unambiguous).
--
-- > import HsBindgen.Frontend.Pass.Parse.Type.Monad (ParseType)
-- > import HsBindgen.Frontend.Pass.Parse.Type.Monad qualified as ParseType
module HsBindgen.Frontend.Pass.Parse.Type.Monad (
    ParseType -- opaque
  , run
    -- * Caching API
  , lookupCache
  , insertCache
  , cached
  , cachedMaybe
    -- * Utility: dispatching
  , dispatch
  , dispatchWithArg
  , dispatchDecl
  ) where

import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Map.Strict qualified as Map

import Clang.Enum.Simple
import Clang.LowLevel.Core

import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition

  Parsing declarations and parsing types have different requirements, so each
  gets its own monad.

  The ParseType monad includes state for caching typedef resolution to avoid
  redundant type parsing during typedef chain resolution. The cache maps typedef
  names to their resolved types.
-------------------------------------------------------------------------------}

newtype ParseType a = Wrap (
      StateT (Map CDeclName (C.Type Parse)) IO a
    )
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

unwrap :: ParseType a -> StateT (Map CDeclName (C.Type Parse)) IO a
unwrap (Wrap ma) = ma

instance MonadError ParseMsg ParseType where
  throwError err   = Wrap $ lift $ throwIO err
  f `catchError` h = Wrap
                   $ StateT
                   $ \s -> catch (runStateT (unwrap f) s)
                                 (\e -> runStateT (unwrap (h e)) s)

run :: MonadIO m => ParseType a -> m a
run = liftIO . flip evalStateT Map.empty . unwrap

{-------------------------------------------------------------------------------
  Caching API
-------------------------------------------------------------------------------}

-- | Look up a typedef in the cache
lookupCache :: CDeclName -> ParseType (Maybe (C.Type Parse))
lookupCache name = Wrap $ Map.lookup name <$> get

-- | Insert a typedef into the cache
insertCache :: CDeclName -> C.Type Parse -> ParseType ()
insertCache name ty = Wrap $ modify' (Map.insert name ty)

-- | Run a parse action with a cache.
--
-- On a cache hit, ignores the parse action and returns the cached value. On a
-- cache hit, runs the parse action, caches the result, and returns the result.
cached :: CDeclName -> ParseType (C.Type Parse) -> ParseType (C.Type Parse)
cached name k = do
    -- Check cache first
    mCachedValue <- lookupCache name
    case mCachedValue of
      -- Cache hit
      Just cachedValue -> pure cachedValue
      -- Cache miss
      Nothing -> do
        newValue <- k
        insertCache name newValue
        pure newValue

-- | Like 'cached', but only uses the cache if the name argument is 'Just'.
cachedMaybe :: Maybe CDeclName -> ParseType (C.Type Parse) -> ParseType (C.Type Parse)
cachedMaybe mName k =
    case mName of
      Nothing -> k
      Just name -> cached name k

{-------------------------------------------------------------------------------
  Utility: dispatching based on the cursor kind
-------------------------------------------------------------------------------}

dispatch ::
     MonadError ParseMsg m
  => CXType -> (CXTypeKind -> m a) -> m a
dispatch curr k = do
    let mKind = fromSimpleEnum $ cxtKind curr
    case mKind of
      Right kind -> k kind
      Left  i    -> throwError $ Immediate $ ParseUnexpectedTypeKind (Left i)

dispatchWithArg ::
     MonadError ParseMsg m
  => CXType
  -> (CXTypeKind -> CXType -> m a)
  -> m a
dispatchWithArg x f = dispatch x $ \kind -> f kind x

dispatchDecl ::
     ( MonadIO m
     , MonadError ParseMsg m
     )
  => CXCursor -> (CXCursorKind -> m b) -> m b
dispatchDecl curr k = do
    mKind <- fromSimpleEnum <$> clang_getCursorKind curr
    case mKind of
      Right kind -> k kind
      Left  i    -> throwError $ Immediate $ ParseUnexpectedCursorKind (Left i)
