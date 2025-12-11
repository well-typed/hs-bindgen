-- | Monad for parsing types
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
    -- * Errors
  , ParseTypeExceptionInContext(..)
  , ParseTypeException(..)
    -- * Utility: dispatching
  , dispatch
  , dispatchWithArg
  , dispatchDecl
  ) where

import Control.Exception
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Data (Typeable)
import Data.Map.Strict qualified as Map

import Clang.Enum.Simple
import Clang.LowLevel.Core

import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Definition

  Parsing declarations and parsing types have different requirements, so each
  gets its own monad.

  The ParseType monad includes state for caching typedef resolution to avoid
  redundant type parsing during typedef chain resolution. The cache maps typedef
  names to their resolved types.
-------------------------------------------------------------------------------}

newtype ParseType a = Wrap {
      unwrap :: StateT (Map C.DeclName (C.Type Parse)) IO a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadIO
    )

instance MonadError ParseTypeException ParseType where
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
lookupCache :: C.DeclName -> ParseType (Maybe (C.Type Parse))
lookupCache name = Wrap $ Map.lookup name <$> get

-- | Insert a typedef into the cache
insertCache :: C.DeclName -> C.Type Parse -> ParseType ()
insertCache name ty = Wrap $ modify' (Map.insert name ty)

{-------------------------------------------------------------------------------
  Errors

  Dealing with errors while parsing types is a bit tricky:

  * The clang API does not make it very easy to associate a type with a source
    location, so it's hard to produce a useful error message.

  * Parsing a type always happens whilst processing some enclosing declaration.
    When we encounter something unsupported (or unexpected) whilst parsing the
    type, we'll want to register a parse failure and avoid generating bindings
    for that declaration, but we do not have sufficient context here to do so.

  For both of these reasons we simply throw an exception here, and then /catch/
  that exception in 'foldDec'. This allows us to address both of these issues:
  a declaration has a clear source location, so we can generate a helpful error
  message, and we can skip the declaration we're currently processing.
-------------------------------------------------------------------------------}

data ParseTypeExceptionInContext ctx =
  ParseTypeExceptionInContext {
    parseContext   :: ctx
  , parseException :: ParseTypeException
  }
  deriving stock (Show, Eq, Ord, Generic)

instance (Show ctx, Typeable ctx) => Exception (ParseTypeExceptionInContext ctx)


{-------------------------------------------------------------------------------
  Utility: dispatching based on the cursor kind
-------------------------------------------------------------------------------}

dispatch ::
     MonadError ParseTypeException m
  => CXType -> (CXTypeKind -> m a) -> m a
dispatch curr k = do
    let mKind = fromSimpleEnum $ cxtKind curr
    case mKind of
      Right kind -> k kind
      Left  i    -> throwError $ UnexpectedTypeKind (Left i)

dispatchWithArg ::
     MonadError ParseTypeException m
  => CXType
  -> (CXTypeKind -> CXType -> m a)
  -> m a
dispatchWithArg x f = dispatch x $ \kind -> f kind x

dispatchDecl ::
     ( MonadIO m
     , MonadError ParseTypeException m
     )
  => CXCursor -> (CXCursorKind -> m b) -> m b
dispatchDecl curr k = do
    mKind <- fromSimpleEnum <$> clang_getCursorKind curr
    case mKind of
      Right kind -> k kind
      Left  i    -> throwError $ UnexpectedTypeDecl (Left i)
