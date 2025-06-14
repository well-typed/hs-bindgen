-- | Monad for parsing types
--
-- Intended for unqualified import (unless context is unambiguous).
--
-- > import HsBindgen.Frontend.Pass.Parse.Type.Monad (ParseType)
-- > import HsBindgen.Frontend.Pass.Parse.Type.Monad qualified as ParseType
module HsBindgen.Frontend.Pass.Parse.Type.Monad (
    ParseType -- opaque
  , run
    -- * Errors
  , ParseTypeException(..)
    -- * Utility: dispatching
  , dispatch
  , dispatchWithArg
  , dispatchDecl
  ) where

import Control.Exception
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Foreign.C

import Clang.Enum.Simple
import Clang.LowLevel.Core
import HsBindgen.Errors
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition

  Parsing declarations and parsing types have different requirements, so each
  gets its own monad.
-------------------------------------------------------------------------------}

newtype ParseType a = Wrap {
      unwrap :: IO a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadIO
    )

instance MonadError ParseTypeException ParseType where
  throwError err   = Wrap $ throwIO err
  f `catchError` h = Wrap $ catch (unwrap f) (unwrap . h)

run :: MonadIO m => ParseType a -> m a
run = liftIO . unwrap

{-------------------------------------------------------------------------------
  Errors

  Dealing with errors while parsing types is a bit tricky:

  * The clang API does not make it very easy to associate a type with a source
    location, so it's hard to produce a useful error message.
  * Parsing a type always happens whilst processing some enclosing declaration.
    When we encounter something unsupported (or unexpected) whilst parsing the
    type, we'll want to skip generating bindings for that declaration, but we do
    not have sufficient context here to do so.

  For both of these reasons we simply throw an exception here, and then /catch/
  that exception in 'foldDec'. This allows us to address both of these issues:
  a declaration has a clear source location, so we can generate a helpful error
  message, and we can skip the declaration we're currently processing.
-------------------------------------------------------------------------------}

data ParseTypeException =
    -- | We encountered an unexpected type kind
    --
    -- This is always a bug in hs-bindgen: if this kind of type is unsupported,
    -- we should explicitly check for it and throw an appropriate exception.
    --
    -- If this is a @Left@ value, it means that our @libclang@ bindings are
    -- incomplete.
    UnexpectedTypeKind (Either CInt CXTypeKind)

    -- | We encountered an unexpected type declaration
    --
    -- Similar comments apply as for 'UnexpectedTypeKind'.
  | UnexpectedTypeDecl (Either CInt CXCursorKind)

    -- | We do not support variadic (varargs) functions
  | UnsupportedVariadicFunction
  deriving stock (Show, Eq)

instance PrettyTrace ParseTypeException where
  prettyTrace = \case
    UnexpectedTypeKind (Right kind) -> concat [
        "Unexpected type kind "
      , show kind
      , ".\n"
      , pleaseReport
      ]
    UnexpectedTypeKind (Left i) -> concat [
        "Unknown type kind "
      , show i
      , ".\n"
      , pleaseReport
      ]
    UnexpectedTypeDecl (Right kind) -> concat [
        "Unexpected type declaration "
      , show kind
      , ".\n"
      , pleaseReport
      ]
    UnexpectedTypeDecl (Left i) -> concat [
        "Unknown type declaration "
      , show i
      , ".\n"
      , pleaseReport
      ]
    UnsupportedVariadicFunction -> concat [
        "Unsupported variadic (varargs) function."
      ]

-- | We use 'Error' for bugs, and 'Warning' for known-to-be-unsupported
--
-- This ensures that for declarations that use known-to-be-unsupported types,
-- we can just skip generating bindings for that declaration.
instance HasDefaultLogLevel ParseTypeException where
  getDefaultLogLevel = \case
    UnexpectedTypeKind{}        -> Error
    UnexpectedTypeDecl{}        -> Error
    UnsupportedVariadicFunction -> Warning

instance Exception ParseTypeException where
  displayException = prettyTrace

{-------------------------------------------------------------------------------
  Utility: dispatching based on the cursor kind
-------------------------------------------------------------------------------}

dispatch :: CXType -> (CXTypeKind -> ParseType a) -> ParseType a
dispatch curr k = do
    let mKind = fromSimpleEnum $ cxtKind curr
    case mKind of
      Right kind -> k kind
      Left  i    -> throwError $ UnexpectedTypeKind (Left i)

-- | Convenience wrapper that repeats the argument
dispatchWithArg ::
     CXType
  -> (CXTypeKind -> CXType -> ParseType a)
  -> ParseType a
dispatchWithArg x f = dispatch x $ \kind -> f kind x

dispatchDecl :: CXCursor -> (CXCursorKind -> ParseType b) -> ParseType b
dispatchDecl curr k = do
    mKind <- fromSimpleEnum <$> clang_getCursorKind curr
    case mKind of
      Right kind -> k kind
      Left  i    -> throwError $ UnexpectedTypeDecl (Left i)
