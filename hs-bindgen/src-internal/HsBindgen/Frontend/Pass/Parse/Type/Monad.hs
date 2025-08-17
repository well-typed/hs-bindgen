-- | Monad for parsing types
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
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint ((><))
import Text.SimplePrettyPrint qualified as PP

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

    -- | We do not support @long double@
  | UnsupportedLongDouble

    -- | Clang built-in declaration
  | UnsupportedBuiltin C.Name
  deriving stock (Show, Eq)

instance PrettyForTrace ParseTypeException where
  prettyForTrace = \case
      UnexpectedTypeKind (Right kind) ->
          unexpected $ "type kind " >< PP.showToCtxDoc kind
      UnexpectedTypeKind (Left i) ->
          unexpected $ "type kind " >< PP.showToCtxDoc i
      UnexpectedTypeDecl (Right kind) ->
          unexpected $ "type declaration " >< PP.showToCtxDoc kind
      UnexpectedTypeDecl (Left i) ->
          unexpected $ "type declaration " >< PP.showToCtxDoc i
      UnsupportedVariadicFunction ->
          "Unsupported variadic (varargs) function."
      UnsupportedLongDouble ->
          "Unsupported long double."
      UnsupportedBuiltin name ->
          "Unsupported built-in " >< prettyForTrace name
    where
      unexpected :: PP.CtxDoc -> PP.CtxDoc
      unexpected msg = PP.vcat [
            "Unexpected " >< msg  >< "."
          , PP.string pleaseReport
          ]

-- | We use 'Error' for bugs, and 'Warning' for known-to-be-unsupported
--
-- This ensures that for declarations that use known-to-be-unsupported types,
-- we can just skip generating bindings for that declaration.
instance IsTrace Level ParseTypeException where
  getDefaultLogLevel = \case
    UnexpectedTypeKind{}        -> Error
    UnexpectedTypeDecl{}        -> Error
    UnsupportedVariadicFunction -> Warning
    UnsupportedLongDouble       -> Warning
    UnsupportedBuiltin{}        -> Warning
  getSource = const HsBindgen

instance Exception ParseTypeException where
  displayException = PP.renderCtxDoc (PP.mkContext 100) . prettyForTrace

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
