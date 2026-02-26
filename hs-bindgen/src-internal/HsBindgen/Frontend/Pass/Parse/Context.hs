module HsBindgen.Frontend.Pass.Parse.Context (
    -- * Declaration and parse contexts
    DeclCtx(..)
  , ParseCtx -- opaque
  , mkCtx
  , pushCtx

    -- * Errors
  , ExceptionInCtx(..)
  , addCtxHandler
  ) where

import Control.Exception (Exception (..), SomeException)
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Typeable (Typeable)
import GHC.Records (HasField (..))

import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Errors

  Dealing with errors while parsing is tricky:

  * The Clang API does not make it very easy to associate an error with a source
    location, so it's hard to produce a useful error message.

  * Parsing always happens whilst processing some enclosing declaration. When we
    encounter something unsupported (or unexpected), we'll want to register a
    parse failure and avoid generating bindings for that declaration, but we do
    not have sufficient context here to do so.

  For both of these reasons we simply throw an exception here, and then /catch/
  that exception in 'foldDec'. This allows us to address both of these issues:
  a declaration has a clear source location, so we can generate a helpful error
  message, and we can skip the declaration we're currently processing.
-------------------------------------------------------------------------------}

data DeclCtx = DeclCtx {
     kind    :: CNameKind
   , scoping :: RequiredForScoping
   }
  deriving stock (Show, Eq, Ord)

-- | The parse context stores information about the inner-most declaration
--   (i.e., the one we are parsing right now), and the outer-most declaration
--   (i.e., the top-level declaration).
data ParseCtx = ParseCtx {
    _ctxs  :: NonEmpty DeclCtx
  , _outer :: DeclCtx

}
  deriving stock (Show, Eq, Ord)

mkCtx :: DeclCtx -> ParseCtx
mkCtx ctx = ParseCtx{
      _ctxs  = NonEmpty.singleton ctx
    , _outer = ctx
    }

pushCtx :: DeclCtx -> ParseCtx -> ParseCtx
pushCtx ctx e = ParseCtx{
      _ctxs = ctx <| e._ctxs
    , _outer = e._outer
    }

instance HasField "inner" ParseCtx DeclCtx where
  getField = NonEmpty.head . (._ctxs)

instance HasField "outer" ParseCtx DeclCtx where
  getField  = (._outer)

{-------------------------------------------------------------------------------
  Exception in context
-------------------------------------------------------------------------------}

data ExceptionInCtx e = ExceptionInCtx {
      exception :: e
    , ctx       :: DeclCtx
    }
  deriving stock (Show, Eq, Ord, Generic)

instance (Show e, Typeable e) => Exception (ExceptionInCtx e)

addCtxHandler ::
     forall e a. (Exception e)
  => Proxy e -> DeclCtx -> SomeException -> IO a
addCtxHandler _p c e
  | Just e' <- (fromException @e e) =
      throwIO (ExceptionInCtx e' c)
  | otherwise = throwIO e
