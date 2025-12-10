-- | Monad we use when translating the language-c AST to our AST
--
-- Intended for unqualified import.
module HsBindgen.Frontend.LanguageC.Monad (
    FromLanC(..)
  , ReparseEnv
  , Error(..)
  , runFromLanC
  , getReparseEnv
    -- * Throwing errors
  , unexpected
  , unsupported
  , nodeOmitted
  , unexpectedF
    -- * Util
  , repeatedly
  , optionally
  ) where

import Control.Monad.Except (ExceptT, MonadError (..))
import Control.Monad.Except qualified as Except
import Control.Monad.Reader (Reader)
import Control.Monad.Reader qualified as Reader
import Data.Foldable qualified as Foldable
import GHC.Stack
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Frontend.AST.Internal
import HsBindgen.Frontend.LanguageC.PartialAST (CName)
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Monad used to translate from the @language-c@ AST to our
--
-- The @p@ parameter indicates a @hs-bindgen@ pass; this will be instantiated
-- to @HandleMacros@, but we leave it polymorphic here to avoid unnecessary
-- mutual dependencies.
newtype FromLanC p a = WrapFromLanC {
      unwrapFromLanC :: ExceptT Error (Reader (ReparseEnv p)) a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadError Error
    )

data Error =
    -- | We encountered something unexpected in the AST from language-c
    --
    -- This indicates a bug or something not yet implemented.
    UpdateUnexpected CallStack String

    -- | We encountered something in the language-c AST we don't not support
    --
    -- It is useful to distinguish known-to-be-unsupported from
    -- unknown-to-be-supported ('UpdateUnexpected'): the latter indicates that
    -- it's simply something we haven't considered yet, the former is a
    -- conscious decision about features we currently don't want to support.
  | UpdateUnsupported String
  deriving stock (Show)

instance PrettyForTrace Error where
  prettyForTrace (UpdateUnexpected cs str) = PP.vsep [
        PP.hsep [
            "Encountered unexpected node in the language-c AST: "
          , PP.string str
          ]
      , PP.string (prettyCallStack cs)
      ]
  prettyForTrace (UpdateUnsupported err) =
        PP.hsep [
            "Unsupported: "
          , PP.string err
          ]

-- | Types in scope when reparsing a particular declaration
type ReparseEnv p = Map CName (Type p)

runFromLanC :: ReparseEnv p -> FromLanC p a -> Either Error a
runFromLanC typeEnv =
      flip Reader.runReader typeEnv
    . Except.runExceptT
    . unwrapFromLanC

getReparseEnv :: FromLanC p (ReparseEnv p)
getReparseEnv = WrapFromLanC Reader.ask

{-------------------------------------------------------------------------------
  Throwing errors
-------------------------------------------------------------------------------}

unexpected :: HasCallStack => String -> FromLanC p x
unexpected = throwError . UpdateUnexpected callStack

unsupported :: String -> FromLanC p x
unsupported = throwError . UpdateUnsupported

data NodeOmitted = NodeOmitted
  deriving (Show)

nodeOmitted :: Functor f => f a -> f NodeOmitted
nodeOmitted = fmap (const NodeOmitted)

unexpectedF ::
     (HasCallStack, Functor f, Show (f NodeOmitted))
  => f a -> FromLanC p x
unexpectedF = unexpected . show . nodeOmitted

{-------------------------------------------------------------------------------
  Utility
-------------------------------------------------------------------------------}

-- | Run updates in order
--
-- Given
--
-- > addSuffix :: Update Int String
-- > addSuffix n str = return $ str ++ "_" ++ show n
--
-- We have
--
-- >    runUpdateM Map.empty $ repeatedly addSuffix [1..5] "str"
-- > == Right "str_1_2_3_4_5"
repeatedly :: forall p t a b.
     Foldable t
  => (  a -> b -> FromLanC p b)
  -> (t a -> b -> FromLanC p b)
repeatedly f = go . Foldable.toList
  where
   --  go :: [a] -> b -> UpdateM b
    go []     b = return b
    go (a:as) b = f a b >>= \b' -> go as b'

optionally :: (a -> b -> FromLanC p b) -> Maybe a -> b -> FromLanC p b
optionally = repeatedly

