-- | Monad we use when translating the language-c AST to our AST
--
-- Intended for unqualified import.
module HsBindgen.Frontend.LanguageC.Monad (
    FromLanC(..)
  , ReparseEnv(..)
  , runFromLanC
  , getReparseEnv
  , getKnownTypes
  , lookupType
    -- * Throwing errors
  , unexpected
  , unsupported
  , nodeOmitted
  , unexpectedF
    -- * Util
  , repeatedly
  , optionally
  ) where

import Control.Applicative
import Control.Monad.Except (ExceptT, MonadError (..))
import Control.Monad.Except qualified as Except
import Control.Monad.Reader (Reader)
import Control.Monad.Reader qualified as Reader
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import GHC.Stack

import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.LanguageC.Error
import HsBindgen.Frontend.LanguageC.PartialAST (CName)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Monad used to translate from the @language-c@ AST to our AST
--
-- The @p@ parameter indicates an @hs-bindgen@ pass; this will be instantiated
-- to @ReparseMacroExpansions@. We leave it polymorphic here to avoid
-- unnecessary mutual dependencies.
newtype FromLanC a = WrapFromLanC (
      ExceptT Error (Reader ReparseEnv) a
    )
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadError Error
    )

-- | Types in scope when reparsing a particular declaration
data ReparseEnv = ReparseEnv {
    -- | Known @typedef@s
    knownTypes      :: Map CName (C.Type ReparseMacroExpansions)
    -- | Known type-like macros
    --
    -- We store macros in a separate field, because in the future we may not
    -- translate macros to their full C types anymore.
    --
    -- Furthermore, we only add the macros /expanded in the reparsed
    -- declaration/ to the environment.
    --
    -- At the moment, we translate to C.Type using @addNewMacroTypeToReparseEnv@
    -- in @TypecheckMacros@.
  , knownMacroTypes :: Map CName (C.Type ReparseMacroExpansions)
  }
  deriving (Show, Eq)

runFromLanC :: ReparseEnv -> FromLanC a -> Either Error a
runFromLanC typeEnv (WrapFromLanC ma) =
      flip Reader.runReader typeEnv $
        Except.runExceptT ma

getReparseEnv :: FromLanC ReparseEnv
getReparseEnv = WrapFromLanC Reader.ask

getKnownTypes :: ReparseEnv -> Set CName
getKnownTypes env = Map.keysSet env.knownTypes <> Map.keysSet env.knownMacroTypes

lookupType :: CName -> ReparseEnv -> Maybe (C.Type ReparseMacroExpansions)
lookupType nm env =
    -- Macro types take priority: a typedef and a type-like macro may share the
    -- same bare name (e.g. 'bool' from stdbool.h alongside a typedef 'bool').
    Map.lookup nm env.knownMacroTypes <|> Map.lookup nm env.knownTypes

{-------------------------------------------------------------------------------
  Throwing errors
-------------------------------------------------------------------------------}

unexpected :: HasCallStack => String -> FromLanC x
unexpected = throwError . UpdateUnexpected callStack

unsupported :: String -> FromLanC x
unsupported = throwError . UpdateUnsupported

data NodeOmitted = NodeOmitted
  deriving (Show)

nodeOmitted :: Functor f => f a -> f NodeOmitted
nodeOmitted = fmap (const NodeOmitted)

unexpectedF ::
     (HasCallStack, Functor f, Show (f NodeOmitted))
  => f a -> FromLanC x
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
repeatedly :: forall t a b.
     Foldable t
  => (  a -> b -> FromLanC b)
  -> (t a -> b -> FromLanC b)
repeatedly f = go . Foldable.toList
  where
   --  go :: [a] -> b -> UpdateM b
    go []     b = return b
    go (a:as) b = f a b >>= \b' -> go as b'

optionally :: (a -> b -> FromLanC b) -> Maybe a -> b -> FromLanC b
optionally = repeatedly

