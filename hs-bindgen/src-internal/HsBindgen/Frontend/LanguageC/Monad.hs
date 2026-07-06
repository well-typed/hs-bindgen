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
  , skipped
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
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Stack

import HsBindgen.Frontend.LanguageC.Error
import HsBindgen.Frontend.LanguageC.PartialAST (CName)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.Intermediate.LanC.IsPass (LanC)
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Monad used to translate from the @language-c@ AST to our AST
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
    knownTypes      :: Map CName (C.Type LanC)
    -- | Known type-like macros
    --
    -- We store macros in a separate field, because we do not translate macros
    -- to their full C types.
    --
    -- Furthermore, we only add the macros /expanded in the reparsed
    -- declaration/ to the environment.
  , knownMacros :: Set CName
  }
  deriving (Show, Eq)

runFromLanC :: ReparseEnv -> FromLanC a -> Either Error a
runFromLanC typeEnv (WrapFromLanC ma) =
      flip Reader.runReader typeEnv $
        Except.runExceptT ma

getReparseEnv :: FromLanC ReparseEnv
getReparseEnv = WrapFromLanC Reader.ask

getKnownTypes :: ReparseEnv -> Set CName
getKnownTypes env = Map.keysSet env.knownTypes <> env.knownMacros

lookupType :: CName -> ReparseEnv -> Maybe (C.Type LanC)
lookupType nm env =
    case (Set.member nm env.knownMacros, Map.lookup nm env.knownTypes) of
      -- Macro types take priority: a typedef and a type-like macro may share the
      -- same bare name (e.g. 'bool' from stdbool.h alongside a typedef 'bool').
      --
      -- The underlying type '()' is a placeholder. The 'Zip' intermediate pass
      -- fills it in by consulting the annotation carrying information from
      -- pre-reparse 'PrepareReparse'.
      (True,  _)            -> Just (C.TypeMacro (C.MacroRef macroId ()))
      (False, mTypedefType) -> mTypedefType
  where
    macroId :: C.DeclId
    macroId = C.DeclId{
        name   = C.DeclName nm C.NameKindMacro
      , isAnon = False
      }

{-------------------------------------------------------------------------------
  Throwing errors
-------------------------------------------------------------------------------}

unexpected :: HasCallStack => String -> FromLanC x
unexpected = throwError . UpdateUnexpected callStack

unsupported :: String -> FromLanC x
unsupported = throwError . UpdateUnsupported

skipped :: String -> FromLanC x
skipped = throwError . UpdateSkipped

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

