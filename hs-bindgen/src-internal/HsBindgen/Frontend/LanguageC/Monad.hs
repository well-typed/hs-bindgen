-- | Monad we use when translating the language-c AST to our AST
--
-- Intended for unqualified import.
module HsBindgen.Frontend.LanguageC.Monad (
    FromLanC(..)
  , ReparseEnv
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

import HsBindgen.Frontend.AST.Internal
import HsBindgen.Frontend.LanguageC.Error
import HsBindgen.Frontend.LanguageC.PartialAST (CName)
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Monad used to translate from the @language-c@ AST to our
--
-- The @p@ parameter indicates a @hs-bindgen@ pass; this will be instantiated
-- to @HandleMacros@, but we leave it polymorphic here to avoid unnecessary
-- mutual dependencies.
newtype FromLanC a = WrapFromLanC {
      unwrapFromLanC :: ExceptT Error (Reader ReparseEnv) a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadError Error
    )

-- | Types in scope when reparsing a particular declaration
type ReparseEnv = Map CName (Type HandleMacros)

runFromLanC :: ReparseEnv -> FromLanC a -> Either Error a
runFromLanC typeEnv =
      flip Reader.runReader typeEnv
    . Except.runExceptT
    . unwrapFromLanC

getReparseEnv :: FromLanC ReparseEnv
getReparseEnv = WrapFromLanC Reader.ask

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

