module HsBindgen.Frontend.Pass.Parse.Monad (
    -- * Definition
    M
  , ExtraOutput(..)
  , UnsupportedError(..)
  , runParseMonad
    -- * Functionaltiy
  , liftIO
  , modifyIncludeGraph
  , recordError
  , getTranslationUnit
  , recordMacroExpansionAt
  , checkHasMacroExpansion
  ) where

import Control.Monad.IO.Class
import Data.IORef
import Data.Set qualified as Set
import Optics

import Clang.HighLevel.Types
import Clang.LowLevel.Core
import HsBindgen.Eff
import HsBindgen.Frontend.Graph.Includes (IncludeGraph)
import HsBindgen.Frontend.Graph.Includes qualified as IncludeGraph
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition

  We are careful to distinguish between the state that the computation can
  depend on ('MacroExpansions') and the additional output that generate during
  parsing but that cannot otherwise affect the computation ('IncludeGraph').
-------------------------------------------------------------------------------}

-- | Monad used during folding
type M = Eff ParseMonad

data ParseMonad a

-- | Support for 'M' (internal type, not exported)
data ParseSupport = ParseSupport {
      parseEnv   :: CXTranslationUnit     -- ^ Reader
    , parseLog   :: IORef ExtraOutput     -- ^ Writer (ish)
    , parseState :: IORef (Set SingleLoc) -- ^ State
    }

data ExtraOutput = ExtraOutput{
      outputGraph  :: IncludeGraph
    , outputErrors :: [UnsupportedError]
    }

emptyExtraOutput :: ExtraOutput
emptyExtraOutput = ExtraOutput{
      outputGraph  = IncludeGraph.empty
    , outputErrors = []
    }

_outputGraph :: Lens' ExtraOutput IncludeGraph
_outputGraph = lens outputGraph (\o x -> o{outputGraph = x})

_outputErrors :: Lens' ExtraOutput [UnsupportedError]
_outputErrors = lens outputErrors (\o x -> o{outputErrors = x})

data UnsupportedError =
    -- | Struct with implicit fields
    --
    -- We record the name of the struct that has the implicit fields.
    UnsupportedImplicitFields DeclId
  deriving stock (Show)

type instance Support ParseMonad = ParseSupport

runParseMonad :: CXTranslationUnit -> M a -> IO (a, ExtraOutput)
runParseMonad unit f = do
    support <- ParseSupport unit
                 <$> newIORef emptyExtraOutput
                 <*> newIORef Set.empty
    result  <- unwrapEff f support
    (result,) <$> readIORef (parseLog support)

modifyIncludeGraph :: (IncludeGraph -> IncludeGraph) -> M ()
modifyIncludeGraph f = wrapEff $ \ParseSupport{parseLog} ->
    modifyIORef parseLog (over _outputGraph f)

recordError :: UnsupportedError -> M ()
recordError err = wrapEff $ \ParseSupport{parseLog} ->
    modifyIORef parseLog (over _outputErrors (err :))

getTranslationUnit :: M CXTranslationUnit
getTranslationUnit = wrapEff $ \ParseSupport{parseEnv} ->
    return parseEnv

recordMacroExpansionAt :: SingleLoc -> M ()
recordMacroExpansionAt loc = do
    wrapEff $ \ParseSupport{parseState} ->
      modifyIORef parseState $ Set.insert loc

checkHasMacroExpansion :: Range SingleLoc -> M Bool
checkHasMacroExpansion extent = do
    wrapEff $ \ParseSupport{parseState} ->
      aux extent <$> readIORef parseState
  where
    aux :: Range SingleLoc -> Set SingleLoc -> Bool
    aux range expansions = or [
        -- Do a quick O(log n) check first, for the common case that the macro
        -- is right at the start of the range. For example, this would capture
        -- cases such as
        --
        -- > #define T int
        -- >
        -- > struct ExampleStruct {
        -- >   T field;
        -- > };
        Set.member (rangeStart range) expansions

        -- If that fails, do a O(n) scan through all macro expansions
      , any (\e -> fromMaybe False (rangeContainsLoc range e)) expansions
      ]
