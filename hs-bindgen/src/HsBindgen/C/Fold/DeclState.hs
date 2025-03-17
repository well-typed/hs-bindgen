module HsBindgen.C.Fold.DeclState (
    DeclState(..)
  , TypeDecl (..)
    -- * Construction
  , initDeclState
  , registerMacroExpansion
  , registerMacroType
  , registerInclude
    -- * Query
  , containsMacroExpansion
  ) where

import Data.Map.Ordered.Strict qualified as OMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Data.DynGraph (DynGraph)
import Data.DynGraph qualified as DynGraph
import HsBindgen.C.AST (CName, Decl, DeclPath, Type)
import HsBindgen.C.Tc.Macro qualified as Macro
import HsBindgen.Clang.HighLevel.Types
import HsBindgen.Clang.LowLevel.Core
import HsBindgen.Clang.Paths
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Declaration state, maintained during AST construction
data DeclState = DeclState {
      -- | Macro expansions
      --
      -- Whenever @clang@ expands a macro, we get a (faux?) AST node of type
      -- 'CXCursor_MacroExpansion', which tells us /where/ a macro was
      -- expanded. We keep track of these, so that we know to look out for them;
      -- for example, it can alert us to the fact that a struct field has a
      -- type which is macro defined.
      macroExpansions  :: !(Set SingleLoc)
    , macroTypes       :: !Macro.TypeEnv
    -- | Type declarations
    --
    -- We accumulate type declarations in (insert)ordered map,
    -- so the ordering resembles the one in the header.
    , typeDeclarations :: !(OMap.OMap CXType TypeDecl)
    -- | C header include path graph
    --
    -- We create a DAG of C header paths with an edge for each @#include@.  The
    -- edges are /reversed/ to represent an \"included by\" relation.
    , cIncludePathGraph :: DynGraph SourcePath
    }

data TypeDecl
    = -- | The type is processing with the specified aliases
      --
      -- The list of aliases contains aliases where more than one C spelling
      -- are mapped to the same C AST node, when a @typedef@ name is the same
      -- as a @struct@, @union@, or @enum@ tag.  This is a subset of the
      -- aliases tracked in the following constructor.
      TypeDeclProcessing Type [DeclPath]
    | -- | An alias, for @typedef@s, etc.
      TypeDeclAlias Type
    | TypeDecl Type Decl
  deriving Show

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

initDeclState :: DeclState
initDeclState = DeclState {
      macroExpansions   = Set.empty
    , macroTypes        = Map.empty
    , typeDeclarations  = OMap.empty
    , cIncludePathGraph = DynGraph.empty
    }

registerMacroExpansion :: MultiLoc -> DeclState -> DeclState
registerMacroExpansion loc st = st{
      macroExpansions = Set.insert (multiLocExpansion loc) (macroExpansions st)
    }

registerMacroType :: CName -> Macro.Quant ( Macro.Type Macro.Ty ) -> DeclState -> DeclState
registerMacroType nm ty st = st{
      macroTypes = Map.insert nm ty (macroTypes st)
    }

registerInclude ::
     SourcePath -- ^ Path of header that includes the following header
  -> SourcePath -- ^ Path of the included header
  -> DeclState
  -> DeclState
registerInclude header incHeader st = st{
      cIncludePathGraph =
        DynGraph.insertEdge incHeader header (cIncludePathGraph st)
    }

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

containsMacroExpansion :: Range MultiLoc -> DeclState -> Bool
containsMacroExpansion range DeclState{macroExpansions} = or [
      -- Do a quick O(log n) check first, for the common case that the macro
      -- is right at the start of the range. For example, this would capture
      -- cases such as
      --
      -- > #define T int
      -- >
      -- > struct ExampleStruct {
      -- >   T field;
      -- > };
      Set.member (rangeStart range') macroExpansions

      -- If that fails, do a O(n) scan through all macro expansions
    , any (\e -> fromMaybe False (rangeContainsLoc range' e)) macroExpansions
    ]
  where
    range' :: Range SingleLoc
    range' = multiLocExpansion <$> range

