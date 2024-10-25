module HsBindgen.C.Fold.DeclState (
    DeclState(..)
    -- * Construction
  , initDeclState
  , registerMacroExpansion
  , registerMacroType
    -- * Query
  , containsMacroExpansion
  ) where

import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import HsBindgen.Clang.HighLevel.Types
import HsBindgen.C.AST (CName)
import HsBindgen.C.Tc.Macro (QuantTy)

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
      macroExpansions :: Set SingleLoc
    , macroTypes      :: Map CName QuantTy
    }

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

initDeclState :: DeclState
initDeclState = DeclState {
      macroExpansions = Set.empty
    , macroTypes      = Map.empty
    }

registerMacroExpansion :: MultiLoc -> DeclState -> DeclState
registerMacroExpansion loc st = st{
      macroExpansions = Set.insert (multiLocExpansion loc) (macroExpansions st)
    }

registerMacroType :: CName -> QuantTy -> DeclState -> DeclState
registerMacroType nm ty st = st{
      macroTypes = Map.insert nm ty (macroTypes st)
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

