-- | Working with the frontend AST after the final pass
--
-- Intended for unqualified import.
module HsBindgen.Frontend.Pass.Final (
    Final
    -- * Annotations
  , unitDeps
    -- ** Name mangler
  , structNames
  , unionNames
  , enumNames
  , typedefNames
  , macroTypeNames
  ) where

import Clang.Paths

import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.MangleNames.IsPass qualified as MangleNames

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Final frontend pass
--
-- Backend passes should refer to 'Final' instead of the actual name of the
-- final pass, so that if we add more passes, the backend is unaffected.
type Final = MangleNames

{-------------------------------------------------------------------------------
  Annotations
-------------------------------------------------------------------------------}

unitDeps :: C.TranslationUnit Final -> [SourcePath]
unitDeps unit = IncludeGraph.toSortedList unit.unitIncludeGraph

structNames :: C.Struct Final -> MangleNames.RecordNames
structNames struct = struct.structAnn

unionNames :: C.Union Final -> MangleNames.NewtypeNames
unionNames union = union.unionAnn

enumNames :: C.Enum Final -> MangleNames.NewtypeNames
enumNames enum = enum.enumAnn

typedefNames :: C.Typedef Final -> MangleNames.NewtypeNames
typedefNames typedef = typedef.typedefAnn

macroTypeNames :: CheckedMacroType Final -> MangleNames.NewtypeNames
macroTypeNames macro = macro.ann
