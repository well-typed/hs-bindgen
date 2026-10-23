-- Intended for unqualified import
module HsBindgen.Frontend.DeclMeta (
    DeclMeta(..)
  ) where

import HsBindgen.Frontend.Analysis.DeclUseGraph.Definition (DeclUseGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.DeclDB (DeclDB)
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass (ConstructTranslationUnit)
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Information about the declarations
-------------------------------------------------------------------------------}

data DeclMeta l = DeclMeta {
      declDB       :: DeclDB l ConstructTranslationUnit
    , useDeclGraph :: UseDeclGraph
    , declUseGraph :: DeclUseGraph
    }
  deriving stock (Show, Generic)
