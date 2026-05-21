-- Intended for unqualified import
module HsBindgen.Frontend.DeclMeta (
    DeclMeta(..)
  ) where

import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclUseGraph.Definition (DeclUseGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Information about the declarations
-------------------------------------------------------------------------------}

data DeclMeta = DeclMeta {
      declIndex    :: DeclIndex
    , useDeclGraph :: UseDeclGraph
    , declUseGraph :: DeclUseGraph
    }
  deriving stock (Show, Generic)
