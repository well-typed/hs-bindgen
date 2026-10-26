-- Intended for qualified import
--
-- @
-- import HsBindgen.Frontend.TranslationUnit qualified as C
-- @
module HsBindgen.Frontend.TranslationUnit (
    TranslationUnit(..)
  ) where

import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.DeclDB (DeclDB)
import HsBindgen.Imports
import HsBindgen.IR.Pass
import HsBindgen.Macro.Type qualified as Macro

-- | Information we collect from a C translation unit.
--
-- The macro language parameter @l@ describes how C macros are parsed,
-- typechecked, and translated (see "HsBindgen.Macro.Type" and
-- "HsBindgen.Macro.Interface").
--
-- The pass parameter @p@ describes how the data types evolve along passes in
-- the frontend in a "trees that grow" (TTG) style.
data TranslationUnit l p = TranslationUnit{
      -- | Include graph
      --
      -- This is used to declare TH dependencies.
      --
      -- It can also be useful for users to see this graph, as it may provide
      -- insight into the binding generation process. For example, suppose we
      -- have a large library (say Gtk), with a few main entry points (for which
      -- we should generate separate Haskell modules) and a core of "common"
      -- definitions; it may be quite useful to look at the include graph to
      -- figure out what this set of "core" headers is.
      includeGraph :: IncludeGraph

      -- | Declaration database
    , db :: DeclDB l p
    }
  deriving stock (Generic)

deriving stock instance ( IsPass p
                        , Macro.HasTypes l
                        ) => Show (TranslationUnit l p)
