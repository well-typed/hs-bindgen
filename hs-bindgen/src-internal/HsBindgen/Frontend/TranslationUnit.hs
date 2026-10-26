-- Intended for qualified import
--
-- @
-- import HsBindgen.Frontend.TranslationUnit qualified as C
-- @
module HsBindgen.Frontend.TranslationUnit (
    TranslationUnit(..)
  , fromDeclDB
  , activeDecls
  ) where

import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.DeclDB (DeclDB)
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
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
      -- | Declarations in the unit
      --
      -- The 'activeDecls' of 'db' (see 'fromDeclDB').
      --
      -- Declarations from all headers that we have processed. Passes may remove
      -- some declarations. For example,
      --
      -- * The 'HsBindgen.Frontend.Pass.Parse.IsPass.Parse' pass filters out declarations not matching the selection
      --   predicate (without program slicing).
      --
      -- * If program slicing is enabled, the @Select@ pass filters selected
      --   declarations and their transitive dependencies.
      --
      -- * The 'HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass.ResolveBindingSpecs' pass removes declarations for which we have
      --   existing external bindings, as well as declarations omitted by a
      --   prescriptive binding specification.
      decls :: [C.Decl l p]

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
    , includeGraph :: IncludeGraph

      -- | Declaration database
    , db :: DeclDB l p
    }
  deriving stock (Generic)

deriving stock instance ( IsPass p
                        , Macro.HasTypes l
                        ) => Show (TranslationUnit l p)

-- | The unit of a database: 'decls' are its 'activeDecls'
fromDeclDB :: IncludeGraph -> DeclDB l p -> TranslationUnit l p
fromDeclDB includeGraph db = TranslationUnit{
      decls        = activeDecls includeGraph db
    , includeGraph = includeGraph
    , db           = db
    }

-- | The active declarations, in the order of 'decls'
--
-- Dependency order, refined by
-- 'HsBindgen.Frontend.Analysis.DeclUseGraph.declSortKey'.
activeDecls :: IncludeGraph -> DeclDB l p -> [C.Decl l p]
activeDecls includeGraph =
    DeclDB.active $ DeclDB.DependencyBy $
      DeclUseGraph.declSortKey (IncludeGraph.toIncludeOrder includeGraph)
