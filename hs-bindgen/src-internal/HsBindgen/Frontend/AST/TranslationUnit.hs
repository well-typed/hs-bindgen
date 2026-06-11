-- Intended for qualified import
--
-- @
-- import HsBindgen.Frontend.AST.TranslationUnit qualified as C
-- @
module HsBindgen.Frontend.AST.TranslationUnit (
    TranslationUnit(..)

  ) where

import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.DeclMeta
import HsBindgen.Frontend.Pass
import HsBindgen.Imports
import HsBindgen.Macro.Type

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

      -- | Pass-specific annotation
    , meta :: DeclMeta l
    }
  deriving stock (Generic)

deriving stock instance ( IsPass p
                        , Show (CommentDecl p)
                        , HasMacroTypes l
                        ) => Show (TranslationUnit l p)

instance (
      CoercePass (C.Decl l) p p'
    , Ann "TranslationUnit" p ~ Ann "TranslationUnit" p'
    ) => CoercePass (TranslationUnit l) p p' where
  coercePass unit = TranslationUnit{
        decls        = map coercePass unit.decls
      , includeGraph = unit.includeGraph
      , meta         = unit.meta
      }

