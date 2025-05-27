module HsBindgen.Frontend.Pass.NameMangler (
    module HsBindgen.Frontend.Pass.NameMangler.IsPass
  , mangleNames
  ) where

import HsBindgen.Frontend.AST
import HsBindgen.Frontend.Pass.NameMangler.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

mangleNames ::
     TranslationUnit ResolveBindingSpecs
  -> TranslationUnit NameMangler
mangleNames = undefined

{-------------------------------------------------------------------------------
  Internal: state required during name mangling
-------------------------------------------------------------------------------}

type State = Map (QualId ResolveBindingSpecs) PairOfIds

