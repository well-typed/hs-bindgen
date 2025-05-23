module HsBindgen.Frontend.Pass.ResolveBindingSpecs (
    module HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
  , resolveBindingSpecs
  ) where

import HsBindgen.Frontend.AST
import HsBindgen.Frontend.Pass.RenameAnon
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass

resolveBindingSpecs ::
     TranslationUnit RenameAnon
  -> TranslationUnit ResolveBindingSpecs
resolveBindingSpecs = undefined

