module HsBindgen.Frontend.Pass.PrepareReparse (
    prepareReparse
  ) where

import HsBindgen.Clang (ClangSetup)
import HsBindgen.Frontend.AST.Coerce (CoercePass (coercePass))
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Pass (IsPass (Msg))
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass (PrepareReparse)
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass (TypecheckMacros)
import HsBindgen.Frontend.RootHeader (RootHeader)
import HsBindgen.Util.Tracer (Tracer)

prepareReparse ::
     Tracer (Msg PrepareReparse)
  -> ClangSetup
  -> RootHeader
  -> C.TranslationUnit TypecheckMacros
  -> IO (C.TranslationUnit PrepareReparse)
prepareReparse _tr _setup _root unit = pure (coercePass unit)
