module HsBindgen.Frontend.Pass.PrepareReparse (
    prepareReparse
  ) where

import HsBindgen.Clang (ClangSetup)
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Pass (IsPass (Msg))
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass (PrepareReparse)
import HsBindgen.Frontend.Pass.PrepareReparse.Updater (update)
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass (TypecheckMacros)
import HsBindgen.Frontend.RootHeader (RootHeader)
import HsBindgen.Util.Tracer (Tracer)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

prepareReparse ::
     Tracer (Msg PrepareReparse)
  -> ClangSetup
  -> RootHeader
  -> C.TranslationUnit TypecheckMacros
  -> IO (C.TranslationUnit PrepareReparse)
prepareReparse _tr _setup _root unit = pure $ runUpdater unit

{-------------------------------------------------------------------------------
  Internal phases
-------------------------------------------------------------------------------}

runUpdater ::
     C.TranslationUnit TypecheckMacros
  -> C.TranslationUnit PrepareReparse
runUpdater unit = update unit
