-- | Main entry point for using @hs-bindgen@ in TH mode
module HsBindgen.TH (
    genBindings
  , genBindings'
  ) where

import Data.Set qualified as Set
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH (addDependentFile)

import Data.DynGraph qualified as DynGraph
import HsBindgen.Backend.Extensions
import HsBindgen.Backend.TH.Translation qualified as Backend.TH
import HsBindgen.C.AST qualified as C
import HsBindgen.C.Fold qualified as C
import HsBindgen.C.Fold.DeclState qualified as C
import HsBindgen.C.Parser qualified as C
import HsBindgen.C.Predicate (Predicate(..))
import HsBindgen.Clang.Args
import HsBindgen.Clang.Paths
import HsBindgen.ExtBindings
import HsBindgen.Hs.Translation qualified as LowLevel
import HsBindgen.Imports
import HsBindgen.Resolve
import HsBindgen.SHs.Translation qualified as SHs
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Template Haskell API
-------------------------------------------------------------------------------}

-- | Generate bindings for the given C header
--
-- TODO: add TranslationOpts argument
genBindings ::
     FilePath -- ^ Input header, as written in C @#include@
  -> ExtBindings
  -> ClangArgs
  -> TH.Q [TH.Dec]
genBindings fp extBindings args = do
    headerIncludePath <- either fail return $ parseCHeaderIncludePath fp

    (cheader, depPaths) <- TH.runIO $ do
      src <- resolveHeader args headerIncludePath
      C.withTranslationUnit nullTracer args src $ \unit -> do
        (decls, finalDeclState) <-
          C.foldTranslationUnitWith
            unit
            (C.runFoldState C.initDeclState)
            (C.foldDecls nullTracer SelectFromMainFile extBindings unit)
        let decls' =
              [ d
              | C.TypeDecl _ d <- toList (C.typeDeclarations finalDeclState)
              ]
            depPaths = DynGraph.vertices $ C.cIncludePathGraph finalDeclState
        return (C.Header (decls ++ decls'), depPaths)

    -- record dependencies, including transitively included headers
    mapM_ (TH.addDependentFile . getSourcePath) depPaths

    let sdecls = map SHs.translateDecl $
          LowLevel.generateDeclarations
            headerIncludePath
            LowLevel.defaultTranslationOpts
            cheader

    -- extensions checks.
    -- Potential TODO: we could also check which enabled extension may interfere with the generated code. (e.g. Strict/Data)
    enabledExts <- Set.fromList <$> TH.extsEnabled
    let requiredExts = foldMap requiredExtensions sdecls
        missingExts  = requiredExts `Set.difference` enabledExts
    unless (null missingExts) $ do
      TH.reportError $ "Missing LANGUAGE extensions: " ++ unwords (map show (toList missingExts))

    -- generate TH declarations
    concat <$> traverse Backend.TH.mkDecl sdecls

-- | Generate bindings for the given C header
--
-- This function uses default Clang arguments but allows you to add directories
-- to the include search path.  Use 'genBindings' when more configuration is
-- required.
genBindings' ::
     [FilePath] -- ^ Quote include search path directories
  -> FilePath   -- ^ Input header, as written in C @#include@
  -> TH.Q [TH.Dec]
genBindings' quoteIncPathDirs fp = genBindings fp emptyExtBindings args
  where
    args :: ClangArgs
    args = defaultClangArgs {
        clangQuoteIncludePathDirs  = CIncludePathDir <$> quoteIncPathDirs
      }
