module HsBindgen.Backend.HsSrcExts.Translation (
    HsModuleOpts(..)
  , translate
  ) where

import Language.Haskell.Exts qualified as E

import HsBindgen.Backend.Common.Translation
import HsBindgen.Backend.HsSrcExts
import HsBindgen.C.AST qualified as C
import HsBindgen.Translation.LowLevel

{-------------------------------------------------------------------------------
  Generate top-level module
-------------------------------------------------------------------------------}

data HsModuleOpts = HsModuleOpts {
      hsModuleName :: String
    }
  deriving (Show)

translate :: HsModuleOpts -> C.Header -> E.Module Ann
translate opts header =
    let (decls', _st) = runM $ mapM (toBE BE) (generateDeclarations header)
    in E.Module
         ann
         (Just $ moduleHead opts)
         [] -- No module pragmas
         importDecls
         decls'

{-------------------------------------------------------------------------------
  Module components
-------------------------------------------------------------------------------}

moduleHead :: HsModuleOpts -> E.ModuleHead Ann
moduleHead opts =
    E.ModuleHead
      ann
      (moduleName opts)
      Nothing -- No warning text (the module is not deprecated)
      exportList

moduleName :: HsModuleOpts -> E.ModuleName Ann
moduleName = E.ModuleName ann . hsModuleName

-- TODO: <https://github.com/well-typed/hs-bindgen/issues/76>
-- Generate export list. For now we just export everything.
exportList :: Maybe (E.ExportSpecList Ann)
exportList = Nothing

importDecls :: [E.ImportDecl Ann]
importDecls = []

