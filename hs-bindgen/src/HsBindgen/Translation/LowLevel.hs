-- | Low-level translation of the C header to a Haskell module
--
-- TODO: This module is intended to implement the following milestones:
--
-- * Milestone 1: @Storable@ instances
--   <https://github.com/well-typed/hs-bindgen/milestone/2>
-- * Milestone 2: Low-level API
--   <https://github.com/well-typed/hs-bindgen/milestone/3>
module HsBindgen.Translation.LowLevel (
    HsModuleOpts(..)
  , generateModule
  , generateDeclarations
  ) where

import Language.Haskell.Exts qualified as Hs

import HsBindgen.C.AST qualified as C
import HsBindgen.Hs.Annotation
-- import HsBindgen.Spec

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

data HsModuleOpts = HsModuleOpts {
      hsModuleName :: String
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- TODO: <https://github.com/well-typed/hs-bindgen/issues/9>
generateModule :: HsModuleOpts -> C.Header -> Hs.Module Ann
generateModule opts (C.Header decls) =
    Hs.Module
      noAnn
      (Just $ moduleHead opts)
      [] -- No module pragmas
      importDecls
      (generateDeclarations decls)

generateDeclarations :: [C.Decl] -> [Hs.Decl Ann]
generateDeclarations _decls = []

{-------------------------------------------------------------------------------
  Module components
-------------------------------------------------------------------------------}

moduleHead :: HsModuleOpts -> Hs.ModuleHead Ann
moduleHead opts =
    Hs.ModuleHead
      noAnn
      (moduleName opts)
      Nothing -- No warning text (the module is not deprecated)
      exportList

moduleName :: HsModuleOpts -> Hs.ModuleName Ann
moduleName = Hs.ModuleName noAnn . hsModuleName

-- TODO: <https://github.com/well-typed/hs-bindgen/issues/76>
-- Generate export list. For now we just export everything.
exportList :: Maybe (Hs.ExportSpecList Ann)
exportList = Nothing

importDecls :: [Hs.ImportDecl Ann]
importDecls = []
