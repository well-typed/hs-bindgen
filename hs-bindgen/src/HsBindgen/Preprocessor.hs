-- | Generate Haskell bindings from C headers
--
-- This module is intended to be used when using the tool in preprocessor mode.
module HsBindgen.Preprocessor (
    generateModule
    -- * Support for TH mode
  , generateDeclarations
  ) where

import Language.Haskell.Exts

import HsBindgen.Annotation (Ann)
import HsBindgen.Spec.Resolved (Spec(..))

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- TODO: <https://github.com/well-typed/hs-bindgen/issues/9>
generateModule :: Spec -> Module Ann
generateModule spec =
    Module
      mempty
      (Just $ moduleHead spec)
      [] -- No module pragmas
      (importDecls spec)
      (generateDeclarations spec)

generateDeclarations :: Spec -> [Decl Ann]
generateDeclarations _spec = []

{-------------------------------------------------------------------------------
  Module components
-------------------------------------------------------------------------------}

moduleHead :: Spec -> ModuleHead Ann
moduleHead spec =
    ModuleHead
      mempty
      (moduleName spec)
      Nothing -- No warning text (the module is not deprecated)
      (exportList spec)

moduleName :: Spec -> ModuleName Ann
moduleName spec = ModuleName mempty $ specHsModuleName spec

-- TODO: <https://github.com/well-typed/hs-bindgen/issues/76>
-- Generate export list. For now we just export everything.
exportList :: Spec -> Maybe (ExportSpecList Ann)
exportList _ = Nothing

importDecls :: Spec -> [ImportDecl Ann]
importDecls _ = []
