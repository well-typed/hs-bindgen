{-# LANGUAGE RecordWildCards #-}

module HsBindgen.Backend.PP.Translation (
    HsModuleOpts(..)
  , Module(..)
  , translate
  ) where

import Data.Set qualified as Set

import HsBindgen.Backend.Common
import HsBindgen.Backend.Common.Translation
import HsBindgen.Backend.PP
import HsBindgen.C.AST qualified as C
import HsBindgen.Translation.LowLevel

{-------------------------------------------------------------------------------
  Generate top-level module
-------------------------------------------------------------------------------}

newtype HsModuleOpts = HsModuleOpts {
      hsModuleName :: String
    }
  deriving (Show)

data Module = Module {
      moduleName    :: String
    , moduleImports :: [QualifiedImport]
    , moduleDecls   :: [SDecl BE]
    }

translate :: HsModuleOpts -> C.Header -> Module
translate opts header =
    let moduleName = hsModuleName opts
        (moduleDecls, _) = runM $ mapM (toBE BE) (generateDeclarations header)
        moduleImports =
          Set.toAscList . mconcat $ map getDeclQualifiedImports moduleDecls
    in  Module{..}
