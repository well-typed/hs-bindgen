module HsBindgen.Backend.PP.Translation (
    HsModuleOpts(..)
  , Module(..)
  , translate
  ) where

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
      moduleName  :: String
    , moduleDecls :: [SDecl BE]
    }

translate :: HsModuleOpts -> C.Header -> Module
translate opts header =
    let (decls', _st) = runM $ mapM (toBE BE) (generateDeclarations header)
    in Module
         (hsModuleName opts)
         decls'
