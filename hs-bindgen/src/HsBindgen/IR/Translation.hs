module HsBindgen.IR.Translation (
    translateDecl,
) where

import HsBindgen.C.AST qualified as C
import HsBindgen.IR.AST qualified as IR

translateDecl :: C.Decl -> [IR.Decl]
translateDecl _ = [] -- TODO
