module HsBindgen.C.Reparse.FieldDecl (
    reparseFieldDecl
  ) where

import HsBindgen.C.AST.Name
import HsBindgen.C.AST.Type
import HsBindgen.C.Reparse.Common
import HsBindgen.C.Reparse.Infra
import HsBindgen.C.Reparse.Type

-- | Field declaration (in a struct)
reparseFieldDecl :: Reparse (Typ, CName)
reparseFieldDecl = (,) <$> reparseTypeUse <*> reparseName