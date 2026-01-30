module HsBindgen.Frontend.AST.Deps (
    depsOfDecl
  , depsOfField
  ) where

import GHC.Records

import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type (ValOrRef, depsOfType)
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass

{-------------------------------------------------------------------------------
  Get all dependencies
-------------------------------------------------------------------------------}

depsOfDecl :: IsPass p => C.DeclKind p -> [(ValOrRef, Id p)]
depsOfDecl (C.DeclStruct struct) = concat [
      concatMap depsOfField struct.fields
    , concatMap depsOfField struct.flam
    ]
depsOfDecl (C.DeclUnion union) =
    concatMap depsOfField union.fields
depsOfDecl (C.DeclEnum _) =
    []
depsOfDecl (C.DeclAnonEnumConstant _) =
    []
depsOfDecl (C.DeclTypedef ty) =
    depsOfTypedef ty
depsOfDecl C.DeclOpaque =
    []
depsOfDecl (C.DeclMacro _ts) =
    -- We cannot know the dependencies of a macro until we parse it, but we
    -- can't parse it until we have sorted all declarations, which requires
    -- knowing the dependencies. Catch-22. We therefore regard macros as not
    -- having /any/ dependencies, and will rely instead on source ordering.
    []
depsOfDecl (C.DeclFunction function) =
    concatMap depsOfType (function.res : map (.typ) function.args)
depsOfDecl (C.DeclGlobal _ext ty) =
    depsOfType ty

-- | Dependencies of struct or union field
depsOfField :: forall a p.
     (HasField "typ" (a p) (C.Type p), IsPass p)
  => a p -> [(ValOrRef, Id p)]
depsOfField field = depsOfType field.typ

depsOfTypedef :: IsPass p => C.Typedef p -> [(ValOrRef, Id p)]
depsOfTypedef typedef = depsOfType typedef.typ

