module HsBindgen.Frontend.AST.Deps (depsOfDecl) where

import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.AST.Type (ValOrRef, depsOfType)
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass

{-------------------------------------------------------------------------------
  Get all dependencies
-------------------------------------------------------------------------------}

depsOfDecl :: C.DeclKind p -> [(ValOrRef, Id p)]
depsOfDecl (C.DeclStruct C.Struct{..}) = concat [
      concatMap (depsOfField C.structFieldType) structFields
    , concatMap (depsOfField C.structFieldType) structFlam
    ]
depsOfDecl (C.DeclUnion C.Union{..}) =
    concatMap (depsOfField C.unionFieldType) unionFields
depsOfDecl (C.DeclEnum _) =
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
depsOfDecl (C.DeclFunction (C.Function {..})) =
    concatMap depsOfType (functionRes : map snd functionArgs)
depsOfDecl (C.DeclGlobal ty) =
    depsOfType ty

-- | Dependencies of struct or union field
depsOfField :: forall a p.
     (a p -> C.Type p)
  -> a p -> [(ValOrRef, Id p)]
depsOfField getType field =
    depsOfType $ getType field

depsOfTypedef :: C.Typedef p -> [(ValOrRef, Id p)]
depsOfTypedef = depsOfType . C.typedefType

