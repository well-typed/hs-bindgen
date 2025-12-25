module HsBindgen.Frontend.AST.Deps (
    Usage(..)
  , ValOrRef(..)
  , usageMode
  , depsOfDecl
  , depsOfType
  ) where

import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definitions
-------------------------------------------------------------------------------}

data Usage p =
    UsedInTypedef ValOrRef
  | UsedInField ValOrRef (ScopedName p)
  | UsedInFunction ValOrRef
  | UsedInVar ValOrRef -- ^ Global or constant

deriving instance IsPass p => Show (Usage p)
deriving instance IsPass p => Eq   (Usage p)
deriving instance IsPass p => Ord  (Usage p)

data ValOrRef = ByValue | ByRef
  deriving stock (Show, Eq, Ord)

usageMode :: Usage p -> ValOrRef
usageMode = \case
    (UsedInTypedef  x  ) -> x
    (UsedInField    x _) -> x
    (UsedInFunction x  ) -> x
    (UsedInVar      x  ) -> x

{-------------------------------------------------------------------------------
  Get all dependencies
-------------------------------------------------------------------------------}

depsOfDecl :: C.DeclKind p -> [(Usage p, Id p)]
depsOfDecl (C.DeclStruct C.Struct{..}) =
    concatMap (depsOfField (C.fieldName . C.structFieldInfo) C.structFieldType) structFields
depsOfDecl (C.DeclUnion C.Union{..}) =
    concatMap (depsOfField (C.fieldName . C.unionFieldInfo) C.unionFieldType) unionFields
depsOfDecl (C.DeclEnum _) =
    []
depsOfDecl (C.DeclTypedef ty) =
    map (uncurry aux) $ depsOfTypedef ty
  where
    aux :: ValOrRef -> Id p -> (Usage p, Id p)
    aux isPtr declId = (UsedInTypedef isPtr, declId)
depsOfDecl C.DeclOpaque =
    []
depsOfDecl (C.DeclMacro _ts) =
    -- We cannot know the dependencies of a macro until we parse it, but we
    -- can't parse it until we have sorted all declarations, which requires
    -- knowing the dependencies. Catch-22. We therefore regard macros as not
    -- having /any/ dependencies, and will rely instead on source ordering.
    []
depsOfDecl (C.DeclFunction (C.Function {..})) =
    map (uncurry aux) $ concatMap depsOfType (functionRes : map snd functionArgs)
  where
    aux :: ValOrRef -> Id p -> (Usage p, Id p)
    aux isPtr qualPrelimDeclId = (UsedInFunction isPtr, qualPrelimDeclId)
depsOfDecl (C.DeclGlobal ty) =
    map (first UsedInVar) $ depsOfType ty

-- | Dependencies of struct or union field
depsOfField :: forall a p.
     (a p -> ScopedName p)
  -> (a p -> C.Type p)
  -> a p -> [(Usage p, Id p)]
depsOfField getName getType field =
    map (uncurry aux) $ depsOfType $ getType field
  where
    aux :: ValOrRef -> Id p -> (Usage p, Id p)
    aux isPtr qualPrelimDeclId = (UsedInField isPtr (getName field), qualPrelimDeclId)

depsOfTypedef :: C.Typedef p -> [(ValOrRef, Id p)]
depsOfTypedef = depsOfType . C.typedefType

-- | The declarations this type depends on
--
-- We also report whether this dependence is through a pointer or not.
--
-- NOTE: We are only interested in /direct/ dependencies here; transitive
-- dependencies will materialize when we build the graph.
--
-- TODO: Duplication between 'depsOfType' and 'typeDeclIds'.
depsOfType :: C.Type p -> [(ValOrRef, Id p)]
depsOfType = \case
    C.TypePrim{}             -> []
    C.TypeRef ref            -> [(ByValue, ref)]
    C.TypeTypedef typedef    -> [(ByValue, typedef.ref)]
    C.TypePointers _ ty      -> first (const ByRef) <$> depsOfType ty
    C.TypeFun args res       -> concatMap depsOfType args <> depsOfType res
    C.TypeVoid               -> []
    C.TypeConstArray _ ty    -> depsOfType ty
    C.TypeIncompleteArray ty -> depsOfType ty
    C.TypeExtBinding{}       -> []
    C.TypeBlock ty           -> depsOfType ty
    C.TypeQualified _qual ty -> depsOfType ty
    C.TypeComplex{}          -> []
