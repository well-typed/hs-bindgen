module HsBindgen.Frontend.AST.Deps (
    Usage(..)
  , ValOrRef(..)
  , usageMode
  , depsOfDecl
  , depsOfType
  ) where

import HsBindgen.Frontend.AST.Internal
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definitions
-------------------------------------------------------------------------------}

data Usage =
    UsedInTypedef ValOrRef
  | UsedInField ValOrRef (FieldName Parse)
  | UsedInFunction ValOrRef
  | UsedInVar ValOrRef -- ^ Global or constant
  deriving stock (Show, Eq, Ord)

data ValOrRef = ByValue | ByRef
  deriving stock (Show, Eq, Ord)

usageMode :: Usage -> ValOrRef
usageMode = \case
    (UsedInTypedef  x  ) -> x
    (UsedInField    x _) -> x
    (UsedInFunction x  ) -> x
    (UsedInVar      x  ) -> x

{-------------------------------------------------------------------------------
  Get all dependencies
-------------------------------------------------------------------------------}

depsOfDecl :: DeclKind Parse -> [(Usage, C.PrelimDeclId)]
depsOfDecl (DeclStruct Struct{..}) =
    concatMap (depsOfField (fieldName . structFieldInfo) structFieldType) structFields
depsOfDecl (DeclUnion Union{..}) =
    concatMap (depsOfField (fieldName . unionFieldInfo) unionFieldType) unionFields
depsOfDecl (DeclEnum _) =
    []
depsOfDecl (DeclTypedef ty) =
    map (uncurry aux) $ depsOfTypedef ty
  where
    aux :: ValOrRef -> C.PrelimDeclId -> (Usage, C.PrelimDeclId)
    aux isPtr qualPrelimDeclId = (UsedInTypedef isPtr, qualPrelimDeclId)
depsOfDecl (DeclOpaque _) =
    []
depsOfDecl (DeclMacro _ts) =
    -- We cannot know the dependencies of a macro until we parse it, but we
    -- can't parse it until we have sorted all declarations, which requires
    -- knowing the dependencies. Catch-22. We therefore regard macros as not
    -- having /any/ dependencies, and will rely instead on source ordering.
    []
depsOfDecl (DeclFunction (Function {..})) =
    map (uncurry aux) $ concatMap depsOfType (functionRes : map snd functionArgs)
  where
    aux :: ValOrRef -> C.PrelimDeclId -> (Usage, C.PrelimDeclId)
    aux isPtr qualPrelimDeclId = (UsedInFunction isPtr, qualPrelimDeclId)
depsOfDecl (DeclGlobal ty) =
    map (first UsedInVar) $ depsOfType ty

-- | Dependencies of struct or union field
depsOfField :: forall a.
     (a Parse -> FieldName Parse)
  -> (a Parse -> Type Parse)
  -> a Parse -> [(Usage, C.PrelimDeclId)]
depsOfField getName getType field =
    map (uncurry aux) $ depsOfType $ getType field
  where
    aux :: ValOrRef -> C.PrelimDeclId -> (Usage, C.PrelimDeclId)
    aux isPtr qualPrelimDeclId = (UsedInField isPtr (getName field), qualPrelimDeclId)

depsOfTypedef :: Typedef Parse -> [(ValOrRef, C.PrelimDeclId)]
depsOfTypedef = depsOfType . typedefType

-- | The declarations this type depends on
--
-- We also report whether this dependence is through a pointer or not.
--
-- NOTE: We are only interested in /direct/ dependencies here; transitive
-- dependencies will materialize when we build the graph.
depsOfType :: Type Parse -> [(ValOrRef, C.PrelimDeclId)]
depsOfType = \case
    TypePrim{}             -> []
    TypeRef uid            -> [(ByValue, uid)]
    TypeTypedef uid _uTy   -> [(ByValue, uid)]
    TypePointers _ ty      -> first (const ByRef) <$> depsOfType ty
    TypeFun args res       -> concatMap depsOfType args <> depsOfType res
    TypeVoid               -> []
    TypeConstArray _ ty    -> depsOfType ty
    TypeIncompleteArray ty -> depsOfType ty
    TypeExtBinding{}       -> []
    TypeBlock ty           -> depsOfType ty
    TypeConst ty           -> depsOfType ty
    TypeComplex{}          -> []
