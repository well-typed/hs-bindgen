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

depsOfDecl :: DeclKind Parse -> [(Usage, C.QualPrelimDeclId)]
depsOfDecl (DeclStruct Struct{..}) =
    concatMap (depsOfField (fieldName . structFieldInfo) structFieldType) structFields
depsOfDecl (DeclUnion Union{..}) =
    concatMap (depsOfField (fieldName . unionFieldInfo) unionFieldType) unionFields
depsOfDecl (DeclEnum _) =
    []
depsOfDecl (DeclTypedef ty) =
    map (uncurry aux) $ depsOfTypedef ty
  where
    aux :: ValOrRef -> C.QualPrelimDeclId -> (Usage, C.QualPrelimDeclId)
    aux isPtr nsid = (UsedInTypedef isPtr, nsid)
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
    aux :: ValOrRef -> C.QualPrelimDeclId -> (Usage, C.QualPrelimDeclId)
    aux isPtr nsid = (UsedInFunction isPtr, nsid)
depsOfDecl (DeclGlobal ty) =
    map (first UsedInVar) $ depsOfType ty

-- | Dependencies of struct or union field
depsOfField :: forall a.
     (a Parse -> FieldName Parse)
  -> (a Parse -> Type Parse)
  -> a Parse -> [(Usage, C.QualPrelimDeclId)]
depsOfField getName getType field =
    map (uncurry aux) $ depsOfType $ getType field
  where
    aux :: ValOrRef -> C.QualPrelimDeclId -> (Usage, C.QualPrelimDeclId)
    aux isPtr nsid = (UsedInField isPtr (getName field), nsid)

depsOfTypedef :: Typedef Parse -> [(ValOrRef, C.QualPrelimDeclId)]
depsOfTypedef = depsOfType . typedefType

-- | The declarations this type depends on
--
-- We also report whether this dependence is through a pointer or not.
--
-- NOTE: We are only interested in /direct/ dependencies here; transitive
-- dependencies will materialize when we build the graph.
depsOfType :: Type Parse -> [(ValOrRef, C.QualPrelimDeclId)]
depsOfType = \case
    TypePrim{}             -> []
    TypeStruct uid         -> [(ByValue, C.qualPrelimDeclId uid (C.NameKindTagged C.TagKindStruct))]
    TypeUnion uid          -> [(ByValue, C.qualPrelimDeclId uid (C.NameKindTagged C.TagKindUnion))]
    TypeEnum uid           -> [(ByValue, C.qualPrelimDeclId uid (C.NameKindTagged C.TagKindEnum))]
    TypeTypedef (OrigTypedefRef name _) -> [
        ( ByValue
        , C.qualPrelimDeclId (C.PrelimDeclIdNamed name) C.NameKindOrdinary
        )
      ]
    TypeMacroTypedef uid   ->
      [(ByValue, C.qualPrelimDeclId uid C.NameKindOrdinary)]
    TypePointer ty         -> first (const ByRef) <$> depsOfType ty
    TypeFun args res       -> concatMap depsOfType args <> depsOfType res
    TypeVoid               -> []
    TypeConstArray _ ty    -> depsOfType ty
    TypeIncompleteArray ty -> depsOfType ty
    TypeExtBinding{}       -> []
    TypeBlock ty           -> depsOfType ty
    TypeConst ty           -> depsOfType ty
    TypeComplex{}          -> []
