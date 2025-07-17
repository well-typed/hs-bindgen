module HsBindgen.Frontend.AST.Deps (
    Usage(..)
  , ValOrRef(..)
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

{-------------------------------------------------------------------------------
  Get all dependencies
-------------------------------------------------------------------------------}

depsOfDecl :: DeclKind Parse -> [(Usage, C.NsPrelimDeclId)]
depsOfDecl (DeclStruct Struct{..}) =
    concatMap (depsOfField structFieldName structFieldType) structFields
depsOfDecl DeclStructOpaque =
    []
depsOfDecl (DeclUnion Union{..}) =
    concatMap (depsOfField unionFieldName unionFieldType) unionFields
depsOfDecl DeclUnionOpaque =
    []
depsOfDecl (DeclEnum _) =
    []
depsOfDecl DeclEnumOpaque =
    []
depsOfDecl (DeclTypedef ty) =
    map (uncurry aux) $ depsOfTypedef ty
  where
    aux :: ValOrRef -> C.NsPrelimDeclId -> (Usage, C.NsPrelimDeclId)
    aux isPtr nsid = (UsedInTypedef isPtr, nsid)
depsOfDecl (DeclMacro _ts) =
    -- We cannot know the dependencies of a macro until we parse it, but we
    -- can't parse it until we have sorted all declarations, which requires
    -- knowing the dependencies. Catch-22. We therefore regard macros as not
    -- having /any/ dependencies, and will rely instead on source ordering.
    []
depsOfDecl (DeclFunction (Function {..})) =
    map (uncurry aux) $ concatMap depsOfType (functionRes : functionArgs)
  where
    aux :: ValOrRef -> C.NsPrelimDeclId -> (Usage, C.NsPrelimDeclId)
    aux isPtr nsid = (UsedInFunction isPtr, nsid)
depsOfDecl (DeclGlobal ty) =
    map (first UsedInVar) $ depsOfType ty
depsOfDecl (DeclConst ty) =
    map (first UsedInVar) $ depsOfType ty

-- | Dependencies of struct or union field
depsOfField :: forall a.
     (a Parse -> FieldName Parse)
  -> (a Parse -> Type Parse)
  -> a Parse -> [(Usage, C.NsPrelimDeclId)]
depsOfField getName getType field =
    map (uncurry aux) $ depsOfType $ getType field
  where
    aux :: ValOrRef -> C.NsPrelimDeclId -> (Usage, C.NsPrelimDeclId)
    aux isPtr nsid = (UsedInField isPtr (getName field), nsid)

depsOfTypedef :: Typedef Parse -> [(ValOrRef, C.NsPrelimDeclId)]
depsOfTypedef = depsOfType . typedefType

-- | The declarations this type depends on
--
-- We also report whether this dependence is through a pointer or not.
--
-- NOTE: We are only interested in /direct/ dependencies here; transitive
-- dependencies will materialize when we build the graph.
depsOfType :: Type Parse -> [(ValOrRef, C.NsPrelimDeclId)]
depsOfType = \case
    TypePrim{}             -> []
    TypeStruct uid         -> [(ByValue, C.nsPrelimDeclId uid C.TypeNamespaceTag)]
    TypeUnion uid          -> [(ByValue, C.nsPrelimDeclId uid C.TypeNamespaceTag)]
    TypeEnum uid           -> [(ByValue, C.nsPrelimDeclId uid C.TypeNamespaceTag)]
    TypeTypedef name       -> [
        ( ByValue
        , C.nsPrelimDeclId (C.PrelimDeclIdNamed name) C.TypeNamespaceOrdinary
        )
      ]
    TypeMacroTypedef uid   ->
      [(ByValue, C.nsPrelimDeclId uid C.TypeNamespaceOrdinary)]
    TypePointer ty         -> first (const ByRef) <$> depsOfType ty
    TypeFun args res       -> concatMap depsOfType args <> depsOfType res
    TypeVoid               -> []
    TypeConstArray _ ty    -> depsOfType ty
    TypeIncompleteArray ty -> depsOfType ty
    TypeExtBinding{}       -> []
    TypeBlock ty           -> depsOfType ty
