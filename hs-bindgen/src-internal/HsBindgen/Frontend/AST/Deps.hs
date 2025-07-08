module HsBindgen.Frontend.AST.Deps (
    Usage(..)
  , ValOrRef(..)
  , depsOfDecl
  , depsOfType
  ) where

import HsBindgen.Frontend.AST.Internal
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Type.DeclId
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

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

depsOfDecl :: DeclKind Parse -> [(Usage, QualDeclId)]
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
    aux :: ValOrRef -> QualDeclId -> (Usage, QualDeclId)
    aux isPtr qid = (UsedInTypedef isPtr, qid)
depsOfDecl (DeclMacro _ts) =
    -- We cannot know the dependencies of a macro until we parse it, but we
    -- can't parse it until we have sorted all declarations, which requires
    -- knowing the dependencies. Catch-22. We therefore regard macros as not
    -- having /any/ dependencies, and will rely instead on source ordering.
    []
depsOfDecl (DeclFunction (Function {..})) =
    map (uncurry aux) $ concatMap depsOfType (functionRes : functionArgs)
  where
    aux :: ValOrRef -> QualDeclId -> (Usage, QualDeclId)
    aux isPtr qid = (UsedInFunction isPtr, qid)
depsOfDecl (DeclExtern ty) =
    map (first UsedInVar) $ depsOfType ty
depsOfDecl (DeclConst ty) =
    map (first UsedInVar) $ depsOfType ty

-- | Dependencies of struct or union field
depsOfField :: forall a.
     (a Parse -> FieldName Parse)
  -> (a Parse -> Type Parse)
  -> a Parse -> [(Usage, QualDeclId)]
depsOfField getName getType field =
    map (uncurry aux) $ depsOfType $ getType field
  where
    aux :: ValOrRef -> QualDeclId -> (Usage, QualDeclId)
    aux isPtr qid = (UsedInField isPtr (getName field), qid)

depsOfTypedef :: Typedef Parse -> [(ValOrRef, QualDeclId)]
depsOfTypedef = depsOfType . typedefType

-- | The declarations this type depends on
--
-- We also report whether this dependence is through a pointer or not.
--
-- NOTE: We are only interested in /direct/ dependencies here; transitive
-- dependencies will materialize when we build the graph.
depsOfType :: Type Parse -> [(ValOrRef, QualDeclId)]
depsOfType = \case
    TypePrim{}             -> []
    TypeStruct uid _       -> [(ByValue, QualDeclId uid C.NameKindStruct)]
    TypeUnion uid _        -> [(ByValue, QualDeclId uid C.NameKindUnion)]
    TypeEnum uid _         -> [(ByValue, QualDeclId uid C.NameKindEnum)]
    TypeTypedef uid        -> [(ByValue, QualDeclId (DeclNamed uid) C.NameKindOrdinary)]
    TypeMacroTypedef uid _ -> [(ByValue, QualDeclId uid C.NameKindOrdinary)]
    TypePointer ty         -> first (const ByRef) <$> depsOfType ty
    TypeFun args res       -> concatMap depsOfType args <> depsOfType res
    TypeVoid               -> []
    TypeConstArray _ ty    -> depsOfType ty
    TypeIncompleteArray ty -> depsOfType ty
    TypeExtBinding{}       -> []
