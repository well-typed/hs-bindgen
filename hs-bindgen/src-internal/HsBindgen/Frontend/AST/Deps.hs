module HsBindgen.Frontend.AST.Deps (
    Usage(..)
  , ValOrRef(..)
  , depsOfDecl
  , depsOfType
  ) where

import HsBindgen.Frontend.AST.Internal
import HsBindgen.Frontend.Pass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass

{-------------------------------------------------------------------------------
  Definitions
-------------------------------------------------------------------------------}

data Usage =
    UsedInTypedef ValOrRef
  | UsedInField ValOrRef (FieldName Parse)
  | UsedInFunction ValOrRef
  deriving stock (Show, Eq, Ord)

data ValOrRef = ByValue | ByRef
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Get all dependencies
-------------------------------------------------------------------------------}

depsOfDecl :: DeclKind Parse -> [(Usage, QualId Parse)]
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
    aux :: ValOrRef -> QualId Parse -> (Usage, QualId Parse)
    aux isPtr uid = (UsedInTypedef isPtr, uid)
depsOfDecl (DeclMacro _ts) =
    -- We cannot know the dependencies of a macro until we parse it, but we
    -- can't parse it until we have sorted all declarations, which requires
    -- knowing the dependencies. Catch-22. We therefore regard macros as not
    -- having /any/ dependencies, and will rely instead on source ordering.
    []
depsOfDecl (DeclFunction (Function {..})) =
    map (uncurry aux) $ concatMap depsOfType (functionRes : functionArgs)
  where
    aux :: ValOrRef -> QualId Parse -> (Usage, QualId Parse)
    aux isPtr uid = (UsedInFunction isPtr, uid)

-- | Dependencies of struct or union field
depsOfField :: forall a.
     (a Parse -> FieldName Parse)
  -> (a Parse -> Type Parse)
  -> a Parse -> [(Usage, QualId Parse)]
depsOfField getName getType field =
    map (uncurry aux) $ depsOfType $ getType field
  where
    aux :: ValOrRef -> QualId Parse -> (Usage, QualId Parse)
    aux isPtr uid = (UsedInField isPtr (getName field), uid)

depsOfTypedef :: Typedef Parse -> [(ValOrRef, QualId Parse)]
depsOfTypedef = depsOfType . typedefType

-- | The declarations this type depends on
--
-- We also report whether this dependence is through a pointer or not.
--
-- NOTE: We are only interested in /direct/ dependencies here; transitive
-- dependencies will materialize when we build the graph.
depsOfType :: Type Parse -> [(ValOrRef, QualId Parse)]
depsOfType = \case
    TypePrim{}             -> []
    TypeStruct uid _       -> [(ByValue, QualId uid C.NameKindStruct)]
    TypeUnion uid _        -> [(ByValue, QualId uid C.NameKindUnion)]
    TypeEnum uid _         -> [(ByValue, QualId uid C.NameKindEnum)]
    TypeTypedef uid        -> [(ByValue, QualId (DeclNamed uid) C.NameKindOrdinary)]
    TypeMacroTypedef uid _ -> [(ByValue, QualId uid C.NameKindOrdinary)]
    TypePointer ty         -> first (const ByRef) <$> depsOfType ty
    TypeFun args res       -> concatMap depsOfType args <> depsOfType res
    TypeVoid               -> []
    TypeConstArray _ ty    -> depsOfType ty
    TypeIncompleteArray ty -> depsOfType ty
    TypeExtBinding{}       -> []
