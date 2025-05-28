module HsBindgen.Frontend.AST.Deps (
    Usage(..)
  , ValOrRef(..)
  , depsOfDecl
  , depsOfType
  ) where

import Data.Maybe (maybeToList)

import HsBindgen.Frontend.AST
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definitions
-------------------------------------------------------------------------------}

data Usage =
    UsedInTypedef ValOrRef
  | UsedInField ValOrRef Text
  deriving stock (Show, Eq, Ord)

data ValOrRef = ByValue | ByRef
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Get all dependencies
-------------------------------------------------------------------------------}

depsOfDecl :: forall p. DeclKind p -> [(Usage, QualId p)]
depsOfDecl (DeclStruct fs) =
    concatMap (depsOfField structFieldName structFieldType) fs
depsOfDecl DeclStructOpaque =
    []
depsOfDecl (DeclUnion fs) =
    concatMap (depsOfField unionFieldName unionFieldType) fs
depsOfDecl DeclUnionOpaque =
    []
depsOfDecl (DeclEnum _) =
    []
depsOfDecl DeclEnumOpaque =
    []
depsOfDecl (DeclTypedef ty) =
    map (uncurry aux) $ maybeToList (depsOfTypedef ty)
  where
    aux :: ValOrRef -> QualId p -> (Usage, QualId p)
    aux isPtr uid = (UsedInTypedef isPtr, uid)
depsOfDecl (DeclMacro _ts) =
    -- We cannot know the dependencies of a macro until we parse it, but we
    -- can't parse it until we have sorted all declarations, which requires
    -- knowing the dependencies. Catch-22. We therefore regard macros as not
    -- having /any/ dependencies, and will rely instead on source ordering.
    []

depsOfField :: (a p -> Text) -> (a p -> Type p) -> a p -> [(Usage, QualId p)]
depsOfField getName getType field =
    map (uncurry aux) $ maybeToList (depsOfType $ getType field)
  where
    aux :: ValOrRef -> QualId p -> (Usage, QualId p)
    aux isPtr uid = (UsedInField isPtr (getName field), uid)

depsOfTypedef :: Typedef p -> Maybe (ValOrRef, QualId p)
depsOfTypedef = depsOfType . typedefType

-- | The declarations this type depends on
--
-- We also report whether this dependence is through a pointer or not.
--
-- NOTE: We are only interested in /direct/ dependencies here; transitive
-- dependencies will materialize when we build the graph. That's why this
-- returns at most /one/ dependency.
depsOfType :: Type p -> Maybe (ValOrRef, QualId p)
depsOfType (TypePrim _)           = Nothing
depsOfType (TypeStruct uid)       = Just (ByValue, QualId uid NamespaceStruct)
depsOfType (TypeUnion uid)        = Just (ByValue, QualId uid NamespaceUnion)
depsOfType (TypeEnum uid)         = Just (ByValue, QualId uid NamespaceEnum)
depsOfType (TypeTypedef uid _ann) = Just (ByValue, QualId uid NamespaceTypedef)
depsOfType (TypePointer ty)       = first (const ByRef) <$> depsOfType ty
