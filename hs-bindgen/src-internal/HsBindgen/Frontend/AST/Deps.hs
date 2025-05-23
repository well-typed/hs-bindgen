module HsBindgen.Frontend.AST.Deps (
    Usage(..)
  , ValOrRef(..)
  , depsOfDecl
  , depsOfType
  ) where

import Data.Maybe (maybeToList)

import HsBindgen.Frontend.AST
import HsBindgen.Frontend.Pass
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

depsOfDecl :: forall p. DeclKind p -> [(Usage, Id p)]
depsOfDecl (DeclStruct fs) =
    concatMap depsOfField fs
depsOfDecl DeclStructOpaque =
    []
depsOfDecl (DeclTypedef ty) =
    map (uncurry aux) $ maybeToList (depsOfTypedef ty)
  where
    aux :: ValOrRef -> Id p -> (Usage, Id p)
    aux isPtr uid = (UsedInTypedef isPtr, uid)
depsOfDecl (DeclMacro _ts) =
    -- We cannot know the dependencies of a macro until we parse it, but we
    -- can't parse it until we have sorted all declarations, which requires
    -- knowing the dependencies. Catch-22. We therefore regard macros as not
    -- having /any/ dependencies, and will rely instead on source ordering.
    []

depsOfField :: forall p. Field p -> [(Usage, Id p)]
depsOfField Field{fieldName, fieldType} =
    map (uncurry aux) $ maybeToList (depsOfType fieldType)
  where
    aux :: ValOrRef -> Id p -> (Usage, Id p)
    aux isPtr uid = (UsedInField isPtr fieldName, uid)

depsOfTypedef :: Typedef p -> Maybe (ValOrRef, Id p)
depsOfTypedef = depsOfType . typedefType

-- | The declarations this type depends on
--
-- We also report whether this dependence is through a pointer or not.
--
-- NOTE: We are only interested in /direct/ dependencies here; transitive
-- dependencies will materialize when we build the graph. That's why this
-- returns at most /one/ dependency.
depsOfType :: Type p -> Maybe (ValOrRef, Id p)
depsOfType (TypePrim _)           = Nothing
depsOfType (TypeStruct  uid)      = Just (ByValue, uid)
depsOfType (TypeTypedef uid _ann) = Just (ByValue, uid)
depsOfType (TypePointer ty)       = first (const ByRef) <$> depsOfType ty
