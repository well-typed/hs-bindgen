module HsBindgen.Frontend.Pass.TypecheckMacros.KnownTypes (
    collectKnownTypes
  ) where

import Data.Map qualified as Map

import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass

type In    = ConstructTranslationUnit
type Out   = TypecheckMacros
type CType = C.Type Out

-- | Collect typedef and tagged-type mappings from all non-macro declarations in
-- the translation unit.
collectKnownTypes :: [C.Decl l In] -> Map C.DeclId CType
collectKnownTypes decls = Map.fromList $ mapMaybe getKnownType decls

getKnownType :: C.Decl l In -> Maybe (C.DeclId, CType)
getKnownType decl = case decl.kind of
    -- We do not know the types of macros, yet.
    C.DeclMacro{}            -> Nothing
    C.DeclTypedef typedef    -> Just $ (declId, getKnownTypedef typedef)
    C.DeclStruct{}           -> Just $ (declId,  knownStructOrUnion)
    C.DeclUnion{}            -> Just $ (declId,  knownStructOrUnion)
    C.DeclEnum enum          -> Just $ (declId,  getKnownEnum enum)
    C.DeclAnonEnumConstant{} -> Nothing
    -- Include opaque tagged types: they can be referenced by macros (e.g. as a
    -- pointer type). The only opaque type-like declarations carrying an
    -- ordinary name are @typedef void foo@, which is not a valid standalone
    -- type, so macros using them will fail anyway.
    C.DeclOpaque{}           -> getKnownTaggedTypeOpaque
    C.DeclFunction{}         -> Nothing
    C.DeclGlobal{}           -> Nothing
  where
    info :: C.DeclInfo In
    info = decl.info

    getKnownTaggedTypeOpaque :: Maybe (C.DeclId, CType)
    getKnownTaggedTypeOpaque =
      case info.id.name.kind of
        C.NameKindTagged _ -> Just (declId, knownStructOrUnion)
        _                  -> Nothing

    declId :: C.DeclId
    declId = info.id

    getKnownTypedef :: C.Typedef In -> CType
    getKnownTypedef typedef = C.TypeTypedef $ C.Ref info.id $ coercePass typedef.typ

    knownStructOrUnion :: CType
    knownStructOrUnion = C.TypeRef info.id

    getKnownEnum :: C.Enum In -> CType
    getKnownEnum enum = C.TypeEnum $ C.Ref info.id $ coercePass enum.typ
