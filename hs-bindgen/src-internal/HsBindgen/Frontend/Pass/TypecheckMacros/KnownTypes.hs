module HsBindgen.Frontend.Pass.TypecheckMacros.KnownTypes (
    collectKnownTypes
  ) where

import Data.Either (partitionEithers)
import Data.Map qualified as Map

import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports

type Pre   = ConstructTranslationUnit
type Post  = TypecheckMacros
type CType = C.Type Post

-- | Collect typedef and tagged-type mappings from all non-macro declarations in
-- the translation unit.
collectKnownTypes ::
     [C.Decl Pre]
  -> (Map Text CType, Map CDeclName CType)
collectKnownTypes decls =
    bimap Map.fromList Map.fromList $
      partitionEithers $
        mapMaybe getKnownType decls

getKnownType ::
     C.Decl Pre
  -> Maybe (Either (Text, CType) (CDeclName, CType))
     -- ^ Left:  Typedef name -> CType
     --   Right: Tagged name  -> CType
getKnownType decl = case decl.kind of
    C.DeclMacro{}            -> Nothing
    C.DeclTypedef typedef    -> Just $ Left  (typedefName, getKnownTypedef typedef)
    C.DeclStruct{}           -> Just $ Right (taggedName,  knownStructOrUnion)
    C.DeclUnion{}            -> Just $ Right (taggedName,  knownStructOrUnion)
    C.DeclEnum enum          -> Just $ Right (taggedName,  getKnownEnum enum)
    C.DeclAnonEnumConstant{} -> Nothing
    -- Include opaque tagged types: they can be referenced by macros (e.g. as a
    -- pointer type). The only opaque type-like declarations carrying an
    -- ordinary name are @typedef void foo@, which is not a valid standalone
    -- type, so macros using them will fail anyway.
    C.DeclOpaque{}           -> Right <$> getKnownTypeOpaque
    C.DeclFunction{}         -> Nothing
    C.DeclGlobal{}           -> Nothing
  where
    info :: C.DeclInfo Pre
    info = decl.info

    getKnownTypeOpaque :: Maybe (CDeclName, CType)
    getKnownTypeOpaque =
      case info.id.name.kind of
        CNameKindTagged _ -> Just (taggedName, knownStructOrUnion)
        _                 -> Nothing

    -- Typedef names are plain text (always 'CNameKindOrdinary').
    typedefName :: Text
    typedefName = info.id.name.text

    -- Tagged names carry their tag kind ('CNameKindTagged').
    taggedName :: CDeclName
    taggedName = info.id.name

    getKnownTypedef :: C.Typedef Pre -> CType
    getKnownTypedef typedef = C.TypeTypedef $ C.Ref info.id $ coercePass typedef.typ

    knownStructOrUnion :: CType
    knownStructOrUnion = C.TypeRef info.id

    getKnownEnum :: C.Enum Pre -> CType
    getKnownEnum enum = C.TypeEnum $ C.Ref info.id $ coercePass enum.typ
