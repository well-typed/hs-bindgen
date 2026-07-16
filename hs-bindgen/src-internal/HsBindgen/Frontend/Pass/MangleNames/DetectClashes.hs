module HsBindgen.Frontend.Pass.MangleNames.DetectClashes (
    detectClashes
  ) where

import Data.Either (partitionEithers)
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Maybe (maybeToList)

import Clang.HighLevel.Types

import HsBindgen.Config.Prelims (FieldNamingStrategy (..))
import HsBindgen.Frontend.Pass.MangleNames.CreateNames
import HsBindgen.Frontend.Pass.MangleNames.Error
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.MangleNames.Names
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports ()
import HsBindgen.IR.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Traversal 2: Detect clashes

  Build a single 'NameRegistry' from the 'NameMap' (top-level names and scoped
  names) and the 'CreateNames' AST (local names in annotations), then report all
  collisions in one sweep. Over-removal is explicitly accepted: a single
  declaration may participate in several collisions, and every participating
  owner is dropped.

  Ideally, we'd not purge all conflicts, but instead let selection resolve
  conflict groups. While this is possible, it is also a difficult problem: If we
  deselect a declaration in a confict group, all reverse transitive dependencies
  of the deselected declaration also become unusable. This _may affect other
  conflict groups_, and so is a recursive problem with a fix point.

-------------------------------------------------------------------------------}

detectClashes ::
     forall l.
     FieldNamingStrategy
  -> NameMap
  -> [C.Decl l CreateNames]
  -> (NameRegistry, [MangleNamesFailure])
detectClashes strategy nameMap decls =
    (registry, collisionFailures (collisions registry) ++ deriveFailures)
  where
    registry :: NameRegistry
    deriveFailures :: [MangleNamesFailure]
    (registry, deriveFailures) = Foldable.foldl' aux (emptyNameRegistry, []) decls
      where
        aux (nameRegistry, failures) decl = fmap (++failures) (registerDecl nameRegistry decl)

    registerDecl :: NameRegistry -> C.Decl l CreateNames -> (NameRegistry, [MangleNamesFailure])
    registerDecl reg decl = (,failures) $
        Foldable.foldl'
          (\r (role, scope, loc', sname) -> registerSomeName role scope owner loc' sname r)
          reg0
          successes
      where
        owner :: C.DeclId
        owner = decl.info.id

        loc :: SingleLoc
        loc = decl.info.loc

        -- The declaration's own top-level name. Squashed declarations do not
        -- appear in 'decls', so they are never registered here.
        reg0 :: NameRegistry
        reg0 = case lookupSomeName owner nameMap of
          Just sname -> registerSomeName Declaration GlobalScope owner loc sname reg
          Nothing    -> reg

        (failures, successes) = partitionEithers $ derivedNames strategy nameMap owner loc decl.kind

-- | Looking up created names for scoped names can fail
type DerivedNamesResult = Either MangleNamesFailure (NameRole, Scope, SingleLoc, Hs.SomeName)

-- | All derived (non-top-level) names of a declaration, with the role, scope,
-- and location under which they must be unique.
--
-- Most derived names are blamed on (and located at) the enclosing declaration.
-- Record field selectors are the exception: they carry the field's /own/
-- location, so that a within-record duplicate-field collision points at the
-- offending fields.
derivedNames :: forall l.
     FieldNamingStrategy
  -> NameMap
  -> C.DeclId
  -> SingleLoc
  -> C.DeclKind l CreateNames
  -> [DerivedNamesResult]
derivedNames strategy nameMap owner loc = \case
    C.DeclStruct struct ->
         success (Constructor, GlobalScope, loc, Hs.demoteNs struct.ann.constr)
       : [ success (AuxType, GlobalScope, loc, Hs.demoteNs flamNames.aux)
         | C.Flam _ flamNames <- [struct.flam] ]
      ++ concatMap fieldNames struct.fields
      ++ foldMap explicitFieldNames (C.flamStructField struct.flam)
    C.DeclUnion union ->
         newtypeNames union.ann
      ++ concatMap fieldNames union.fields
    C.DeclEnum enum ->
         newtypeNames enum.ann
      ++ concatMap enumConstantName enum.constants
    -- An anonymous enum constant is a top-level declaration in its own right;
    -- its name is registered as a 'Declaration'. We must /not/ also register
    -- the inner enum-constant scoped name, or it would collide with itself.
    C.DeclAnonEnumConstant{} -> []
    C.DeclTypedef typedef    -> typedefNames typedef.ann
    C.DeclMacro (MacroType macroType) -> newtypeNames macroType.ann
    C.DeclMacro MacroValue{} -> []
    C.DeclFunction{}         -> []
    C.DeclGlobal{}           -> []
    C.DeclOpaque{}           -> []
  where
    success :: (NameRole, Scope, SingleLoc, Hs.SomeName) -> DerivedNamesResult
    success = Right

    failure :: C.DeclId -> C.ScopedName -> DerivedNamesResult
    failure declId scopedName =
        Left $ MangleNamesFailure owner loc $
          MangleNamesCollisionError $ DetectClashesScopedNameNotMangled declId scopedName

    -- Field selectors created under 'OmitFieldPrefixes' enable
    -- @DuplicateRecordFields@, so they need only be unique within a record.
    fieldScope :: Scope
    fieldScope = case strategy of
      AddFieldPrefixes  -> GlobalScope
      OmitFieldPrefixes -> DeclScope owner

    fieldNames :: C.Field CreateNames -> [DerivedNamesResult]
    fieldNames = C.elimField explicitFieldNames implicitFieldNames

    explicitFieldNames :: C.ExplicitField CreateNames -> [DerivedNamesResult]
    explicitFieldNames field =
        case lookupScopedName owner field.info.name nameMap of
          Nothing     -> [ failure owner field.info.name ]
          Just hsName -> [ success (Field, fieldScope, field.info.loc, hsName) ]

    implicitFieldNames :: C.ImplicitField CreateNames -> [DerivedNamesResult]
    implicitFieldNames field =
        case lookupScopedName owner field.info.name nameMap of
          Nothing     -> [ failure owner field.info.name ]
          Just hsName -> [ success (Field, fieldScope, field.info.loc, hsName) ]

    enumConstantName :: C.EnumConstant CreateNames -> [DerivedNamesResult]
    enumConstantName constant =
        case lookupScopedName owner constant.info.name nameMap of
          Nothing     -> [ failure owner constant.info.name ]
          Just hsName -> [ success (Constructor, GlobalScope, constant.info.loc, hsName) ]

    -- The newtype "unwrap" field is recorded only under 'AddFieldPrefixes',
    -- where it is the globally-unique @unwrap<TypeName>@. Under
    -- 'OmitFieldPrefixes' it is simply @unwrap@, lives alone in its own record,
    -- and so can never collide.
    newtypeNames :: NewtypeNames -> [DerivedNamesResult]
    newtypeNames n =
        success (Constructor, GlobalScope, loc, Hs.demoteNs n.dataConstr)
      : [ success (Field, GlobalScope, loc, Hs.demoteNs n.field)
        | AddFieldPrefixes <- [strategy] ]

    typedefNames :: TypedefNames -> [DerivedNamesResult]
    typedefNames t =
         newtypeNames t.orig
      ++ concat [ success (AuxType, GlobalScope, loc, Hs.demoteNs auxName) : newtypeNames auxNames
                | (auxName, auxNames) <- maybeToList t.aux ]

-- | Turn registry collisions into per-declaration failures
--
-- Each 'Collision' produced by 'collisions' lives in a single 'Scope'. A
-- collision within a 'DeclScope' is a set of record field selectors of one
-- declaration that mangle to the same Haskell name (only possible under
-- 'OmitFieldPrefixes'); we report it as a more specific
-- 'DetectClashesDuplicateFieldName'. All other collisions are reported as
-- 'DetectClashesCollision', one failure per participating declaration.
collisionFailures :: [Collision] -> [MangleNamesFailure]
collisionFailures = concatMap getCollisionFailures
  where
    getCollisionFailures :: Collision -> [MangleNamesFailure]
    getCollisionFailures c = case c.origins of
        o:_ | DeclScope{} <- o.scope ->
          -- Within-record duplicate field selector. All origins share the same
          -- owner (the enclosing record), so this yields a single failure.
          [ MangleNamesFailure o.owner o.loc $ MangleNamesCollisionError $
              DetectClashesDuplicateFieldName c.name (map (.loc) c.origins)
          ]
        _otherwise ->
          [ MangleNamesFailure o.owner o.loc $ MangleNamesCollisionError $
              DetectClashesCollision c.name idsWithLocs
          | o <- origins
          ]
      where
        -- One failure per distinct declaration.
        origins :: [NameOrigin]
        origins = Map.elems $ Map.fromList $ map (\o -> (o.owner, o)) c.origins

        idsWithLocs :: [C.WithLocationInfo C.DeclId]
        idsWithLocs =
          [ C.WithLocationInfo (C.declIdLocationInfo o.owner [o.loc]) o.owner
          | o <- origins
          ]
