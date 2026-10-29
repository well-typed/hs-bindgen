module HsBindgen.Frontend.Pass.MangleNames (
    mangleNames
  , ModulePlacement(..)
  , TypeNameSpelling(..)
  , splitQualifiedTypeName
  ) where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

import Clang.HighLevel.Types

import HsBindgen.Config.MangleCandidate (MangleCandidate (..))
import HsBindgen.Config.MangleCandidate qualified as MangleCandidate
import HsBindgen.Config.Prelims (FieldNamingStrategy)
import HsBindgen.Frontend.Analysis.DeclIndex (Squashed (..))
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.Typedefs qualified as TypedefAnalysis
import HsBindgen.Frontend.DeclMeta
import HsBindgen.Frontend.Pass.MangleNames.CreateNames
import HsBindgen.Frontend.Pass.MangleNames.DetectClashes
import HsBindgen.Frontend.Pass.MangleNames.Error
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.MangleNames.Names
import HsBindgen.Frontend.Pass.MangleNames.ResolveNames
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Macro.Type qualified as Macro
import HsBindgen.Util.Tracer (withCallStack)

{-------------------------------------------------------------------------------
  Top-level

  Name mangling proceeds in three traversals:

  1. 'createNames' chooses top-level names and all within-declaration names
     (annotations and 'ScopedNamePair's), producing a 'CreateNames' AST whose
     'Id' and 'ScopedName' references are still unresolved. It also produces the
     'NameMap' and squash records.

  2. 'detectClashes' assembles a single 'NameRegistry' from the 'NameMap'
     (top-level names and scoped names) and the 'CreateNames' AST (local names
     in annotations) and reports collisions. It runs once for every set of
     declarations that end up in the same modules (see 'ModulePlacement').

  3. 'resolveNames' rewrites 'C.DeclId' references into 'DeclIdPair's and
     'C.ScopedName' references into 'ScopedNamePair' using the 'NameMap',
     copying annotations across unchanged, and dropping the declarations flagged
     by 'detectClashes'. This produces the final 'MangleNames' AST.

  Importantly, name mangling can fail (Traversal 1), and names can conflict
  (Traversal 2), ultimately leading to dangling references (Traversal 3) in
  declarations referring to other declarations that have failed or are
  conflicting.

  In general, we commit to making /local decisions/, not caring about global
  consequences (i.e., the fate of reverse transitive dependencies). In
  particular, in Traversal 3, if we encounter a dangling reference, we mark the
  declaration we currently handle as /unusable/.

  /It is the job of selection to compute the consequences of all of these local
  decisions/.
-------------------------------------------------------------------------------}

-- | Which generated modules each declaration ends up in
--
-- Two declarations need different Haskell names only if some generated module
-- sees both of them unqualified. A types module and its term modules (@Safe@,
-- @Unsafe@ and so on), which import it unqualified, see each other's names.
-- Generated code imports every other module only qualified.
data ModulePlacement =
    -- | All declarations end up in the same modules
    --
    -- This is every run without @--library@.
    AllTogether
    -- | Each declaration ends up with these types modules
    --
    -- That is, in one of these types modules or in one of their term modules.
    -- With @--library@ every header has its own types module. A type is
    -- generated in one of them, and a value (such as a macro) with every one
    -- that needs it. A declaration missing from the map is generated with
    -- none, so its names are not checked.
  | ByTypesModule TypeNameSpelling (Map C.DeclId (Set Hs.ModuleName))
  deriving stock (Show)

-- | How to spell the names of types under 'ByTypesModule'
data TypeNameSpelling =
    -- | As the generated code writes them, such as @Point@
    PlainTypeNames

    -- | With their types module, such as @Lib.Geo.Point.Point@
    --
    -- Two types modules can each have a @Point@, so plain names of types are
    -- unique only within a types module. Spelled with their types module, they
    -- are unique across modules, which is what the backend needs when it
    -- translates all modules at once (to generate their binding
    -- specifications). Such a translation cannot be rendered.
    --
    -- Only the names of types are spelled this way. The names derived from
    -- them, such as constructors and fields, stay plain, and so does the name
    -- of a type that does not end up with exactly one types module.
    -- 'splitQualifiedTypeName' takes a name apart again.
  | QualifiedTypeNames
  deriving stock (Show)

mangleNames ::
     forall l. (HasCallStack, Macro.HasTypes l)
  => FieldNamingStrategy
  -> ModulePlacement
  -> C.TranslationUnit l ResolveBindingSpecs
  -> (C.TranslationUnit l MangleNames, [AnnMsg MangleNames])
mangleNames fieldNaming placement unit = (
         C.TranslationUnit{
           decls        = decls3
         , includeGraph = unit.includeGraph
         , meta         = updateDeclMeta
                            (failures1 ++ failures2 ++ failures3)
                            squashes
                            unit.meta
        }
    , msgs1 ++ catMaybes [
          Just $ debugMsg $ MangleNamesNameMap nameMap
        , bugMsg . MangleNamesNameMapDuplicates <$> nameMapDups
        ]
      ++ map (debugMsg . MangleNamesNameRegistry) registries
    )
  where
    typedefAnalysis :: TypedefAnalysis.TypedefAnalysis
    typedefAnalysis =
      TypedefAnalysis.fromDecls
        (placedTogether placement)
        unit.meta.declUseGraph
        unit.decls

    mangleCandidateConfig :: MangleCandidate Maybe
    mangleCandidateConfig = MangleCandidate.mangleCandidateDefault

    -- Traversal 1: choose top-level names and within-declaration names.
    declsC      :: [C.Decl l CreateNames]
    squashes    :: [(C.DeclId, Hs.Name Hs.NsTypeConstr, TypedefAnalysis.Squash)]
    nameMap     :: NameMap
    nameMapDups :: Maybe (NonEmpty DupAssign)
    failures1   :: [MangleNamesFailure]
    msgs1       :: [AnnMsg MangleNames]
    (declsC, squashes, nameMap, nameMapDups, failures1, msgs1) =
      createNames
        typedefAnalysis
        mangleCandidateConfig
        fieldNaming
        (spellTypeName placement)
        unit.decls

    -- Traversal 2: detect clashes among the names of the declarations that end
    -- up in the same modules.
    registries :: [NameRegistry]
    failures2  :: [MangleNamesFailure]
    (registries, failures2) = second concat $ unzip [
        detectClashes fieldNaming nameMap together
      | together <- declsSharingModules placement declsC
      ]

    dropped :: Set C.DeclId
    dropped = Set.fromList $ map (.id) failures2

    -- Traversal 3: resolve references.
    decls3    :: [C.Decl l MangleNames]
    failures3 :: [MangleNamesFailure]
    (decls3, failures3) = resolveNames nameMap dropped declsC

-- | Do two declarations end up in some common module?
placedTogether :: ModulePlacement -> C.DeclId -> C.DeclId -> Bool
placedTogether AllTogether               _ _ = True
placedTogether (ByTypesModule _ modules) x y =
    not $ Set.disjoint (typesModulesOf modules x) (typesModulesOf modules y)

-- | The declarations that end up in the same modules, one list per types
-- module
--
-- A declaration is in the list of every types module it ends up with.
declsSharingModules ::
     ModulePlacement
  -> [C.Decl l CreateNames]
  -> [[C.Decl l CreateNames]]
declsSharingModules AllTogether               decls = [decls]
declsSharingModules (ByTypesModule _ modules) decls =
    -- One pass over the declarations, which go onto their lists in reverse
    map reverse $ Map.elems $ Map.fromListWith (++) [
        (typesModule, [decl])
      | decl        <- decls
      , typesModule <- Set.toList (typesModulesOf modules decl.info.id)
      ]

typesModulesOf ::
     Map C.DeclId (Set Hs.ModuleName)
  -> C.DeclId
  -> Set Hs.ModuleName
typesModulesOf modules declId = Map.findWithDefault Set.empty declId modules

-- | Spell the name of the type of a declaration (see 'TypeNameSpelling')
spellTypeName ::
     ModulePlacement
  -> C.DeclId
  -> Hs.Name Hs.NsTypeConstr
  -> Hs.Name Hs.NsTypeConstr
spellTypeName = \case
    ByTypesModule QualifiedTypeNames modules -> \declId name ->
      case Set.toList (typesModulesOf modules declId) of
        [typesModule] -> qualifyTypeName typesModule name
        _otherwise    -> name
    ByTypesModule PlainTypeNames _ -> const id
    AllTogether                    -> const id

qualifyTypeName ::
     Hs.ModuleName
  -> Hs.Name Hs.NsTypeConstr
  -> Hs.Name Hs.NsTypeConstr
qualifyTypeName typesModule name =
    Hs.UnsafeName $ typesModule.text <> "." <> name.text

-- | The types module and the plain name of a type spelled with
-- 'QualifiedTypeNames'
--
-- 'Nothing' for a plain name. The module is everything before the last dot: a
-- plain name has none, since the mangler escapes a dot like every other
-- character that is invalid in a name.
splitQualifiedTypeName ::
     Hs.Name Hs.NsTypeConstr
  -> Maybe (Hs.ModuleName, Hs.Name Hs.NsTypeConstr)
splitQualifiedTypeName name =
    case Text.breakOnEnd "." name.text of
      ("",        _    ) -> Nothing
      (qualifier, plain) -> Just (
          Hs.ModuleName (Text.dropEnd 1 qualifier)
        , Hs.UnsafeName plain
        )

updateDeclMeta ::
      [MangleNamesFailure]
   -> [(C.DeclId, Hs.Name Hs.NsTypeConstr, TypedefAnalysis.Squash)]
   -> DeclMeta l
   -> DeclMeta l
updateDeclMeta failures squashes declMeta = declMeta{
      declIndex =
        DeclIndex.registerMangleNamesFailure failuresMap $
        DeclIndex.registerSquashedDeclarations squashesMap $
          declMeta.declIndex
    }
  where
    failuresMap :: Map C.DeclId (SingleLoc C.DeclPath, MangleNamesError)
    failuresMap = Map.fromList $ map (\f -> (f.id, (f.loc, f.err))) failures

    squashesMap ::  Map C.DeclId Squashed
    squashesMap = Map.fromList $ map toSquashed squashes

    toSquashed ::
         (C.DeclId, Hs.Name Hs.NsTypeConstr, TypedefAnalysis.Squash)
      -> (C.DeclId, Squashed)
    toSquashed (cName, hsName, s) = (
        cName
      , Squashed {
          typedefLoc   = s.typedefLoc
        , targetNameC  = s.targetId
        , targetNameHs = hsName
        }
      )


debugMsg :: MangleNamesMsg -> AnnMsg MangleNames
debugMsg = withCallStack . C.WithLocationInfo C.LocationUnavailable

bugMsg :: MangleNamesMsg -> AnnMsg MangleNames
bugMsg = withCallStack . C.WithLocationInfo C.LocationUnavailable
