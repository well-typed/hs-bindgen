module HsBindgen.Frontend.Pass.MangleNames (
    mangleNames
  ) where

import Data.Map qualified as Map
import Data.Set qualified as Set

import HsBindgen.Config.MangleCandidate (MangleCandidate (..))
import HsBindgen.Config.MangleCandidate qualified as MangleCandidate
import HsBindgen.Config.Prelims (FieldNamingStrategy)
import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.Typedefs qualified as TypedefAnalysis
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Frontend.DelayedMsg
import HsBindgen.Frontend.Pass.MangleNames.CreateNames
import HsBindgen.Frontend.Pass.MangleNames.DetectClashes
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.MangleNames.IsPass.Msg
import HsBindgen.Frontend.Pass.MangleNames.Names
import HsBindgen.Frontend.Pass.MangleNames.ResolveNames
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.IR.Translation (DeclIdPair (..))
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
     in annotations) and reports collisions.

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

mangleNames ::
     forall l. (HasCallStack, Macro.HasTypes l)
  => FieldNamingStrategy
  -> C.TranslationUnit l ResolveBindingSpecs
  -> (C.TranslationUnit l MangleNames, [AnnMsg MangleNames])
mangleNames fieldNaming unit = (
      unit{C.db =
          DeclDB.addDelayedMsgs (map (second DelayedMangleNames) msgs1) $
            runIdentity $
              DeclDB.traverseActive DeclDB.AnyOrder (pure . result) unit.db
        }
    , catMaybes [
          Just $ debugMsg $ MangleNamesNameMap nameMap
        , bugMsg . MangleNamesNameMapDuplicates <$> nameMapDups
        , Just $ debugMsg $ MangleNamesNameRegistry registry
        ]
    )
  where
    decls :: [C.Decl l ResolveBindingSpecs]
    decls = DeclDB.active DeclDB.AnyOrder unit.db

    typedefAnalysis :: TypedefAnalysis.TypedefAnalysis
    typedefAnalysis = TypedefAnalysis.fromDecls unit.db.declUseGraph decls

    mangleCandidateConfig :: MangleCandidate Maybe
    mangleCandidateConfig = MangleCandidate.mangleCandidateDefault

    -- Traversal 1: choose top-level names and within-declaration names.
    declsC      :: [C.Decl l CreateNames]
    squashes    :: [(C.DeclId, Hs.Name Hs.NsTypeConstr, TypedefAnalysis.Squash)]
    nameMap     :: NameMap
    nameMapDups :: Maybe (NonEmpty DupAssign)
    failures1   :: [MangleNamesFailure]
    msgs1       :: [(C.DeclId, DelayedMangleNamesMsg)]
    (declsC, squashes, nameMap, nameMapDups, failures1, msgs1) =
      createNames typedefAnalysis mangleCandidateConfig fieldNaming decls

    -- Traversal 2: detect clashes among all names.
    registry  :: NameRegistry
    failures2 :: [MangleNamesFailure]
    (registry, failures2) = detectClashes fieldNaming nameMap declsC

    dropped :: Set C.DeclId
    dropped = Set.fromList $ map (.id) failures2

    -- Traversal 3: resolve references.
    decls3    :: [C.Decl l MangleNames]
    failures3 :: [MangleNamesFailure]
    (decls3, failures3) = resolveNames nameMap dropped declsC

    results :: Map C.DeclId (DeclDB.PassResult l MangleNames)
    results = mangleResults decls3 squashes (failures1 ++ failures2 ++ failures3)

    result :: C.Decl l ResolveBindingSpecs -> DeclDB.PassResult l MangleNames
    result decl = case Map.lookup decl.id results of
      Just r  -> r
      Nothing -> panicPure $ "mangleNames: no result for " ++ show decl.id

-- | Panics if a declaration has more than one result
mangleResults ::
      HasCallStack
   => [C.Decl l MangleNames]
   -> [(C.DeclId, Hs.Name Hs.NsTypeConstr, TypedefAnalysis.Squash)]
   -> [MangleNamesFailure]
   -> Map C.DeclId (DeclDB.PassResult l MangleNames)
mangleResults decls squashes failures =
    Map.fromListWithKey duplicate $ concat [
      [ (decl.id.cName, DeclDB.Advanced decl)
      | decl <- decls
      ]
    , [ (cName, DeclDB.Squash DeclDB.SquashTarget{c = s.targetId, hs = hsName})
      | (cName, hsName, s) <- squashes
      ]
      -- A declaration may fail more than once, e.g. in several clashes; the
      -- last failure is its reason. The failures of one declaration are in an
      -- order independent of the order of the declarations.
    , Map.toList $ Map.fromList [
          (f.id, DeclDB.Fail $ DeclDB.MangleNamesFailed f.err)
        | f <- failures
        ]
    ]
  where
    duplicate :: C.DeclId -> a -> a -> a
    duplicate declId _ _ =
        panicPure $ "mangleResults: more than one result for " ++ show declId

debugMsg :: MangleNamesMsg -> AnnMsg MangleNames
debugMsg = withCallStack . C.WithLocationInfo C.LocationUnavailable

bugMsg :: MangleNamesMsg -> AnnMsg MangleNames
bugMsg = withCallStack . C.WithLocationInfo C.LocationUnavailable
