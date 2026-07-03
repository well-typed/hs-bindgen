module HsBindgen.Frontend.Pass.MangleNames (
    mangleNames
  ) where

import Data.Map qualified as Map
import Data.Set qualified as Set

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

  1. 'createNames' chooses top-level names and all within-declaration
     names (annotations and 'ScopedNamePair's), producing a 'CreateNames'
     AST whose 'Id' references are still unresolved. It also produces the
     'NameMap' and squash records.

  2. 'detectClashes' assembles a single 'NameRegistry' from the 'NameMap'
     (top-level names) and the 'CreateNames' AST (derived names) and reports
     collisions.

  3. 'resolveNames' rewrites 'C.DeclId' references into 'DeclIdPair's using the
     'NameMap', copying annotations and scoped names across unchanged, and
     dropping the declarations flagged by 'detectClashes'. This produces the
     final 'MangleNames' AST.

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
         C.TranslationUnit{
           decls        = decls3
         , includeGraph = unit.includeGraph
         , meta         = updateDeclMeta
                            (failures1 ++ failures2 ++ failures3)
                            squashes
                            unit.meta
        }
    , msgs1 ++ [
          debugMsg $ MangleNamesNameMap nameMap
        , debugMsg $ MangleNamesNameRegistry registry
        ]
    )
  where
    typedefAnalysis :: TypedefAnalysis.TypedefAnalysis
    typedefAnalysis = TypedefAnalysis.fromDecls unit.meta.declUseGraph unit.decls

    mangleCandidateConfig :: MangleCandidate Maybe
    mangleCandidateConfig = MangleCandidate.mangleCandidateDefault

    -- Traversal 1: choose top-level names and within-declaration names.
    declsC    :: [C.Decl l CreateNames]
    squashes  :: [(C.DeclId, Hs.Name Hs.NsTypeConstr, TypedefAnalysis.Squash)]
    nameMap   :: NameMap
    failures1 :: [MangleNamesFailure]
    msgs1     :: [AnnMsg MangleNames]
    (declsC, squashes, nameMap, failures1, msgs1) =
      createNames typedefAnalysis mangleCandidateConfig fieldNaming unit.decls

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
    failuresMap :: Map C.DeclId (SingleLoc, MangleNamesError)
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
