-- | Invariants of the declaration database, and its transitions between passes
module Test.HsBindgen.Frontend.DeclDB (
    -- * Checks
    Target(..)
  , Violation(..)
  , checkTransition
  , checkGraph
    -- * Frontend
  , checkFrontend
  ) where

import Data.Digraph qualified as Digraph
import Data.Map.Strict qualified as Map
import Data.Set ((\\))
import Data.Set qualified as Set

import HsBindgen.Artefact (Artefact (..), FrontendPass (..))
import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph)
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.DeclDB (Deactivation (..), DeclDB (..), Entry (..),
                                  Inactive (..), InactiveInfo (..),
                                  InactiveReason (..), UnusableReason (..))
import HsBindgen.Frontend.DeclMeta (DeclMeta (..))
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass (Id)

{-------------------------------------------------------------------------------
  Checks
-------------------------------------------------------------------------------}

-- | What an entry may become between two consecutive databases
data Target =
    ToDeactivated Deactivation
  | ToSquashed
  | ToExternal

data Violation =
    -- | An active declaration whose identifier is not its key
    ActiveIdNotKey C.DeclId C.DeclId
  | DelayedMsgsWithoutEntry C.DeclId
  | VertexWithoutEntry C.DeclId
  | EntryWithoutVertex C.DeclId
    -- | An entry of the earlier database only
  | EntryDisappeared C.DeclId
    -- | An entry of the later database only
  | EntryAppeared C.DeclId
  | ForbiddenTransition C.DeclId String String
  deriving stock (Show)

-- | Check the invariants of both databases, and the transitions between them
checkTransition ::
     forall l p p'. (Id p ~ C.DeclId, Id p' ~ C.DeclId)
  => (Target -> Bool)
  -> DeclDB l p
  -> DeclDB l p'
  -> [Violation]
checkTransition allowed old new = concat [
      checkInvariants old
    , checkInvariants new
    , map EntryDisappeared $ Set.toList $ oldKeys \\ newKeys
    , map EntryAppeared    $ Set.toList $ newKeys \\ oldKeys
    , concat $ Map.elems $
        Map.intersectionWithKey checkEntry old.entries new.entries
    ]
  where
    oldKeys, newKeys :: Set C.DeclId
    oldKeys = Map.keysSet old.entries
    newKeys = Map.keysSet new.entries

    checkEntry :: C.DeclId -> Entry l p -> Entry l p' -> [Violation]
    checkEntry declId x y
      | isAllowed x y = []
      | otherwise     = [ForbiddenTransition declId (describe x) (describe y)]

    isAllowed :: Entry l p -> Entry l p' -> Bool
    isAllowed x y = case (x, y) of
      (Active{}, Active{}) -> True
      (Active{}, Inactive (InactiveDecl InactiveInfo{reason = Deactivated d})) ->
        allowed (ToDeactivated d)
      (Active{}  , Squashed{}) -> allowed ToSquashed
      (Active{}  , External{}) -> allowed ToExternal
      (Inactive{}, External{}) -> allowed ToExternal
      (Active{}, _) -> False
      -- An entry that is not active is pass-independent: it stays as it is
      (_, Active{}) -> False
      _otherwise    -> describe x == describe y

-- | Everything but the declaration of an active entry
describe :: Entry l p -> String
describe = \case
    Active{}   -> "Active"
    Squashed x -> "Squashed " ++ show x
    External x -> "External " ++ show x
    Inactive x -> "Inactive " ++ show x

-- | Invariants of a single database
checkInvariants :: Id p ~ C.DeclId => DeclDB l p -> [Violation]
checkInvariants db = concat [
      [ ActiveIdNotKey declId decl.id
      | (declId, Active decl) <- Map.toList db.entries
      , decl.id /= declId
      ]
    , map DelayedMsgsWithoutEntry $ Set.toList $
        Map.keysSet db.delayedMsgs \\ Map.keysSet db.entries
    ]

-- | The vertices of the decl-use graph are the keys of the database
checkGraph :: DeclDB l p -> DeclUseGraph -> [Violation]
checkGraph db graph = concat [
      map VertexWithoutEntry $ Set.toList $ vertices \\ keys
    , map EntryWithoutVertex $ Set.toList $ keys \\ vertices
    ]
  where
    keys, vertices :: Set C.DeclId
    keys     = Map.keysSet db.entries
    vertices = Set.fromList $ Digraph.vertices $ DeclUseGraph.toDigraph graph

{-------------------------------------------------------------------------------
  Frontend
-------------------------------------------------------------------------------}

-- | Check the databases of a frontend run, pair by pair
--
-- @translateTypes@ has no 'FrontendPass'; the pair @adjustTypes@ to @select@
-- spans it, as @applyMacroFallback@ to @reparseMacroExpansions@ spans
-- @PrepareReparse@. Allowed transitions compose.
checkFrontend :: Artefact l [String]
checkFrontend = do
    constructTranslationUnit <- FrontendPassA ConstructTranslationUnitPass
    typecheckMacros          <- FrontendPassA TypecheckMacrosPass
    applyMacroFallback       <- FrontendPassA ApplyMacroFallbackPass
    reparseMacroExpansions   <- FrontendPassA ReparseMacroExpansionsPass
    resolveBindingSpecs      <- FrontendPassA ResolveBindingSpecsPass
    mangleNames              <- FrontendPassA MangleNamesPass
    adjustTypes              <- FrontendPassA AdjustTypesPass
    select                   <- FrontendPassA SelectPass
    final                    <- FrontendPassA FinalPass
    pure $ concat [
        pair "constructTranslationUnit" "typecheckMacros" none
          constructTranslationUnit typecheckMacros
      , pair "typecheckMacros" "applyMacroFallback" (\case
            ToDeactivated (Unusable UnusableMacro{}) -> True
            _otherwise                               -> False)
          typecheckMacros applyMacroFallback
      , pair "applyMacroFallback" "reparseMacroExpansions" none
          applyMacroFallback reparseMacroExpansions
      , pair "reparseMacroExpansions" "resolveBindingSpecs" (\case
            ToDeactivated Omitted -> True
            ToExternal            -> True
            _otherwise            -> False)
          reparseMacroExpansions resolveBindingSpecs
      , pair "resolveBindingSpecs" "mangleNames" (\case
            ToDeactivated (Unusable UnusableMangleNames{}) -> True
            ToSquashed                                     -> True
            _otherwise                                     -> False)
          resolveBindingSpecs mangleNames
      , pair "mangleNames" "adjustTypes" none
          mangleNames adjustTypes
      , pair "adjustTypes" "select" (\case
            ToDeactivated Deselected -> True
            _otherwise               -> False)
          adjustTypes select
      , pair "select" "final" none
          select final
      ]
  where
    none :: Target -> Bool
    none = const False

    pair ::
         String
      -> String
      -> (Target -> Bool)
      -> C.TranslationUnit l p
      -> C.TranslationUnit l p'
      -> [String]
    pair nameOld nameNew allowed old new = [
          nameOld ++ " -> " ++ nameNew ++ ": " ++ show violation
        | violation <- concat [
              checkGraph old.meta.declDB old.meta.declUseGraph
            , checkGraph new.meta.declDB new.meta.declUseGraph
            , checkTransition allowed old.meta.declDB new.meta.declDB
            ]
        ]
