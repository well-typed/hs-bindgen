-- | Invariants of the declaration database, and its transitions between passes
module Test.HsBindgen.Frontend.DeclDB (
    -- * Checks
    Target(..)
  , Violation(..)
  , checkTransition
  , checkKeysDeclId
  , checkKeysDeclIdPair
  , checkDeclsDeclId
  , checkDeclsDeclIdPair
    -- * Frontend
  , checkFrontend
  ) where

import Data.Digraph qualified as Digraph
import Data.Map.Strict qualified as Map
import Data.Set ((\\))
import Data.Set qualified as Set

import HsBindgen.Artefact (Artefact (..), FrontendPass (..))
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.DeclDB (Deactivation (..), DeclDB (..), Entry (..),
                                  Inactive (..), InactiveInfo (..),
                                  InactiveReason (..), UnusableReason (..))
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass (Id)
import HsBindgen.IR.Translation (DeclIdPair (..))

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
    -- | A declaration of the unit without an active entry
  | DeclNotActive C.DeclId
    -- | An active entry without a declaration in the unit
  | ActiveNotDecl C.DeclId
    -- | A declaration that occurs more than once in the unit
  | DuplicateDecl C.DeclId
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
     forall l p p'.
     (Target -> Bool)
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

-- | Invariants of a single database, but for the identifiers of active
-- declarations ('checkKeysDeclId', 'checkKeysDeclIdPair')
checkInvariants :: DeclDB l p -> [Violation]
checkInvariants db = concat [
      map DelayedMsgsWithoutEntry $ Set.toList $
        Map.keysSet db.delayedMsgs \\ keys
    , map VertexWithoutEntry $ Set.toList $ vertices \\ keys
    , map EntryWithoutVertex $ Set.toList $ keys \\ vertices
    ]
  where
    keys, vertices :: Set C.DeclId
    keys     = Map.keysSet db.entries
    vertices =
      Set.fromList $ Digraph.vertices $ DeclUseGraph.toDigraph db.declUseGraph

-- | Every active declaration's identifier is its key
checkKeysDeclId :: Id p ~ C.DeclId => DeclDB l p -> [Violation]
checkKeysDeclId = checkKeys (\decl -> decl.id)

-- | Every active declaration's C identifier is its key
checkKeysDeclIdPair :: Id p ~ DeclIdPair => DeclDB l p -> [Violation]
checkKeysDeclIdPair = checkKeys (\decl -> decl.id.cName)

-- | The identifiers of the unit's declarations are the active keys
checkDeclsDeclId :: Id p ~ C.DeclId => C.TranslationUnit l p -> [Violation]
checkDeclsDeclId = checkDecls (\decl -> decl.id)

-- | The C identifiers of the unit's declarations are the active keys
checkDeclsDeclIdPair ::
     Id p ~ DeclIdPair
  => C.TranslationUnit l p
  -> [Violation]
checkDeclsDeclIdPair = checkDecls (\decl -> decl.id.cName)

checkKeys :: (C.Decl l p -> C.DeclId) -> DeclDB l p -> [Violation]
checkKeys key db = [
      ActiveIdNotKey declId (key decl)
    | (declId, Active decl) <- Map.toList db.entries
    , key decl /= declId
    ]

checkDecls :: (C.Decl l p -> C.DeclId) -> C.TranslationUnit l p -> [Violation]
checkDecls key unit = concat [
      map DeclNotActive $ Set.toList $ declIds \\ activeKeys
    , map ActiveNotDecl $ Set.toList $ activeKeys \\ declIds
    , map DuplicateDecl $ Map.keys $ Map.filter (> 1) $
        Map.fromListWith (+) [(key decl, 1 :: Int) | decl <- unit.decls]
    ]
  where
    declIds, activeKeys :: Set C.DeclId
    declIds    = Set.fromList $ map key unit.decls
    activeKeys = Map.keysSet $ Map.filter isActive unit.db.entries

    isActive :: Entry l p -> Bool
    isActive = \case
      Active{}   -> True
      _otherwise -> False

{-------------------------------------------------------------------------------
  Frontend
-------------------------------------------------------------------------------}

-- | Check the databases of a frontend run, pass by pass and pair by pair
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
        atDeclId "constructTranslationUnit" constructTranslationUnit
      , atDeclId "typecheckMacros" typecheckMacros
      , atDeclId "applyMacroFallback" applyMacroFallback
      , atDeclId "reparseMacroExpansions" reparseMacroExpansions
      , atDeclId "resolveBindingSpecs" resolveBindingSpecs
      , atDeclIdPair "mangleNames" mangleNames
      , atDeclIdPair "adjustTypes" adjustTypes
        -- TODO <https://github.com/well-typed/hs-bindgen/issues/1580>: until
        -- @Select@ deactivates the declarations it does not select, the
        -- declarations of these units are only the selected active ones.
      , report "select" $ checkKeysDeclIdPair select.db
      , report "final"  $ checkKeysDeclIdPair final.db
      , pair "constructTranslationUnit" "typecheckMacros" none
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

    atDeclId :: Id p ~ C.DeclId => String -> C.TranslationUnit l p -> [String]
    atDeclId name unit =
      report name $ checkKeysDeclId unit.db ++ checkDeclsDeclId unit

    atDeclIdPair ::
         Id p ~ DeclIdPair
      => String
      -> C.TranslationUnit l p
      -> [String]
    atDeclIdPair name unit =
      report name $ checkKeysDeclIdPair unit.db ++ checkDeclsDeclIdPair unit

    report :: String -> [Violation] -> [String]
    report name violations = [
          name ++ ": " ++ show violation
        | violation <- violations
        ]

    pair ::
         String
      -> String
      -> (Target -> Bool)
      -> C.TranslationUnit l p
      -> C.TranslationUnit l p'
      -> [String]
    pair nameOld nameNew allowed old new =
      report (nameOld ++ " -> " ++ nameNew) $
        checkTransition allowed old.db new.db
