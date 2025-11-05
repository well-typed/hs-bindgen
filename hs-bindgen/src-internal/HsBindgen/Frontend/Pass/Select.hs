module HsBindgen.Frontend.Pass.Select (
    selectDecls
  ) where

import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set

import Clang.HighLevel.Types

import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex (..))
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Coerce (CoercePass (coercePass))
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports

-- Identifier of a declaration.
type I = C.QualPrelimDeclId

-- Declaration itself.
type D = C.Decl Select

-- Match function to find selection roots.
type Match = I -> SingleLoc -> C.Availability -> Bool

selectDecls ::
     IsMainHeader
  -> IsInMainHeaderDir
  -> Set C.QualName
  -> Config Select
  -> C.TranslationUnit ResolveBindingSpecs
  -> (C.TranslationUnit Select, [Msg Select])
selectDecls
  isMainHeader
  isInMainHeaderDir
  declsWithExternalBindingSpecs
  SelectConfig{..}
  unit =
    let -- Identifiers of selection roots.
        rootIds :: Set I
        rootIds = getSelectedRoots match index

        -- Identifiers of transitive dependencies (without roots), and all
        -- selected declarations.
        transIds, selIds :: Set I
        (transIds, selIds) = case selectConfigProgramSlicing of
          DisableProgramSlicing ->
            (Set.empty, rootIds)
          EnableProgramSlicing ->
            let rootAndTransIds =
                  UseDeclGraph.getTransitiveDeps unit.unitAnn.declUseDecl $
                    Set.toList rootIds
            in  (rootAndTransIds Set.\\ rootIds, rootAndTransIds)

        Acc {
            selDs = selDeclsReversed
            -- TODO: #1037.
          , rIds  = _unavailableRootIds
          , tIds  = _unavailableTransIds
          , msgs  = selectStatusMsgs
          } = foldDecls rootIds transIds decls

    in (    unitSelectWith $ reverse selDeclsReversed
       ,    selectStatusMsgs
         -- TODO: #1037.
         -- ++ toUnavailableMsgs        unavailableRootIds
         -- ++ toUnavailableMsgs        unavailableTransIds
         ++ getDelayedParseMsgs      selIds index
         ++ getParseNotAttemptedMsgs match hasExternalBindingSpec index
         ++ getParseFailureMsgs      match hasExternalBindingSpec index
       )
  where
    decls :: [D]
    decls = map coercePass unit.unitDecls

    hasExternalBindingSpec :: I -> Bool
    hasExternalBindingSpec = \case
          C.QualPrelimDeclIdNamed n k ->
            Set.member (C.QualName n k) declsWithExternalBindingSpecs
          _otherwise -> False

    -- TODO: #1037.
    _toUnavailableMsgs :: Set I -> [Msg Select]
    _toUnavailableMsgs = map SelectUnavailableDeclaration
      . Set.toList
      . Set.filter (not . hasExternalBindingSpec)

    index :: DeclIndex
    index = unit.unitAnn.declIndex

    unitSelectWith :: [D] -> C.TranslationUnit Select
    unitSelectWith xs = C.TranslationUnit {
            C.unitDecls = xs
          , C.unitIncludeGraph = unit.unitIncludeGraph
          , C.unitAnn = unit.unitAnn
          }

    match :: Match
    match = \declId -> go declId declId
      where
        -- We compare the use sites of anonymous declarations with the original
        -- @declId@, so we can detect cycles involving anonymous declarations in
        -- the use-decl graph. We believe these cycles can not exist.
        go origDeclId declId loc availability = case declId of
            C.QualPrelimDeclIdNamed name kind ->
              matchSelect
                isMainHeader
                isInMainHeaderDir
                (singleLocPath loc)
                (C.QualName name kind)
                availability
                selectConfigPredicate
            -- Apply the select predicate to the use site.
            anon@(C.QualPrelimDeclIdAnon{}) -> matchAnon origDeclId anon
            -- Never select builtins.
            C.QualPrelimDeclIdBuiltin _ -> False

        matchAnon :: I -> I -> Bool
        matchAnon origDeclId anon =
          case DeclUseGraph.getUseSites unit.unitAnn.declDeclUse anon of
            [(declId, _)] -> matchUseSite origDeclId declId
            -- Unused anonymous declarations are removed in the @NameAnon@
            -- pass. Here we are using the decl-use graph to find use sites,
            -- and so we still can encounter unused anonymous declarations.
            []            -> False
            xs            -> panicPure $
              "anonymous declaration with multiple use sites: "
              ++ show anon ++ " used by " ++ show xs

        matchUseSite :: I -> I -> Bool
        matchUseSite origDeclId declIdUseSite
          | declIdUseSite == origDeclId = panicPure $
              "unexpected cycle involving anonymous declaration: "
              ++ show origDeclId
          | otherwise =
          case DeclIndex.lookup declIdUseSite index of
            Nothing   -> panicPure $ "did not find declaration: " <> show declIdUseSite
            Just decl -> match
                           declIdUseSite
                           (C.declLoc $ C.declInfo decl)
                           (C.declAvailability $ C.declInfo decl)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

getSelectedRoots :: Match -> DeclIndex -> Set I
getSelectedRoots match index = Foldable.foldl' addMatch Set.empty index.succeeded
  where
   addMatch :: Set I -> ParseSuccess -> Set I
   addMatch xs (ParseSuccess decl _) =
     let info = decl.declInfo
         qualPrelimDeclId = C.declQualPrelimDeclId decl
         isSelected = match qualPrelimDeclId info.declLoc info.declAvailability
     in  if isSelected then
           Set.insert qualPrelimDeclId xs
         else xs

data Acc = Acc {
      -- Selected declarations
      selDs    :: [D]
      -- Identifiers of selection roots yet to be selected. Identifiers
      -- remaining after the fold are unavailable and lead to error traces.
    , rIds     :: Set I
      -- Identifiers of transitive dependencies yet to be selected. Identifiers
      -- remaining after the fold are unavailable and lead to error traces.
    , tIds     :: Set I
      -- @SelectSelectStatus@ trace messages.
    , msgs     :: [Msg Select]
    }

-- Traverse the declarations, partition them into selected and not-selected
-- declarations. Also return IDs that were _not_ found in the list of available
-- declarations. These declarations are _unvailable_, and lead to error traces.
foldDecls :: Set I -> Set I -> [D] -> Acc
foldDecls rootIds transIds decls =
    Foldable.foldl' acc (Acc [] rootIds transIds []) decls
  where
    acc :: Acc -> D -> Acc
    acc Acc{..} decl =
      let declId = C.declOrigQualPrelimDeclId decl
      in  case ( deleteAndCheck declId rIds
               , deleteAndCheck declId tIds ) of
            -- Declaration is a selection root.
            (Just rIds', Nothing) ->
              Acc (decl:selDs) rIds' tIds
                (getSelMsgs SelectionRoot decl ++ msgs)
            -- Declaration is a transitive dependency.
            (Nothing, Just tIds') ->
              Acc (decl:selDs) rIds tIds'
                (getSelMsgs TransitiveDependency decl ++ msgs)
            -- Declaration is not selected.
            (Nothing, Nothing) ->
              Acc selDs rIds tIds
                (getNotSelMsg decl : msgs)
            -- Impossible :-), bug.
            (Just _, Just _) ->
              panicPure $
                "Declaration is selection root and transitive dependency: "
                ++ show decl.declInfo

    -- Return @Just@ the new set if the element was deleted, otherwise return
    -- @Nothing@.
    deleteAndCheck :: Ord a => a -> Set a -> Maybe (Set a)
    deleteAndCheck x xs = Set.alterF (\b -> if b then Just False else Nothing) x xs

    getSelMsgs :: SelectReason -> D -> [Msg Select]
    getSelMsgs selectReason decl =
      let info         = decl.declInfo
          selectDepr   = [ SelectDeprecated info | isDeprecated info ]
      in  SelectSelectStatus (Selected selectReason) info : selectDepr

    isDeprecated :: C.DeclInfo Select -> Bool
    isDeprecated info = case C.declAvailability info of
      C.Deprecated -> True
      _            -> False

    getNotSelMsg :: D -> Msg Select
    getNotSelMsg decl = SelectSelectStatus NotSelected decl.declInfo

getDelayedParseMsgs :: Set I -> DeclIndex -> [Msg Select]
getDelayedParseMsgs selIds index = concatMap getMsgs $ Set.toList selIds
  where
    getMsgs :: I -> [Msg Select]
    getMsgs k = map SelectParseSuccess $ DeclIndex.lookupAttachedParseMsgs k index

getParseNotAttemptedMsgs :: Match -> (I -> Bool) -> DeclIndex -> [Msg Select]
getParseNotAttemptedMsgs match hasExternalBindingSpec =
  Foldable.foldl' (Foldable.foldl' addMsg) [] . omitted
  where
    addMsg :: [SelectMsg] -> ParseNotAttempted -> [SelectMsg]
    addMsg xs (ParseNotAttempted i l a r) =
      [ SelectParseNotAttempted i l r
      | match i l a
      , not $ hasExternalBindingSpec i
      ] ++ xs

getParseFailureMsgs :: Match -> (I -> Bool) -> DeclIndex -> [Msg Select]
getParseFailureMsgs match hasExternalBindingSpec =
  Foldable.foldl' (Foldable.foldl' addMsg) [] . failed
  where
    addMsg :: [SelectMsg] -> ParseFailure -> [SelectMsg]
    addMsg xs (ParseFailure i l a msgs) =
      [ SelectParseFailure msg
      | match i l a
      , not $ hasExternalBindingSpec i
      , msg <- NonEmpty.toList msgs
      ] ++ xs
