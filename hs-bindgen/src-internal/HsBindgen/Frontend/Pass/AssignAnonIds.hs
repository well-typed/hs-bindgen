module HsBindgen.Frontend.Pass.AssignAnonIds (
    assignAnonIds
  ) where

import Data.Either (partitionEithers)
import Data.Map qualified as Map
import Data.Tuple

import HsBindgen.Frontend.Analysis.AnonUsage qualified as AnonUsageAnalysis
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.AssignAnonIds.ChooseNames
import HsBindgen.Frontend.Pass.AssignAnonIds.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Assign name to all anonymous declarations
assignAnonIds ::
     [ParseResult Parse]
  -> ([ParseResult AssignAnonIds], [ImmediateAssignAnonIdsMsg])
assignAnonIds parseResults =
    swap . partitionEithers $
      map (updateParseResult chosenNames) parseResults
  where
    decls :: [C.Decl Parse]
    decls = mapMaybe getParseResultMaybeDecl parseResults

    chosenNames :: ChosenNames
    chosenNames =
        chooseNames
          (AnonUsageAnalysis.fromDecls decls)
          (mapMaybe anonDeclId decls)

    anonDeclId :: C.Decl Parse -> Maybe C.AnonId
    anonDeclId decl =
        case decl.declInfo.declId of
          C.PrelimDeclIdNamed{}           -> Nothing
          C.PrelimDeclIdAnon anonId _kind -> Just anonId

{-------------------------------------------------------------------------------
  Update 'ParseResults' with chosen names
-------------------------------------------------------------------------------}

updateParseResult ::
     ChosenNames
  -> ParseResult Parse
  -> Either ImmediateAssignAnonIdsMsg (ParseResult AssignAnonIds)
updateParseResult chosenNames result =
    case result.classification of
      ParseResultSuccess success -> do
        flip auxSuccess success <$>
          updateDefInto chosenNames success.decl.declInfo
      ParseResultNotAttempted notAttempted ->
        flip auxNotAttempted notAttempted <$>
          updateDefSite chosenNames result.declId
      ParseResultFailure failure ->
        flip auxFailure failure <$>
          updateDefSite chosenNames result.declId
  where
    auxSuccess ::
         C.DeclInfo AssignAnonIds
      -> ParseSuccess Parse -> ParseResult AssignAnonIds
    auxSuccess declInfo' success =
        case updateUseSites chosenNames success.decl.declKind of
          Left (UnusableAnonDecl anonId) -> ParseResult{
              declId         = declInfo'.declId
            , declLoc        = result.declLoc
            , classification = ParseResultFailure $ ParseFailure $
                                 ParseUnusableAnonDecl anonId
            }
          Right declKind' -> ParseResult{
              declId         = declInfo'.declId
            , declLoc        = result.declLoc
            , classification = ParseResultSuccess ParseSuccess{
                  decl = C.Decl{
                      declInfo = declInfo'
                    , declKind = declKind'
                    , declAnn  = NoAnn
                    }
                , delayedParseMsgs = success.delayedParseMsgs
                }
            }

    auxNotAttempted ::
         C.DeclId AssignAnonIds
      -> ParseNotAttempted -> ParseResult AssignAnonIds
    auxNotAttempted declId' notAttempted = ParseResult{
          declId         = declId'
        , declLoc        = result.declLoc
        , classification = ParseResultNotAttempted notAttempted
        }

    auxFailure ::
         C.DeclId AssignAnonIds
      -> ParseFailure -> ParseResult AssignAnonIds
    auxFailure declId' failure = ParseResult{
          declId         = declId'
        , declLoc        = result.declLoc
        , classification = ParseResultFailure failure
        }

{-------------------------------------------------------------------------------
  Update definition sites
-------------------------------------------------------------------------------}

updateDefSite ::
     ChosenNames
  -> Id Parse
  -> Either ImmediateAssignAnonIdsMsg (Id AssignAnonIds)
updateDefSite chosenNames origDeclId =
    case origDeclId of
      C.PrelimDeclIdNamed name ->
        Right $ C.DeclId{
            name       = name
          , origDeclId = C.OrigDeclId origDeclId
          , haskellId  = ()
          }
      C.PrelimDeclIdAnon anonId kind ->
        case Map.lookup anonId chosenNames of
          Nothing   -> Left $ AssignAnonIdsSkippedDecl anonId
          Just name ->
            Right $ C.DeclId{
                name       = C.DeclName name kind
              , origDeclId = C.OrigDeclId origDeclId
              , haskellId  = ()
              }

updateDefInto ::
     ChosenNames
  -> C.DeclInfo Parse
  -> Either ImmediateAssignAnonIdsMsg (C.DeclInfo AssignAnonIds)
updateDefInto chosenNames info =
    reconstruct <$> updateDefSite chosenNames info.declId
  where
    reconstruct :: C.DeclId AssignAnonIds -> C.DeclInfo AssignAnonIds
    reconstruct declId' = C.DeclInfo{
          declId           = declId'
        , declLoc          = info.declLoc
        , declHeaderInfo   = info.declHeaderInfo
        , declAvailability = info.declAvailability
        , declComment      = coercePass <$> info.declComment
        }

{-------------------------------------------------------------------------------
  Update use sites
-------------------------------------------------------------------------------}

-- | We encountered an unusable anonymous declaration
--
-- If a \"outer\" declaration (e.g. a function declaration) contains an unusable
-- anonymous declaration, updating the use sites of that outer declaration will
-- fail (because we do not assign names to unusable anonymous declarations).
data UnusableAnonDecl = UnusableAnonDecl C.AnonId

class UpdateUseSites a where
  updateUseSites ::
       ChosenNames
    -> a Parse
    -> Either UnusableAnonDecl (a AssignAnonIds)

instance UpdateUseSites C.DeclKind where
  updateUseSites chosenNames = \case
      C.DeclStruct struct ->
        C.DeclStruct   <$> updateUseSites chosenNames struct
      C.DeclUnion union ->
        C.DeclUnion    <$> updateUseSites chosenNames union
      C.DeclTypedef typedef ->
        C.DeclTypedef  <$> updateUseSites chosenNames typedef
      C.DeclEnum enum ->
        C.DeclEnum     <$> updateUseSites chosenNames enum
      C.DeclOpaque cNameKind ->
        return (C.DeclOpaque cNameKind)
      C.DeclMacro unparsedMacro ->
        return (C.DeclMacro unparsedMacro)
      C.DeclFunction fun ->
        C.DeclFunction <$> updateUseSites chosenNames fun
      C.DeclGlobal ty ->
        C.DeclGlobal   <$> updateUseSites chosenNames ty

instance UpdateUseSites C.Struct where
  updateUseSites chosenNames C.Struct{..} =
      reconstruct <$> mapM (updateUseSites chosenNames) structFields
    where
      reconstruct :: [C.StructField AssignAnonIds] -> C.Struct AssignAnonIds
      reconstruct structFields' = C.Struct {
          structFields = structFields'
        , ..
        }

instance UpdateUseSites C.StructField where
  updateUseSites chosenNames C.StructField{..} =
      reconstruct <$> updateUseSites chosenNames structFieldType
    where
      reconstruct :: C.Type AssignAnonIds -> C.StructField AssignAnonIds
      reconstruct structFieldType' = C.StructField {
          structFieldInfo = coercePass structFieldInfo
        , structFieldType = structFieldType'
        , ..
        }

instance UpdateUseSites C.Union where
  updateUseSites chosenNames C.Union{..} =
      reconstruct <$> mapM (updateUseSites chosenNames) unionFields
    where
      reconstruct :: [C.UnionField AssignAnonIds] -> C.Union AssignAnonIds
      reconstruct unionFields' = C.Union {
          unionFields = unionFields'
        , ..
        }

instance UpdateUseSites C.UnionField where
  updateUseSites chosenNames C.UnionField{..} =
      reconstruct <$> updateUseSites chosenNames unionFieldType
    where
      reconstruct :: C.Type AssignAnonIds -> C.UnionField AssignAnonIds
      reconstruct unionFieldType' = C.UnionField {
          unionFieldInfo = coercePass unionFieldInfo
        , unionFieldType = unionFieldType'
        , ..
        }

instance UpdateUseSites C.Typedef where
  updateUseSites chosenNames C.Typedef{..} =
      reconstruct <$> updateUseSites chosenNames typedefType
    where
      reconstruct :: C.Type AssignAnonIds -> C.Typedef AssignAnonIds
      reconstruct typedefType' = C.Typedef {
          typedefType = typedefType'
        , ..
        }

instance UpdateUseSites C.Enum where
  updateUseSites chosenNames C.Enum{..} =
      reconstruct <$> updateUseSites chosenNames enumType
    where
      reconstruct :: C.Type AssignAnonIds -> C.Enum AssignAnonIds
      reconstruct enumType' = C.Enum {
          enumType      = enumType'
        , enumConstants = map coercePass enumConstants
        , ..
        }

instance UpdateUseSites C.Function where
  updateUseSites chosenNames C.Function{..} =
      reconstruct
        <$> mapM
              (\(mName, ty) -> (mName,) <$> updateUseSites chosenNames ty)
              functionArgs
        <*> updateUseSites chosenNames functionRes
    where
      reconstruct ::
           [(ArgumentName AssignAnonIds, C.Type AssignAnonIds)]
        -> C.Type AssignAnonIds
        -> C.Function AssignAnonIds
      reconstruct functionArgs' functionRes' = C.Function {
          functionArgs = functionArgs'
        , functionRes  = functionRes'
        , ..
        }

instance UpdateUseSites C.Type where
  updateUseSites chosenNames = go
    where
      go :: C.Type Parse -> Either UnusableAnonDecl (C.Type AssignAnonIds)
      go = \case
        -- Actual modifications
        C.TypeRef uid      -> C.TypeRef <$> updateDeclId uid
        C.TypeTypedef n ty -> C.TypeTypedef <$> updateDeclId n <*> go ty

        -- Recursive cases
        C.TypePointer ty         -> C.TypePointer <$> go ty
        C.TypeFun args res       -> C.TypeFun <$> mapM go args <*> go res
        C.TypeConstArray n ty    -> C.TypeConstArray n <$> go ty
        C.TypeIncompleteArray ty -> C.TypeIncompleteArray <$> go ty
        C.TypeBlock ty           -> C.TypeBlock <$> go ty
        C.TypeConst ty           -> C.TypeConst <$> go ty

        -- SimpleCases
        C.TypePrim pt        -> return (C.TypePrim pt)
        C.TypeVoid           -> return C.TypeVoid
        C.TypeExtBinding ext -> absurd ext
        C.TypeComplex pt     -> return (C.TypeComplex pt)

      updateDeclId ::
           C.PrelimDeclId
        -> Either UnusableAnonDecl (C.DeclId AssignAnonIds)
      updateDeclId cPrelimDeclId = case cPrelimDeclId of
        C.PrelimDeclIdNamed name -> Right $
          C.DeclId name (C.OrigDeclId cPrelimDeclId) ()
        C.PrelimDeclIdAnon anonId kind ->
          case Map.lookup anonId chosenNames of
            Nothing   -> Left  $ UnusableAnonDecl anonId
            Just name -> Right $
              C.DeclId (C.DeclName name kind) (C.OrigDeclId cPrelimDeclId) ()
