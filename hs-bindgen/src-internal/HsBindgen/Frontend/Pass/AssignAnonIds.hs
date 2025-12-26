module HsBindgen.Frontend.Pass.AssignAnonIds (
    assignAnonIds
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Either (partitionEithers)
import Data.Map qualified as Map
import Data.Tuple

import HsBindgen.Frontend.Analysis.AnonUsage qualified as AnonUsageAnalysis
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.AssignAnonIds.ChooseNames
import HsBindgen.Frontend.Pass.AssignAnonIds.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (AnonId, PrelimDeclId)
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId qualified as PrelimDeclId
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
    chosenNames = chooseNames (AnonUsageAnalysis.fromDecls decls)

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
        auxSuccess success <$>
          updateDefSite chosenNames success.decl.info.declId
      ParseResultNotAttempted notAttempted ->
        auxNotAttempted notAttempted <$>
          updateDefSite chosenNames result.declId
      ParseResultFailure failure ->
        auxFailure failure <$>
          updateDefSite chosenNames result.declId
  where
    auxSuccess :: ParseSuccess Parse -> DeclId -> ParseResult AssignAnonIds
    auxSuccess ParseSuccess{decl, delayedParseMsgs} declId' =
        case runM chosenNames updated of
          Left (UnusableAnonDecl anonId) -> ParseResult{
              declId         = declId'
            , declLoc        = result.declLoc
            , classification = ParseResultFailure $ ParseFailure $
                                 ParseUnusableAnonDecl anonId
            }
          Right (declInfo', declKind') -> ParseResult{
              declId         = declInfo'.declId
            , declLoc        = result.declLoc
            , classification = ParseResultSuccess ParseSuccess{
                  decl = C.Decl{
                      info = declInfo'
                    , kind = declKind'
                    , ann  = NoAnn
                    }
                , delayedParseMsgs = delayedParseMsgs
                }
            }

      where
        updated :: M (C.DeclInfo AssignAnonIds, C.DeclKind AssignAnonIds)
        updated = (,)
            <$> updateDeclInfo declId' decl.info
            <*> updateUseSites         decl.kind

    auxNotAttempted ::
         ParseNotAttempted
      -> DeclId
      -> ParseResult AssignAnonIds
    auxNotAttempted notAttempted declId' = ParseResult{
          declId         = declId'
        , declLoc        = result.declLoc
        , classification = ParseResultNotAttempted notAttempted
        }

    auxFailure ::
         ParseFailure
      -> DeclId
      -> ParseResult AssignAnonIds
    auxFailure failure declId' = ParseResult{
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
updateDefSite chosenNames =
    first AssignAnonIdsSkippedDecl . fromPrelimDeclId chosenNames

updateDeclInfo ::
     DeclId
  -> C.DeclInfo Parse
  -> M (C.DeclInfo AssignAnonIds)
updateDeclInfo declId' info =
    reconstruct <$> mapM updateUseSites info.declComment
  where
    reconstruct :: Maybe (C.Comment AssignAnonIds) -> C.DeclInfo AssignAnonIds
    reconstruct declComment' = C.DeclInfo{
          declId           = declId'
        , declComment      = declComment'
        , declLoc          = info.declLoc
        , declHeaderInfo   = info.declHeaderInfo
        , declAvailability = info.declAvailability
        }

{-------------------------------------------------------------------------------
  Internal auxiliary: monad for updating use sites
-------------------------------------------------------------------------------}

newtype M a = WrapM {
      unwrapM :: ReaderT ChosenNames (Except UnusableAnonDecl) a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    )

-- | We encountered an unusable anonymous declaration
--
-- Not all use sites of anon decls are given a name; for example, if we have a
-- function signature with an anonymous struct, we will not even traverse that
-- function signature in "HsBindgen.Frontend.Analysis.AnonUsage". This means
-- that if we then try to /update/ those use sites, that will fail.
--
-- This is an internal type; the external equivalent is 'ParseUnusableAnonDecl'.
data UnusableAnonDecl = UnusableAnonDecl AnonId
  deriving stock (Show)

runM :: ChosenNames -> M a -> Either UnusableAnonDecl a
runM chosenNames = runExcept . flip runReaderT chosenNames . unwrapM

{-------------------------------------------------------------------------------
  Update use sites
-------------------------------------------------------------------------------}


class UpdateUseSites a where
  updateUseSites :: a Parse -> M (a AssignAnonIds)

instance UpdateUseSites C.DeclKind where
  updateUseSites = \case
      C.DeclStruct   x -> C.DeclStruct   <$> updateUseSites x
      C.DeclUnion    x -> C.DeclUnion    <$> updateUseSites x
      C.DeclTypedef  x -> C.DeclTypedef  <$> updateUseSites x
      C.DeclEnum     x -> C.DeclEnum     <$> updateUseSites x
      C.DeclFunction x -> C.DeclFunction <$> updateUseSites x
      C.DeclGlobal   x -> C.DeclGlobal   <$> updateUseSites x
      C.DeclMacro    x -> return $ C.DeclMacro x
      C.DeclOpaque     -> return $ C.DeclOpaque

instance UpdateUseSites C.Struct where
  updateUseSites C.Struct{..} =
      reconstruct
        <$> mapM updateUseSites structFields
        <*> mapM updateUseSites structFlam
    where
      reconstruct ::
           [C.StructField AssignAnonIds]
        -> (Maybe (C.StructField AssignAnonIds))
        -> C.Struct AssignAnonIds
      reconstruct structFields' structFlam' = C.Struct {
          structFields = structFields'
        , structFlam   = structFlam'
        , ..
        }

instance UpdateUseSites C.StructField where
  updateUseSites C.StructField{..} =
      reconstruct
        <$> updateUseSites structFieldType
        <*> updateUseSites structFieldInfo
    where
      reconstruct ::
           C.Type AssignAnonIds
        -> C.FieldInfo AssignAnonIds
        -> C.StructField AssignAnonIds
      reconstruct structFieldType' structFieldInfo' = C.StructField {
          structFieldType = structFieldType'
        , structFieldInfo = structFieldInfo'
        , ..
        }

instance UpdateUseSites C.Union where
  updateUseSites C.Union{..} =
      reconstruct <$> mapM updateUseSites unionFields
    where
      reconstruct :: [C.UnionField AssignAnonIds] -> C.Union AssignAnonIds
      reconstruct unionFields' = C.Union {
          unionFields = unionFields'
        , ..
        }

instance UpdateUseSites C.UnionField where
  updateUseSites C.UnionField{..} =
      reconstruct
        <$> updateUseSites unionFieldType
        <*> updateUseSites unionFieldInfo
    where
      reconstruct ::
           C.Type AssignAnonIds
        -> C.FieldInfo AssignAnonIds
        -> C.UnionField AssignAnonIds
      reconstruct unionFieldType' unionFieldInfo' = C.UnionField {
          unionFieldType = unionFieldType'
        , unionFieldInfo = unionFieldInfo'
        , ..
        }

instance UpdateUseSites C.Typedef where
  updateUseSites C.Typedef{..} =
      reconstruct <$> updateUseSites typedefType
    where
      reconstruct :: C.Type AssignAnonIds -> C.Typedef AssignAnonIds
      reconstruct typedefType' = C.Typedef {
          typedefType = typedefType'
        , ..
        }

instance UpdateUseSites C.Enum where
  updateUseSites C.Enum{..} =
      reconstruct
        <$> updateUseSites enumType
        <*> mapM updateUseSites enumConstants
    where
      reconstruct ::
           C.Type AssignAnonIds
        -> [C.EnumConstant AssignAnonIds]
        -> C.Enum AssignAnonIds
      reconstruct enumType' enumConstants' = C.Enum {
            enumType      = enumType'
          , enumConstants = enumConstants'
          , ..
          }

instance UpdateUseSites C.Function where
  updateUseSites C.Function{..} =
      reconstruct
        <$> mapM
              (\(mName, ty) -> (mName,) <$> updateUseSites ty)
              functionArgs
        <*> updateUseSites functionRes
    where
      reconstruct ::
           [(Maybe (C.ScopedName), C.Type AssignAnonIds)]
        -> C.Type AssignAnonIds
        -> C.Function AssignAnonIds
      reconstruct functionArgs' functionRes' = C.Function {
            functionArgs = functionArgs'
          , functionRes  = functionRes'
          , ..
          }

instance UpdateUseSites C.Type where
  updateUseSites = go
    where
      go :: C.Type Parse -> M (C.Type AssignAnonIds)
      go = \case
          -- Actual modifications
          C.TypeRef     ref     -> C.TypeRef     <$> updateDeclId ref
          C.TypeTypedef typedef -> C.TypeTypedef <$> updateUseSites typedef

          -- Recursive cases
          C.TypePointers n      ty -> C.TypePointers n <$> go ty
          C.TypeConstArray n    ty -> C.TypeConstArray n <$> go ty
          C.TypeIncompleteArray ty -> C.TypeIncompleteArray <$> go ty
          C.TypeBlock           ty -> C.TypeBlock <$> go ty
          C.TypeQual qual       ty -> C.TypeQual qual <$> go ty
          C.TypeFun args res       -> C.TypeFun <$> mapM go args <*> go res

          -- SimpleCases
          C.TypeVoid           -> return $ C.TypeVoid
          C.TypePrim    pt     -> return $ C.TypePrim    pt
          C.TypeComplex pt     -> return $ C.TypeComplex pt
          C.TypeExtBinding ext -> absurd ext

instance UpdateUseSites C.TypedefRef where
  updateUseSites (C.TypedefRef n uTy) =
      C.TypedefRef
        <$> updateDeclId n
        <*> updateUseSites uTy

updateDeclId :: PrelimDeclId -> M DeclId
updateDeclId prelimDeclId = WrapM $ do
    chosenNames <- ask
    case fromPrelimDeclId chosenNames prelimDeclId of
      Left  anonId -> throwError $ UnusableAnonDecl anonId
      Right declId -> return declId

{-------------------------------------------------------------------------------
  Comments
-------------------------------------------------------------------------------}

instance UpdateUseSites C.FieldInfo where
  updateUseSites C.FieldInfo{..} =
      reconstruct <$> mapM updateUseSites fieldComment
    where
      reconstruct ::
           Maybe (C.Comment AssignAnonIds)
        -> C.FieldInfo AssignAnonIds
      reconstruct fieldComment' = C.FieldInfo{
            fieldComment = fieldComment'
          , ..
          }

instance UpdateUseSites C.EnumConstant where
  updateUseSites C.EnumConstant{..} =
      reconstruct <$> updateUseSites enumConstantInfo
    where
      reconstruct :: C.FieldInfo AssignAnonIds -> C.EnumConstant AssignAnonIds
      reconstruct enumConstantInfo' = C.EnumConstant{
            enumConstantInfo = enumConstantInfo'
           ,..
          }

instance UpdateUseSites C.Comment where
  updateUseSites (C.Comment comment) =
      C.Comment <$> mapM updateUseSites comment

instance UpdateUseSites C.CommentRef where
  updateUseSites (C.CommentRef name mId) =
      C.CommentRef name <$> mapM updateDeclId mId

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Construct 'DeclId' from 'C.PrelimDeclId'
--
-- Returns 'Left' an 'C.AnonId' if the 'C.PrelimDeclId' is anonymous and we have
-- assigned no name.
fromPrelimDeclId :: ChosenNames -> PrelimDeclId -> Either AnonId DeclId
fromPrelimDeclId chosenNames = \case
    PrelimDeclId.Named name ->
      Right DeclId{name, isAnon = False}
    PrelimDeclId.Anon anonId ->
      maybe (Left anonId) Right $ Map.lookup anonId chosenNames
