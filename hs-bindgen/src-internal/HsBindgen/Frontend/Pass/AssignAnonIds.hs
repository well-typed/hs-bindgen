module HsBindgen.Frontend.Pass.AssignAnonIds (
    assignAnonIds
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Either (partitionEithers)
import Data.Map qualified as Map
import Data.Tuple

import HsBindgen.Frontend.Analysis.AnonUsage
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.AssignAnonIds.ChooseNames
import HsBindgen.Frontend.Pass.AssignAnonIds.IsPass
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (AnonId, PrelimDeclId)
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId qualified as PrelimDeclId
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Pass.SimplifyAST.IsPass (SimplifyAST)
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Assign name to all anonymous declarations
assignAnonIds ::
     AnonUsageAnalysis
  -> [ParseResult SimplifyAST]
  -> ([ParseResult AssignAnonIds], [ImmediateAssignAnonIdsMsg])
assignAnonIds usage parseResults =
    swap . partitionEithers $
      map (updateParseResult chosenNames) parseResults
  where
    chosenNames :: ChosenNames
    chosenNames = chooseNames usage

{-------------------------------------------------------------------------------
  Update 'ParseResults' with chosen names
-------------------------------------------------------------------------------}

updateParseResult ::
     ChosenNames
  -> ParseResult SimplifyAST
  -> Either ImmediateAssignAnonIdsMsg (ParseResult AssignAnonIds)
updateParseResult chosenNames result =
    case result.classification of
      ParseResultSuccess success -> do
        auxSuccess success <$>
          updateDefSite chosenNames success.decl.info.id
      ParseResultNotAttempted notAttempted ->
        auxNotAttempted notAttempted <$>
          updateDefSite chosenNames result.id
      ParseResultFailure failure ->
        auxFailure failure <$>
          updateDefSite chosenNames result.id
  where
    auxSuccess :: ParseSuccess SimplifyAST -> DeclId -> ParseResult AssignAnonIds
    auxSuccess success declId' =
        case runM chosenNames updated of
          Left (UnusableAnonDecl anonId) -> ParseResult{
              id             = declId'
            , loc            = result.loc
            , classification = ParseResultFailure $ ParseFailure $
                                 ParseUnusableAnonDecl anonId
            }
          Right (declInfo', declKind') -> ParseResult{
              id             = declInfo'.id
            , loc            = result.loc
            , classification = ParseResultSuccess ParseSuccess{
                  decl = C.Decl{
                      info = declInfo'
                    , kind = declKind'
                    , ann  = NoAnn
                    }
                , delayedParseMsgs = success.delayedParseMsgs
                }
            }

      where
        updated :: M (C.DeclInfo AssignAnonIds, C.DeclKind AssignAnonIds)
        updated = (,)
            <$> updateDeclInfo declId' success.decl.info
            <*> updateUseSites         success.decl.kind

    auxNotAttempted ::
         ParseNotAttempted
      -> DeclId
      -> ParseResult AssignAnonIds
    auxNotAttempted notAttempted declId' = ParseResult{
          id             = declId'
        , loc            = result.loc
        , classification = ParseResultNotAttempted notAttempted
        }

    auxFailure ::
         ParseFailure
      -> DeclId
      -> ParseResult AssignAnonIds
    auxFailure failure declId' = ParseResult{
          id             = declId'
        , loc            = result.loc
        , classification = ParseResultFailure failure
        }

{-------------------------------------------------------------------------------
  Update definition sites
-------------------------------------------------------------------------------}

updateDefSite ::
     ChosenNames
  -> Id SimplifyAST
  -> Either ImmediateAssignAnonIdsMsg (Id AssignAnonIds)
updateDefSite chosenNames =
    first AssignAnonIdsSkippedDecl . fromPrelimDeclId chosenNames

updateDeclInfo ::
     DeclId
  -> C.DeclInfo SimplifyAST
  -> M (C.DeclInfo AssignAnonIds)
updateDeclInfo declId' info =
    reconstruct <$> mapM updateUseSites info.comment
  where
    reconstruct :: Maybe (C.Comment AssignAnonIds) -> C.DeclInfo AssignAnonIds
    reconstruct declComment' = C.DeclInfo{
          id           = declId'
        , comment      = declComment'
        , loc          = info.loc
        , headerInfo   = info.headerInfo
        , availability = info.availability
        }

{-------------------------------------------------------------------------------
  Internal auxiliary: monad for updating use sites
-------------------------------------------------------------------------------}

newtype M a = WrapM (
      ReaderT ChosenNames (Except UnusableAnonDecl) a
    )
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
runM chosenNames (WrapM ma) = runExcept $ runReaderT ma chosenNames

{-------------------------------------------------------------------------------
  Update use sites
-------------------------------------------------------------------------------}

class UpdateUseSites a where
  updateUseSites :: a SimplifyAST -> M (a AssignAnonIds)

instance UpdateUseSites C.DeclKind where
  updateUseSites = \case
      C.DeclStruct   x         -> C.DeclStruct   <$> updateUseSites x
      C.DeclUnion    x         -> C.DeclUnion    <$> updateUseSites x
      C.DeclTypedef  x         -> C.DeclTypedef  <$> updateUseSites x
      C.DeclEnum     x         -> C.DeclEnum     <$> updateUseSites x
      C.DeclAnonEnumConstant x -> C.DeclAnonEnumConstant <$> updateUseSites x
      C.DeclFunction x         -> C.DeclFunction <$> updateUseSites x
      C.DeclGlobal   x         -> C.DeclGlobal   <$> updateUseSites x
      C.DeclMacro    x         -> return $ C.DeclMacro x
      C.DeclOpaque             -> return $ C.DeclOpaque

instance UpdateUseSites C.Struct where
  updateUseSites struct =
      reconstruct
        <$> mapM updateUseSites struct.fields
        <*> mapM updateUseSites struct.flam
    where
      reconstruct ::
           [C.StructField AssignAnonIds]
        -> (Maybe (C.StructField AssignAnonIds))
        -> C.Struct AssignAnonIds
      reconstruct structFields' structFlam' = C.Struct {
            fields    = structFields'
          , flam      = structFlam'
          , sizeof    = struct.sizeof
          , alignment = struct.alignment
          , ann       = struct.ann
          }

instance UpdateUseSites C.StructField where
  updateUseSites field =
      reconstruct
        <$> updateUseSites field.typ
        <*> updateUseSites field.info
    where
      reconstruct ::
           C.Type AssignAnonIds
        -> C.FieldInfo AssignAnonIds
        -> C.StructField AssignAnonIds
      reconstruct structFieldType' structFieldInfo' = C.StructField {
            typ    = structFieldType'
          , info   = structFieldInfo'
          , offset = field.offset
          , width  = field.width
          , ann    = field.ann
          }

instance UpdateUseSites C.Union where
  updateUseSites union =
      reconstruct <$> mapM updateUseSites union.fields
    where
      reconstruct :: [C.UnionField AssignAnonIds] -> C.Union AssignAnonIds
      reconstruct unionFields' = C.Union {
            fields    = unionFields'
          , sizeof    = union.sizeof
          , alignment = union.alignment
          , ann       = union.ann
          }

instance UpdateUseSites C.UnionField where
  updateUseSites field =
      reconstruct
        <$> updateUseSites field.typ
        <*> updateUseSites field.info
    where
      reconstruct ::
           C.Type AssignAnonIds
        -> C.FieldInfo AssignAnonIds
        -> C.UnionField AssignAnonIds
      reconstruct unionFieldType' unionFieldInfo' = C.UnionField {
          typ  = unionFieldType'
        , info = unionFieldInfo'
        , ann  = field.ann
        }

instance UpdateUseSites C.Typedef where
  updateUseSites typedef =
      reconstruct <$> updateUseSites typedef.typ
    where
      reconstruct :: C.Type AssignAnonIds -> C.Typedef AssignAnonIds
      reconstruct typedefType' = C.Typedef {
          typ = typedefType'
        , ann = typedef.ann
        }

instance UpdateUseSites C.Enum where
  updateUseSites enum =
      reconstruct
        <$> updateUseSites enum.typ
        <*> mapM updateUseSites enum.constants
    where
      reconstruct ::
           C.Type AssignAnonIds
        -> [C.EnumConstant AssignAnonIds]
        -> C.Enum AssignAnonIds
      reconstruct enumType' enumConstants' = C.Enum {
            typ       = enumType'
          , constants = enumConstants'
          , sizeof    = enum.sizeof
          , alignment = enum.alignment
          , ann       = enum.ann
          }

instance UpdateUseSites C.Function where
  updateUseSites function =
      reconstruct
        <$> mapM updateUseSites function.args
        <*> updateUseSites function.res
    where
      reconstruct ::
           [C.FunctionArg AssignAnonIds]
        -> C.Type AssignAnonIds
        -> C.Function AssignAnonIds
      reconstruct functionArgs' functionRes' = C.Function {
            args  = functionArgs'
          , res   = functionRes'
          , attrs = function.attrs
          , ann   = function.ann
          }

instance UpdateUseSites C.FunctionArg where
  updateUseSites functionArg =
      reconstruct
        <$> pure functionArg.name
        <*> updateUseSites functionArg.typ
    where
      reconstruct ::
           Maybe C.ScopedName
        -> C.Type AssignAnonIds
        -> C.FunctionArg AssignAnonIds
      reconstruct name' typ' = C.FunctionArg {
            name = name'
          , typ = typ'
          }

instance UpdateUseSites C.Type where
  updateUseSites = go
    where
      go :: C.Type SimplifyAST -> M (C.Type AssignAnonIds)
      go = \case
          -- Actual modifications
          C.TypeRef     ref     -> C.TypeRef     <$> updateDeclId ref
          C.TypeEnum    ref     ->
            fmap C.TypeEnum $ C.Ref
                <$> updateDeclId ref.name
                <*> updateUseSites ref.underlying
          C.TypeTypedef ref ->
            fmap C.TypeTypedef $ C.Ref
                <$> updateDeclId ref.name
                <*> updateUseSites ref.underlying

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
  updateUseSites info =
      reconstruct <$> mapM updateUseSites info.comment
    where
      reconstruct ::
           Maybe (C.Comment AssignAnonIds)
        -> C.FieldInfo AssignAnonIds
      reconstruct fieldComment' = C.FieldInfo{
            comment = fieldComment'
          , name    = info.name
          , loc     = info.loc
          }

instance UpdateUseSites C.EnumConstant where
  updateUseSites constant =
      reconstruct <$> updateUseSites constant.info
    where
      reconstruct :: C.FieldInfo AssignAnonIds -> C.EnumConstant AssignAnonIds
      reconstruct enumConstantInfo' = C.EnumConstant{
            info  = enumConstantInfo'
          , value = constant.value
          }

instance UpdateUseSites C.AnonEnumConstant where
  updateUseSites anonEnumConstant =
      reconstruct <$> updateUseSites anonEnumConstant.constant
    where
      reconstruct :: C.EnumConstant AssignAnonIds -> C.AnonEnumConstant AssignAnonIds
      reconstruct constant' = C.AnonEnumConstant{
            typ      = anonEnumConstant.typ  -- PrimType has no use sites to update
          , constant = constant'
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
      Right DeclId{name = name, isAnon = False}
    PrelimDeclId.Anon anonId ->
      maybe (Left anonId) Right $ Map.lookup anonId chosenNames
