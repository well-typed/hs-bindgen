module HsBindgen.Frontend.Pass.AssignAnonIds (
    assignAnonIds
  ) where

import Control.Monad.State
import Data.Either (partitionEithers)
import Data.Map qualified as Map

import HsBindgen.Frontend.Analysis.AnonUsage (AnonUsageAnalysis (..))
import HsBindgen.Frontend.Analysis.AnonUsage qualified as AnonUsageAnalysis
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.AssignAnonIds.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

-- | Assign name to all anonymous declarations
assignAnonIds ::
      C.TranslationUnit Parse
  -> (C.TranslationUnit AssignAnonIds, [Msg AssignAnonIds])
assignAnonIds unit =
    let (msgs, decls') =
           partitionEithers $
             map (updateDecl chosenNames) unit.unitDecls
    in  ( C.TranslationUnit{
              unitDecls        = decls'
            , unitIncludeGraph = unit.unitIncludeGraph
            , unitAnn          = NoAnn
            }
        , msgs
        )
  where
    chosenNames :: Map C.AnonId Text
    chosenNames =
        chooseNames
          (AnonUsageAnalysis.fromDecls unit.unitDecls)
          (mapMaybe anonDeclId unit.unitDecls)

    anonDeclId :: C.Decl Parse -> Maybe C.AnonId
    anonDeclId decl =
        case decl.declInfo.declId of
          C.PrelimDeclIdNamed{}           -> Nothing
          C.PrelimDeclIdAnon anonId _kind -> Just anonId

{-------------------------------------------------------------------------------
  Choose names
-------------------------------------------------------------------------------}

data AssignedName =
    AssignedName Text
  | FailedToAssignName

getAssignedName :: AssignedName -> Maybe Text
getAssignedName = \case
    AssignedName name  -> Just name
    FailedToAssignName -> Nothing

-- | Memoized assigned names
--
-- To avoid considering the same 'C.AnonId' over and over again, we maintain an
-- map for values already considered.
type Memoize = State (Map C.AnonId AssignedName)

checkMemoized :: C.AnonId -> Memoize (Maybe AssignedName)
checkMemoized = gets . Map.lookup

memoize :: C.AnonId -> AssignedName -> Memoize ()
memoize anonId = modify . Map.insert anonId

-- | Choose names
chooseNames :: AnonUsageAnalysis -> [C.AnonId] -> Map C.AnonId Text
chooseNames (AnonUsageAnalysis usageAnalysis) =
      Map.mapMaybe getAssignedName
    . flip execState Map.empty
    . mapM nameFor
  where
    -- Name for the given 'C.AnonId'
    --
    -- Returns 'Nothing' if we fail to assign a name.
    nameFor :: C.AnonId -> Memoize (Maybe Text)
    nameFor anonId = do
        mMemoized <- checkMemoized anonId
        case mMemoized of
          Just memoized -> return $ getAssignedName memoized
          Nothing       ->
            case Map.lookup anonId usageAnalysis of
              Nothing -> do
                -- Unused (or unusable) anonymous declaration
                memoize anonId FailedToAssignName
                return Nothing
              Just usage ->
                nameForUsage usage

    nameForUsage :: AnonUsageAnalysis.Context -> Memoize (Maybe Text)
    nameForUsage = \case
        AnonUsageAnalysis.Field declInfo fieldInfo ->
          fmap (nameForField fieldInfo) <$> declName declInfo
        AnonUsageAnalysis.TypedefVal declInfo ->
          fmap nameForTypedefVal <$> declName declInfo
        AnonUsageAnalysis.TypedefRef declInfo ->
          fmap nameForTypedefRef <$> declName declInfo

    declName :: C.DeclInfo Parse -> Memoize (Maybe Text)
    declName declInfo =
        case declInfo.declId of
          C.PrelimDeclIdNamed (C.DeclName name _kind) ->
            return $ Just name
          C.PrelimDeclIdAnon anonId _kind ->
            nameFor anonId

    nameForField :: C.FieldInfo Parse -> Text -> Text
    nameForField field typeName = typeName <> "_" <> field.fieldName.text

    -- Assign the name of the typedef to the struct
    -- NOTE: Only necessary in clang < 16; later clang do this automatically.
    nameForTypedefVal :: Text -> Text
    nameForTypedefVal = id

    -- Typedef around a pointer to an anonymous struct
    --
    -- Fortunately, clang does not assign a name to the struct in this situation
    -- (or rather, it assigns a name such as "(unnamed struct at ..)", so we can
    -- detect this case.
    nameForTypedefRef :: Text -> Text
    nameForTypedefRef typeName = typeName <> "_" <> "Deref"

{-------------------------------------------------------------------------------
  Use chosen names
-------------------------------------------------------------------------------}

updateDecl ::
     Map C.AnonId Text
  -> C.Decl Parse
  -> Either AssignAnonIdsMsg (C.Decl AssignAnonIds)
updateDecl chosenNames decl =
    reconstruct
      <$> updateDeclInfo chosenNames decl.declInfo
      <*> first mkMsg (updateUseSites chosenNames decl.declKind)
  where
    reconstruct ::
         C.DeclInfo AssignAnonIds
      -> C.DeclKind AssignAnonIds
      -> C.Decl AssignAnonIds
    reconstruct info' kind' = C.Decl{
        declInfo = info'
      , declKind = kind'
      , declAnn  = NoAnn
      }

    mkMsg :: SkippedUse -> AssignAnonIdsMsg
    mkMsg (SkippedUse anonId kind) =
      AssignAnonIdsSkippedUse decl.declInfo anonId kind

updateDeclInfo ::
     Map C.AnonId Text
  -> C.DeclInfo Parse
  -> Either AssignAnonIdsMsg (C.DeclInfo AssignAnonIds)
updateDeclInfo chosenNames info =
    case origDeclId of
      C.PrelimDeclIdNamed name ->
        Right $ reconstruct name
      C.PrelimDeclIdAnon anonId kind ->
        case Map.lookup anonId chosenNames of
          Nothing   -> Left  $ AssignAnonIdsSkippedDecl info
          Just name -> Right $ reconstruct $ C.DeclName name kind
  where
    origDeclId = info.declId

    reconstruct :: C.DeclName -> C.DeclInfo AssignAnonIds
    reconstruct name' = C.DeclInfo{
          declId = C.DeclId{
              name       = name'
            , origDeclId = C.OrigDeclId origDeclId
            , haskellId  = ()
            }

         -- The rest stays the same
        , declLoc          = info.declLoc
        , declHeaderInfo   = info.declHeaderInfo
        , declAvailability = info.declAvailability
        , declComment      = coercePass <$> info.declComment
        }

{-------------------------------------------------------------------------------
  Update use sites
-------------------------------------------------------------------------------}

data SkippedUse = SkippedUse C.AnonId C.NameKind

class UpdateUseSites a where
  -- | Update use sites
  --
  -- Unusable anonymous types, such as an anonymous type in a function
  -- signature, cannot be updated.
  updateUseSites ::
       Map C.AnonId Text
    -> a Parse
    -> Either SkippedUse (a AssignAnonIds)

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
        , structFieldAnn  = NoAnn
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
        , unionFieldAnn  = NoAnn
        , ..
        }

instance UpdateUseSites C.Typedef where
  updateUseSites chosenNames C.Typedef{..} =
      reconstruct <$> updateUseSites chosenNames typedefType
    where
      reconstruct :: C.Type AssignAnonIds -> C.Typedef AssignAnonIds
      reconstruct typedefType' = C.Typedef {
          typedefType = typedefType'
        , typedefAnn  = NoAnn
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
        , functionAnn  = NoAnn
        , ..
        }

instance UpdateUseSites C.Type where
  updateUseSites chosenNames = go
    where
      go :: C.Type Parse -> Either SkippedUse (C.Type AssignAnonIds)
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
        -> Either SkippedUse (C.DeclId AssignAnonIds)
      updateDeclId cPrelimDeclId = case cPrelimDeclId of
        C.PrelimDeclIdNamed name -> Right $
          C.DeclId name (C.OrigDeclId cPrelimDeclId) ()
        C.PrelimDeclIdAnon anonId kind ->
          case Map.lookup anonId chosenNames of
            Nothing   -> Left  $ SkippedUse anonId kind
            Just name -> Right $
              C.DeclId (C.DeclName name kind) (C.OrigDeclId cPrelimDeclId) ()
