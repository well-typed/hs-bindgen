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
    reconstruct <$> updateDeclInfo chosenNames decl.declInfo
  where
    reconstruct :: C.DeclInfo AssignAnonIds -> C.Decl AssignAnonIds
    reconstruct info' = C.Decl{
          declInfo = info'
        , declKind = updateUseSites chosenNames decl.declKind
        , declAnn  = NoAnn
        }

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
          Nothing   -> Left  $ AssignAnonIdsSkipped info
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

class UpdateUseSites a where
  -- | Update use sites
  --
  -- TODO: We should think about whether or not this case fail. In principle
  -- the fact that we have a use site means that the anonymous type is used;
  -- however, we also don't assign a name when an anonymous type is /unusable/,
  -- rather than unused (for example, if they appear in a function sig).
  updateUseSites :: Map C.AnonId Text -> a Parse -> a AssignAnonIds

instance UpdateUseSites C.DeclKind where
  updateUseSites = undefined
