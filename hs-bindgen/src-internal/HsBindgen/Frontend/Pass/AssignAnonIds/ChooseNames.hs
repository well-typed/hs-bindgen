module HsBindgen.Frontend.Pass.AssignAnonIds.ChooseNames (
    ChosenNames
  , chooseNames
  ) where

import Control.Monad.State
import Data.Map qualified as Map

import HsBindgen.Frontend.Analysis.AnonUsage (AnonUsageAnalysis (..))
import HsBindgen.Frontend.Analysis.AnonUsage qualified as AnonUsageAnalysis
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

type ChosenNames = Map C.AnonId Text

-- | Choose names for anonymous declarations
chooseNames :: AnonUsageAnalysis -> [C.AnonId] -> ChosenNames
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
  Internal: memoization

  To avoid considering the same 'C.AnonId' over and over again, we maintain an
  map for values already considered.
-------------------------------------------------------------------------------}

type Memoize = State (Map C.AnonId AssignedName)

data AssignedName =
    AssignedName Text
  | FailedToAssignName

getAssignedName :: AssignedName -> Maybe Text
getAssignedName = \case
    AssignedName name  -> Just name
    FailedToAssignName -> Nothing

checkMemoized :: C.AnonId -> Memoize (Maybe AssignedName)
checkMemoized = gets . Map.lookup

memoize :: C.AnonId -> AssignedName -> Memoize ()
memoize anonId = modify . Map.insert anonId
