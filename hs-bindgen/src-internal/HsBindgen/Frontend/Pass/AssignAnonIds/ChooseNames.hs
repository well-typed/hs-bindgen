module HsBindgen.Frontend.Pass.AssignAnonIds.ChooseNames (
    ChosenNames
  , chooseNames
  ) where

import Control.Monad.State
import Data.Map qualified as Map

import HsBindgen.Frontend.Analysis.AnonUsage (AnonUsageAnalysis (..))
import HsBindgen.Frontend.Analysis.AnonUsage qualified as AnonUsageAnalysis
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.Parse.IsPass (Parse)
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (AnonId, PrelimDeclId)
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId qualified as PrelimDeclId
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

type ChosenNames = Map AnonId DeclId

-- | Choose names for anonymous declarations
chooseNames :: AnonUsageAnalysis -> ChosenNames
chooseNames (AnonUsageAnalysis usageAnalysis) =
    Map.mapMaybe assignedName $
      flip execState Map.empty $
        -- Find name for any anon decl for which we found a use site
        mapM nameFor (Map.keys usageAnalysis)
  where
    -- Name for the given 'C.AnonId'
    --
    -- Returns 'Nothing' if we fail to assign a name.
    nameFor :: AnonId -> Memoize (Maybe DeclId)
    nameFor = memoize $ \anonId ->
        case Map.lookup anonId usageAnalysis of
          Nothing    -> return Nothing      -- Unused (or unusable) anon decl
          Just usage -> nameForUsage anonId usage

    nameForUsage ::
         AnonId
      -> AnonUsageAnalysis.Context
      -> Memoize (Maybe DeclId)
    nameForUsage anonId = \case
        AnonUsageAnalysis.Field declInfo fieldInfo ->
          fmap (nameForField anonId fieldInfo) <$> declName declInfo.id
        AnonUsageAnalysis.TypedefDirect declInfo ->
          fmap (nameForTypedefDirect anonId) <$> declName declInfo.id
        AnonUsageAnalysis.TypedefIndirect declInfo ->
          fmap (nameForTypedefIndirect anonId) <$> declName declInfo.id

    declName :: PrelimDeclId -> Memoize (Maybe DeclId)
    declName = \case
        PrelimDeclId.Named name@C.DeclName{} ->
          return $ Just DeclId{name = name, isAnon = False}
        PrelimDeclId.Anon anonId ->
          nameFor anonId

    nameForField :: AnonId -> C.FieldInfo Parse -> DeclId -> DeclId
    nameForField anonId field outerStruct = DeclId{
          isAnon = True
        , name   = C.DeclName{
              text = outerStruct.name.text <> "_" <> field.name.text
            , kind = anonId.kind
            }
        }

    -- Assign the name of the typedef to the struct
    --
    -- In @clang >= 16@ this is done automatically; this means that we cannot
    -- distinguish between these two declarations:
    --
    -- > typedef struct     { .. } foo;
    -- > typedef struct foo { .. } foo;
    --
    -- Consequently we are unable to detect that @foo@ is anonymous in this
    -- case. To emulate this behaviour older clang, we set @isAnon@ to @False@.
    nameForTypedefDirect :: AnonId -> DeclId -> DeclId
    nameForTypedefDirect anonId typedef = DeclId{
          isAnon = False -- 'False' instead of 'True'!
        , name   = C.DeclName{
              text = typedef.name.text
            , kind = anonId.kind
            }
        }

    -- Typedef around a pointer to an anonymous struct
    --
    -- Fortunately, clang does not assign a name to the struct in this situation
    -- (or rather, it assigns a name such as "(unnamed struct at ..)", so we can
    -- detect this case.
    nameForTypedefIndirect :: AnonId -> DeclId -> DeclId
    nameForTypedefIndirect anonId typedef = DeclId{
          isAnon = True
        , name   = C.DeclName{
              text = typedef.name.text <> "_Aux"
            , kind = anonId.kind
            }
        }

{-------------------------------------------------------------------------------
  Internal: memoization

  To avoid considering the same 'C.AnonId' over and over again, we maintain an
  map for values already considered.
-------------------------------------------------------------------------------}

data AssignedName =
    AssignedName DeclId
  | FailedToAssignName
  deriving stock (Show)

assignedName :: AssignedName -> Maybe DeclId
assignedName = \case
    AssignedName name  -> Just name
    FailedToAssignName -> Nothing

type Memoize = State (Map AnonId AssignedName)

memoize ::
     (AnonId -> Memoize (Maybe DeclId))
  -> (AnonId -> Memoize (Maybe DeclId))
memoize f anonId = state $ \acc ->
    case Map.lookup anonId acc of
      Just memoized -> (assignedName memoized, acc)
      Nothing       ->
        case runState (f anonId) acc of
          (mName, acc') -> (
              mName
            , case mName of
                Nothing   -> Map.insert anonId FailedToAssignName  acc'
                Just name -> Map.insert anonId (AssignedName name) acc'
            )

