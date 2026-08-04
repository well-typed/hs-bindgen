module HsBindgen.Frontend.Pass.FillUnnamedIds.ChooseNames (
    ChosenNames
  , chooseNames
  ) where

import Control.Monad.State
import Data.Map qualified as Map

import HsBindgen.Frontend.Analysis.UnnamedIdUsage (UnnamedIdUsageAnalysis (..))
import HsBindgen.Frontend.Analysis.UnnamedIdUsage qualified as UnnamedIdUsageAnalysis
import HsBindgen.Frontend.Pass.Parse.IsPass (Parse)
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

type ChosenNames = Map C.UnnamedId C.DeclId

-- | Choose names for unnamed declarations
chooseNames :: UnnamedIdUsageAnalysis -> ChosenNames
chooseNames (UnnamedIdUsageAnalysis usageAnalysis) =
    Map.mapMaybe assignedName $
      flip execState Map.empty $
        -- Find name for any unnamed decl for which we found a use site
        mapM nameFor (Map.keys usageAnalysis)
  where
    -- Name for the given 'C.UnnamedId'
    --
    -- Returns 'Nothing' if we fail to assign a name.
    nameFor :: C.UnnamedId -> Memoize (Maybe C.DeclId)
    nameFor = memoize $ \unnamedId ->
        case Map.lookup unnamedId usageAnalysis of
          Nothing    -> return Nothing      -- Unused (or unusable) unnamed decl
          Just usage -> nameForUsage unnamedId usage

    nameForUsage ::
         C.UnnamedId
      -> UnnamedIdUsageAnalysis.Context
      -> Memoize (Maybe C.DeclId)
    nameForUsage unnamedId = \case
        UnnamedIdUsageAnalysis.Field declInfo fieldInfo ->
          fmap (nameForField unnamedId fieldInfo) <$> declName declInfo.id
        UnnamedIdUsageAnalysis.TypedefDirect declInfo ->
          fmap (nameForTypedefDirect unnamedId) <$> declName declInfo.id
        UnnamedIdUsageAnalysis.TypedefIndirect declInfo ->
          fmap (nameForTypedefIndirect unnamedId) <$> declName declInfo.id
        UnnamedIdUsageAnalysis.GlobalVar declInfo ->
          fmap (nameForGlobalVar unnamedId) <$> declName declInfo.id

    declName :: C.PrelimDeclId -> Memoize (Maybe C.DeclId)
    declName = \case
        C.PrelimDeclIdNamed name@C.DeclName{} ->
          return $ Just C.DeclId{name = name, isUnnamed = False}
        C.PrelimDeclIdUnnamed unnamedId ->
          nameFor unnamedId

    nameForField :: C.UnnamedId -> C.FieldInfo Parse -> C.DeclId -> C.DeclId
    nameForField unnamedId field outerStruct = C.DeclId{
          isUnnamed = True
        , name   = C.DeclName{
              text = outerStruct.name.text <> "_" <> field.name.text
            , kind = unnamedId.kind
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
    -- Consequently we are unable to detect that @foo@ is unnamed in this
    -- case. To emulate this behaviour older clang, we set @isUnnamed@ to @False@.
    nameForTypedefDirect :: C.UnnamedId -> C.DeclId -> C.DeclId
    nameForTypedefDirect unnamedId typedef = C.DeclId{
          isUnnamed = False -- 'False' instead of 'True'!
        , name   = C.DeclName{
              text = typedef.name.text
            , kind = unnamedId.kind
            }
        }

    -- Typedef around a pointer to an untagged struct
    --
    -- Fortunately, clang does not assign a name to the struct in this situation
    -- (or rather, it assigns a name such as "(untagged struct at ..)", so we can
    -- detect this case.
    nameForTypedefIndirect :: C.UnnamedId -> C.DeclId -> C.DeclId
    nameForTypedefIndirect unnamedId typedef = C.DeclId{
          isUnnamed = True
        , name   = C.DeclName{
              text = typedef.name.text <> "_Aux"
            , kind = unnamedId.kind
            }
        }

    -- | Use the name of the global variable for the untagged struct
    --
    -- For example, given:
    --
    -- > struct { int x; int y; } a;
    --
    -- the struct is named "a".
    --
    -- Unlike 'nameForTypedefDirect' (where @typedef struct { .. } foo;@ creates
    -- a real C type name @foo@), @struct { .. } bar;@ does /not/ create
    -- any C type name — the struct remains untagged from C's perspective.
    -- We set @isUnnamed@ to @True@ so that the backend can detect this and avoid
    -- generating invalid C types like @struct bar *@.
    nameForGlobalVar :: C.UnnamedId -> C.DeclId -> C.DeclId
    nameForGlobalVar unnamedId globalVar = C.DeclId{
          isUnnamed = True
        , name   = C.DeclName{
              text = globalVar.name.text
            , kind = unnamedId.kind
            }
        }

{-------------------------------------------------------------------------------
  Internal: memoization

  To avoid considering the same 'C.UnnamedId' over and over again, we maintain an
  map for values already considered.
-------------------------------------------------------------------------------}

data AssignedId =
    AssignedId C.DeclId
  | FailedToAssignId
  deriving stock (Show)

assignedName :: AssignedId -> Maybe C.DeclId
assignedName = \case
    AssignedId name  -> Just name
    FailedToAssignId -> Nothing

type Memoize = State (Map C.UnnamedId AssignedId)

memoize ::
     (C.UnnamedId -> Memoize (Maybe C.DeclId))
  -> (C.UnnamedId -> Memoize (Maybe C.DeclId))
memoize f unnamedId = state $ \acc ->
    case Map.lookup unnamedId acc of
      Just memoized -> (assignedName memoized, acc)
      Nothing       ->
        case runState (f unnamedId) acc of
          (mName, acc') -> (
              mName
            , case mName of
                Nothing   -> Map.insert unnamedId FailedToAssignId  acc'
                Just name -> Map.insert unnamedId (AssignedId name) acc'
            )

