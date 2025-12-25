module HsBindgen.Frontend.Pass.AssignAnonIds.ChooseNames (
    ChosenNames
  , chooseNames
  ) where

import Control.Monad.State
import Data.Map qualified as Map

import HsBindgen.Frontend.Analysis.AnonUsage (AnonUsageAnalysis (..))
import HsBindgen.Frontend.Analysis.AnonUsage qualified as AnonUsageAnalysis
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

type ChosenNames = Map C.AnonId C.DeclId

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
    nameFor :: C.AnonId -> Memoize (Maybe C.DeclId)
    nameFor = memoize $ \anonId ->
        case Map.lookup anonId usageAnalysis of
          Nothing    -> return Nothing      -- Unused (or unusable) anon decl
          Just usage -> nameForUsage anonId usage

    nameForUsage ::
         C.AnonId
      -> AnonUsageAnalysis.Context
      -> Memoize (Maybe C.DeclId)
    nameForUsage anonId = \case
        AnonUsageAnalysis.Field declInfo fieldInfo ->
          fmap (nameForField anonId fieldInfo) <$> declName declInfo.declId
        AnonUsageAnalysis.TypedefDirect declInfo ->
          fmap (nameForTypedefDirect anonId) <$> declName declInfo.declId
        AnonUsageAnalysis.TypedefIndirect declInfo ->
          fmap (nameForTypedefIndirect anonId) <$> declName declInfo.declId

    declName :: C.PrelimDeclId -> Memoize (Maybe C.DeclId)
    declName = \case
        C.PrelimDeclIdNamed name@C.DeclName{} ->
          return $ Just C.DeclId{name, isAnon = False}
        C.PrelimDeclIdAnon anonId ->
          nameFor anonId

    nameForField :: C.AnonId -> C.FieldInfo Parse -> C.DeclId -> C.DeclId
    nameForField anonId field outerStruct = C.DeclId{
          isAnon = True
        , name   = C.DeclName{
              text = outerStruct.name.text <> "_" <> field.fieldName.text
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
    nameForTypedefDirect :: C.AnonId -> C.DeclId -> C.DeclId
    nameForTypedefDirect anonId typedef = C.DeclId{
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
    --
    -- TODO: <https://github.com/well-typed/hs-bindgen/issues/1427>
    -- This should not really be called @_Deref@, but maybe something like
    -- @_Aux@: it's not just pointer dereferencing, but also other uses. (Here
    -- as well as in 'HandleTypedefs').
    nameForTypedefIndirect :: C.AnonId -> C.DeclId -> C.DeclId
    nameForTypedefIndirect anonId typedef = C.DeclId{
          isAnon = True
        , name   = C.DeclName{
              text = typedef.name.text <> "_" <> "Deref"
            , kind = anonId.kind
            }
        }

{-------------------------------------------------------------------------------
  Internal: memoization

  To avoid considering the same 'C.AnonId' over and over again, we maintain an
  map for values already considered.
-------------------------------------------------------------------------------}

data AssignedName =
    AssignedName C.DeclId
  | FailedToAssignName
  deriving stock (Show)

assignedName :: AssignedName -> Maybe C.DeclId
assignedName = \case
    AssignedName name  -> Just name
    FailedToAssignName -> Nothing

type Memoize = State (Map C.AnonId AssignedName)

memoize ::
     (C.AnonId -> Memoize (Maybe C.DeclId))
  -> (C.AnonId -> Memoize (Maybe C.DeclId))
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

