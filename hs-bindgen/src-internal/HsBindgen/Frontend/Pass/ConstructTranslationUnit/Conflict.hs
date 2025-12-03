module HsBindgen.Frontend.Pass.ConstructTranslationUnit.Conflict (
    ConflictingDeclarations -- opaque
  , conflictingDeclarations
  , addConflictingLoc
  , getDeclId
  , getLocs
  , getMinimumLoc
  ) where

import Data.Set qualified as Set
import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel.Types

import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Imports
import HsBindgen.Util.Tracer

-- | Multiple declarations for the same identifier
data ConflictingDeclarations = ConflictingDeclarations {
        conflictId   :: C.PrelimDeclId
      , conflictLocs :: Set SingleLoc
      }
  deriving stock (Eq, Show)

instance PrettyForTrace ConflictingDeclarations where
  prettyForTrace = \case
    ConflictingDeclarations{..} ->
      let lead = PP.hcat [
              "Conflicting declarations for "
            , prettyForTrace conflictId
            , " declared at:"
            ]
          details = [
              PP.vcat [ PP.string $ "- " ++ show l | l <- Set.toList conflictLocs ]
            , "No binding generated."
            ]
      in  PP.hangs' lead 2 details

instance IsTrace Level ConflictingDeclarations where
  getDefaultLogLevel = \case
      ConflictingDeclarations{} -> Warning
  getSource  = const HsBindgen
  getTraceId = const "decl-index"

-- | Create conflicting declarations.
conflictingDeclarations :: C.PrelimDeclId -> SingleLoc -> SingleLoc -> ConflictingDeclarations
conflictingDeclarations d l1 l2 = ConflictingDeclarations d $ Set.fromList [l1, l2]

addConflictingLoc :: ConflictingDeclarations -> SingleLoc -> ConflictingDeclarations
addConflictingLoc (ConflictingDeclarations d xs) x = ConflictingDeclarations d $ Set.insert x xs

getDeclId :: ConflictingDeclarations -> C.PrelimDeclId
getDeclId = conflictId

getLocs :: ConflictingDeclarations -> [SingleLoc]
getLocs = Set.toList . conflictLocs

getMinimumLoc :: ConflictingDeclarations -> SingleLoc
-- Use of 'minimum' is safe here, becuase we ensure that the location set is
-- non-empty.
getMinimumLoc = minimum . conflictLocs
