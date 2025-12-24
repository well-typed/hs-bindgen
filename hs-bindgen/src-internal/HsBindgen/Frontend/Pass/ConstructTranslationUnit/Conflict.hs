module HsBindgen.Frontend.Pass.ConstructTranslationUnit.Conflict (
    ConflictingDeclarations -- opaque
  , conflictingDeclarations
  , addConflictingLoc
  , getLocs
  , getMinimumLoc
  ) where

import Data.Set qualified as Set
import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel.Types

import HsBindgen.Imports
import HsBindgen.Util.Tracer

-- | Multiple declarations for the same identifier
data ConflictingDeclarations = ConflictingDeclarations {
        conflictLocs :: Set SingleLoc
      }
  deriving stock (Eq, Show)

instance PrettyForTrace ConflictingDeclarations where
  prettyForTrace = \case
    ConflictingDeclarations{..} ->
      PP.hangs' ("Conflicting declarations") 2 [
          PP.vcat [ PP.string $ "- " ++ show l | l <- Set.toList conflictLocs ]
        , "No binding generated."
        ]

instance IsTrace Level ConflictingDeclarations where
  getDefaultLogLevel = \case
      ConflictingDeclarations{} -> Warning
  getSource  = const HsBindgen
  getTraceId = const "decl-index"

-- | Create conflicting declarations.
conflictingDeclarations :: SingleLoc -> SingleLoc -> ConflictingDeclarations
conflictingDeclarations l1 l2 = ConflictingDeclarations $ Set.fromList [l1, l2]

addConflictingLoc :: ConflictingDeclarations -> SingleLoc -> ConflictingDeclarations
addConflictingLoc (ConflictingDeclarations xs) x = ConflictingDeclarations $ Set.insert x xs

getLocs :: ConflictingDeclarations -> [SingleLoc]
getLocs = Set.toList . conflictLocs

getMinimumLoc :: ConflictingDeclarations -> SingleLoc
-- Use of 'minimum' is safe here, becuase we ensure that the location set is
-- non-empty.
getMinimumLoc = minimum . conflictLocs
