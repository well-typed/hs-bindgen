-- | Conflicting C declarations
--
-- This module should only be used within the @HsBindgen.IR@ hierarchy.  From
-- outside the @HsBindgen.IR@ hierarchy, "HsBindgen.IR.C" should be used.
--
-- Within @HsBindgen.IR@, all modules aside from "HsBindgen.IR.C" should import
-- this module qualified for consistency.
--
-- > import HsBindgen.IR.C.Conflict qualified as C
module HsBindgen.IR.C.Conflict (
    Conflict
    -- * Construction
  , conflictBetween
  , conflictInsert
  , conflictFromList
    -- * Query
  , conflictToList
  , conflictGetMinimumLoc
  ) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel.Types

import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Multiple declarations for the same identifier
--
-- Two declarations are in conflict, if we cannot disambiguate between them in
-- C.
--
-- Conflicts can arise:
--
-- - macro vs macro (macros can be redefined)
--
-- - macro vs ordinary
--
-- - Clang-generated collision between a named tagged declaration and an
--   anonymous tagged declaration
--
--   For example,
--
--   @
--   struct foo { ... };
--   typedef struct { ... } foo;
--   @
--
-- Invariant: the 'Set' must have cardinality of 2 or larger.
data Conflict = Conflict {
      locs :: Set SingleLoc
    }
  deriving stock (Eq, Ord, Show)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

conflictBetween :: SingleLoc -> SingleLoc -> Conflict
conflictBetween l1 l2 = Conflict $ Set.fromList [l1, l2]

conflictInsert :: Conflict -> SingleLoc -> Conflict
conflictInsert (Conflict xs) x = Conflict $ Set.insert x xs

-- | Precondition: The length of the list must be 2 or longer.
--
-- TODO <https://github.com/well-typed/hs-bindgen/issues/1577>
conflictFromList :: [SingleLoc] -> Conflict
conflictFromList ls = Conflict $ Set.fromList ls

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

conflictToList :: Conflict -> NonEmpty SingleLoc
 -- 'NonEmpty.fromList' safe due to invariant.
conflictToList conflict = NonEmpty.fromList $ Set.toList conflict.locs

-- | Attempts to get the “minimum” location.
--
-- This is only meaningful if the locations share the same source path.
-- Comparisons across source paths happen in lexicographical order.
conflictGetMinimumLoc :: Conflict -> SingleLoc
 -- 'minimum' safe due to invariant.
conflictGetMinimumLoc conflict = minimum conflict.locs

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

instance PrettyForTrace Conflict where
  prettyForTrace conflict =
      PP.hangs' ("Conflicting declarations") 2 [
          PP.vcat [PP.string $ "- " ++ show l | l <- Set.toList conflict.locs]
        , "No binding generated."
        ]
