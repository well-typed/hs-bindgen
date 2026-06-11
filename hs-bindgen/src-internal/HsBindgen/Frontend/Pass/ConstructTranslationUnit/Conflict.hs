-- | Conflicting declarations
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Pass.ConstructTranslationUnit.Conflict (Conflict)
-- > import HsBindgen.Frontend.Pass.ConstructTranslationUnit.Conflict qualified as Conflict
module HsBindgen.Frontend.Pass.ConstructTranslationUnit.Conflict (
    Conflict
    -- * Construction
  , between
  , insert
  , fromList
    -- * Query
  , toList
  , getMinimumLoc
  ) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel.Types

import HsBindgen.Imports hiding (toList)
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
  deriving stock (Eq, Show)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

between :: SingleLoc -> SingleLoc -> Conflict
between l1 l2 = Conflict $ Set.fromList [l1, l2]

insert :: Conflict -> SingleLoc -> Conflict
insert (Conflict xs) x = Conflict $ Set.insert x xs

-- | Precondition: The length of the list must be 2 or longer.
--
-- TODO <https://github.com/well-typed/hs-bindgen/issues/1577>
fromList :: [SingleLoc] -> Conflict
fromList ls = Conflict $ Set.fromList ls

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

toList :: Conflict -> NonEmpty SingleLoc
 -- 'NonEmpty.fromList' safe due to invariant.
toList conflict = NonEmpty.fromList $ Set.toList conflict.locs

-- | Attempts to get the “minimum” location.
--
-- This is only meaningful if the locations share the same source path.
-- Comparisons across source paths happen in lexicographical order.
getMinimumLoc :: Conflict -> SingleLoc
 -- 'minimum' safe due to invariant.
getMinimumLoc conflict = minimum conflict.locs

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

instance PrettyForTrace Conflict where
  prettyForTrace conflict =
      PP.hangs' ("Conflicting declarations") 2 [
          PP.vcat [PP.string $ "- " ++ show l | l <- Set.toList conflict.locs]
        , "No binding generated."
        ]
