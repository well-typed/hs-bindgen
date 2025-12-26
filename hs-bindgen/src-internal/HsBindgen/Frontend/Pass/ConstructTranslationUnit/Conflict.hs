{-# LANGUAGE NoFieldSelectors  #-}
{-# LANGUAGE NoRecordWildCards #-}
{-# LANGUAGE OverloadedLabels  #-}

-- | Conflicting declarations
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Pass.ConstructTranslationUnit.Conflict (Conflict)
-- > import HsBindgen.Frontend.Pass.ConstructTranslationUnit.Conflict qualified as Conflict
module HsBindgen.Frontend.Pass.ConstructTranslationUnit.Conflict (
    Conflict(..)
    -- * Construction
  , between
  , insert
    -- * Query
  , toList
  , getMinimumLoc
  ) where

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
-- Invariant: the 'Set' must be non-empty.
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

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

toList :: Conflict -> [SingleLoc]
toList conflict = Set.toList conflict.locs

getMinimumLoc :: Conflict -> SingleLoc
getMinimumLoc conflict = minimum conflict.locs -- safe due to invariant

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

instance PrettyForTrace Conflict where
  prettyForTrace conflict =
      PP.hangs' ("Conflicting declarations") 2 [
          PP.vcat [PP.string $ "- " ++ show l | l <- Set.toList conflict.locs]
        , "No binding generated."
        ]
