-- | Mapping of source location ranges in C code to @a@ values
--
-- Intended for unqualified import (unless context is unambiguous).
--
-- > import HsBindgen.Frontend.Pass.Parse.Monad.SourceRangeMap
module HsBindgen.Frontend.Pass.Parse.Monad.SourceRangeMap (
    SourceRangeMap
  , initSourceRangeMap
  , recordAt
  , LookupResult (..)
  , lookupRange
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Semigroup (Semigroup (sconcat))

import Clang.HighLevel.Types (Range (rangeEnd, rangeStart),
                              SingleLoc (singleLocColumn, singleLocLine, singleLocPath))
import Clang.Paths (SourcePath)

-- | Position storing @(line, column)@ so that lookups are precise to the
-- column: two values recorded on the same line (e.g. a struct tag and a field
-- type, both macro expansions) are attributed to the correct extent.
--
-- See https://github.com/well-typed/hs-bindgen/issues/2049.
data Pos = Pos {
    line :: Int
  , column :: Int
  }
  deriving stock (Eq, Show)

-- | We provide a custom 'Ord' instance because it is important to compare lines
--   before columns!
instance Ord Pos where
  left `compare` right =
       left.line   `compare` right.line
    <> left.column `compare` right.column

-- | Mapping of source location ranges in C code to @a@ values
newtype SourceRangeMap a = SRM {
    -- | We use a stacked map so we can lookup values in source location ranges
    -- reasonably fast.
    unwrap :: Map SourcePath (Map Pos (NonEmpty a))
  }

-- | An empty 'SourceRangeMap'
initSourceRangeMap :: SourceRangeMap a
initSourceRangeMap = SRM Map.empty

-- | Map the given location to a value of type @a@
recordAt :: forall a. SingleLoc -> a -> SourceRangeMap a -> SourceRangeMap a
recordAt loc new srm = SRM (addMacro srm.unwrap)
  where
    pos :: Pos
    pos = Pos loc.singleLocLine loc.singleLocColumn

    addMacro ::
         Map SourcePath (Map Pos (NonEmpty a))
      -> Map SourcePath (Map Pos (NonEmpty a))
    addMacro = Map.alter addMacroAtFile loc.singleLocPath

    addMacroAtFile ::
         Maybe (Map Pos (NonEmpty a))
      -> Maybe (Map Pos (NonEmpty a))
    addMacroAtFile = Just . \case
      Nothing ->
        Map.singleton pos $ NonEmpty.singleton new
      Just posMap ->
        Map.alter addMacroAtPos pos posMap

    addMacroAtPos :: Maybe (NonEmpty a) -> Maybe (NonEmpty a)
    addMacroAtPos = Just . \case
      Nothing     -> NonEmpty.singleton new
      Just macros -> NonEmpty.cons      new macros

data LookupResult a =
    -- | Ranges that span multiple files are not supported in lookups
    LookupErrorMultipleFiles
  | LookupNotFound
  | LookupFound a

-- | Lookup values in the given source location range
lookupRange :: forall a. Range SingleLoc -> SourceRangeMap a -> LookupResult (NonEmpty a)
lookupRange range srm
  | range.rangeStart.singleLocPath /= range.rangeEnd.singleLocPath
  = LookupErrorMultipleFiles
  | otherwise
  = maybe LookupNotFound LookupFound aux
  where
    sourcePath :: SourcePath
    sourcePath = range.rangeStart.singleLocPath

    topLeft, bottomRight :: Pos
    topLeft = Pos{
        line   = range.rangeStart.singleLocLine
      , column = range.rangeStart.singleLocColumn
      }
    bottomRight = Pos{
        line   = range.rangeEnd.singleLocLine
      , column = range.rangeEnd.singleLocColumn
      }

    aux :: Maybe (NonEmpty a)
    aux = do
        let fileMap = srm.unwrap
        posMap <- Map.lookup sourcePath fileMap
        fmap sconcat $ NE.nonEmpty $ Map.elems $
          Map.takeWhileAntitone (bottomRight >=) $
            Map.dropWhileAntitone (topLeft >) posMap
