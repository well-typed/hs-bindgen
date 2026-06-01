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
                              SingleLoc (singleLocLine, singleLocPath))
import Clang.Paths (SourcePath)

-- | Mapping of source location ranges in C code to @a@ values
newtype SourceRangeMap a = SRM {
    -- | We use a stacked map so we can lookup values in source location ranges
    -- reasonably fast.
    unwrap :: Map SourcePath (Map Int (NonEmpty a))
  }

-- | An empty 'SourceRangeMap'
initSourceRangeMap :: SourceRangeMap a
initSourceRangeMap = SRM Map.empty

-- | Map the given location to a value of type @a@
recordAt :: forall a. SingleLoc -> a -> SourceRangeMap a -> SourceRangeMap a
recordAt loc new srm = SRM (addMacro srm.unwrap)
  where
    addMacro ::
         Map SourcePath (Map Int (NonEmpty a))
      -> Map SourcePath (Map Int (NonEmpty a))
    addMacro = Map.alter addMacroAtFile loc.singleLocPath

    addMacroAtFile ::
         Maybe (Map Int (NonEmpty a))
      -> Maybe (Map Int (NonEmpty a))
    addMacroAtFile = Just . \case
      Nothing ->
        Map.singleton loc.singleLocLine $ NonEmpty.singleton new
      Just lineMap ->
        Map.alter addMacroAtLine loc.singleLocLine lineMap

    addMacroAtLine :: Maybe (NonEmpty a) -> Maybe (NonEmpty a)
    addMacroAtLine = Just . \case
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

    aux :: Maybe (NonEmpty a)
    aux = do
        let fileMap = srm.unwrap
        lineMap <- Map.lookup sourcePath fileMap
        fmap sconcat $ NE.nonEmpty $ Map.elems $
          Map.takeWhileAntitone (range.rangeEnd.singleLocLine >=) $
            Map.dropWhileAntitone (range.rangeStart.singleLocLine >) lineMap
