-- | Types used by the high-level API
--
-- Intended for unqualified import; see "Clang.HighLevel" for more detailed
-- discussion.
module Clang.HighLevel.Types (
    -- * Source locations
    SingleLoc(..)
  , MultiLoc(..)
  , Range(..)
    -- ** Comparisons
  , compareSingleLoc
  , rangeContainsLoc
    -- ** Conversion
  , toMulti
  , toRange
  , fromSingle
  , fromRange
    -- * Tokens
  , Token(..)
  , TokenSpelling(..)
    -- * Diagnostics
  , Diagnostic(..)
  , FixIt(..)
  , diagnosticIsError
    -- * Folds
  , Fold
  , Next
    -- ** Construction
  , simpleFold
  , runFold
    -- ** Fold-specific functionality
  , foldBreak
  , foldBreakWith
  , foldBreakOpt
  , foldContinue
  , foldContinueWith
  , foldContinueOpt
  , foldRecurse
  , foldRecurseWith
  , foldRecurseOpt
  , foldRecursePure
  , foldRecursePureOpt
    -- * User-provided names
  , UserProvided
  , getUserProvided
    -- * Declaration classification
  , Declaration(..)
  ) where

import Clang.HighLevel.Declaration
import Clang.HighLevel.Diagnostics
import Clang.HighLevel.Fold
import Clang.HighLevel.SourceLoc
import Clang.HighLevel.Tokens
import Clang.HighLevel.UserProvided
