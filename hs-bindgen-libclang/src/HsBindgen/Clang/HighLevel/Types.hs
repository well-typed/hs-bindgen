-- | Types used by the high-level API
--
-- Intended for unqualified import; see "HsBindgen.Clang.HighLevel" for
-- more detailed discussion.
module HsBindgen.Clang.HighLevel.Types (
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
  , Next(..)
    -- * User-provided names
  , UserProvided
  , getUserProvided
    -- * Declaration classification
  , Declaration(..)
  ) where

import HsBindgen.Clang.HighLevel.Declaration
import HsBindgen.Clang.HighLevel.Diagnostics
import HsBindgen.Clang.HighLevel.Fold
import HsBindgen.Clang.HighLevel.SourceLoc
import HsBindgen.Clang.HighLevel.Tokens
import HsBindgen.Clang.HighLevel.UserProvided
