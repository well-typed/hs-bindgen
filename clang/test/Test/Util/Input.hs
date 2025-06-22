-- | Test inputs
--
-- Intended for qualified import:
--
-- > import Test.Util.Input (TestInput(..), ToTestInput(..), ShowComment(..))
-- > import Test.Util.Input qualified as Input
module Test.Util.Input (
    TestInput(..)
    -- * Construction
  , unlines
  , intercalate
  , indent
  ) where

import Prelude hiding (unlines)
import Prelude qualified

import Data.Coerce
import Data.List qualified as List
import Data.String (IsString)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Test input (header)
--
-- The 'Show' instance for 'TestInput' is just the string itself, without any
-- escaping; this results in much more readable test output.
newtype TestInput = TestInput String
  deriving newtype (IsString, Semigroup, Monoid)

instance Show TestInput where
  show (TestInput header) = "\n" ++ header

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

unlines :: [String] -> TestInput
unlines = coerce Prelude.unlines

intercalate :: String -> [TestInput] -> TestInput
intercalate = coerce (List.intercalate @Char)

indent :: TestInput -> TestInput
indent (TestInput input) = TestInput $
    -- Don't define this using 'Prelude.unlines' to avoid trailing newlines
    "  " ++ concatMap aux input
  where
    aux :: Char -> String
    aux '\n' = "\n  "
    aux c    = [c]
