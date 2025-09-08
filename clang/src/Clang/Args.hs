module Clang.Args (
    -- * Clang arguments
    ClangArgs(..)
  , InvalidClangArgs
    -- * C standard
  , CStandard(..)
  , Gnu(..)
  , getStdClangArg
  ) where

import Control.Exception (Exception)
import Data.Default (Default(def))
import Data.String (IsString)
import Data.Text (Text)

import Clang.Version

{-------------------------------------------------------------------------------
  Clang arguments
-------------------------------------------------------------------------------}

-- | Command-line arguments passed to @libclang@
--
-- The order of command-line arguments is significant.
--
-- Reference:
--
-- * <https://clang.llvm.org/docs/ClangCommandLineReference.html>
newtype ClangArgs = ClangArgs { unClangArgs :: [String] }
  deriving stock (Show)
  deriving newtype (Eq)

instance Default ClangArgs where
  def = ClangArgs []

-- | Invalid Clang arugments exception
newtype InvalidClangArgs = InvalidClangArgs String
  deriving stock (Show)
  deriving newtype (IsString)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  C standard
-------------------------------------------------------------------------------}

-- | C standard
--
-- Reference:
--
-- * "C Support in Clang"
--   <https://clang.llvm.org/c_status.html>
-- * "Differences between various standard modes" in the clang user manual
--   <https://clang.llvm.org/docs/UsersManual.html#differences-between-various-standard-modes>
--
-- We don't currently support @C2y@ because it requires @clang-19@ or later, and
-- we have no reliable way to test for that (see 'ClangVersion').
data CStandard =
    C89
  | C99
  | C11
  | C17
  | C23
  deriving stock (Bounded, Enum, Eq, Ord, Show)

-- | Enable GNU extensions?
data Gnu =
    DisableGnu
  | EnableGnu
  deriving stock (Bounded, Enum, Eq, Ord, Show)

-- | Get the Clang argument for the specified C standard
--
-- Support for C standards differ across different versions of Clang.
getStdClangArg :: CStandard -> Gnu -> Either InvalidClangArgs String
getStdClangArg cStandard gnu = case cStandard of
    C89 -> standardOrGnu "-std=c89" "-std=gnu89"
    C99 -> standardOrGnu "-std=c99" "-std=gnu99"
    C11 -> case clangVersion of
      ClangVersion version
        | version < (3, 2, 0)     -> standardOrGnu "-std=c1x" "-std=gnu1x"
        | otherwise               -> standardOrGnu "-std=c11" "-std=gnu11"
      ClangVersionUnknown version -> unknownClangVersion version
    C17 -> case clangVersion of
      ClangVersion version
        | version < (6, 0, 0)     -> invalid "C17 requires clang-6 or later"
        | otherwise               -> standardOrGnu "-std=c17" "-std=gnu17"
      ClangVersionUnknown version -> unknownClangVersion version
    C23 -> case clangVersion of
      ClangVersion version
        | version < (9, 0, 0)     -> invalid "C23 requires clang-9 or later"
        | version < (18, 0, 0)    -> standardOrGnu "-std=c2x" "-std=gnu2x"
        | otherwise               -> standardOrGnu "-std=c23" "-std=gnu23"
      ClangVersionUnknown version -> unknownClangVersion version
  where
    standardOrGnu :: String -> String -> Either a String
    standardOrGnu argStandard argGnu = case gnu of
      DisableGnu -> Right argStandard
      EnableGnu  -> Right argGnu

    invalid :: String -> Either InvalidClangArgs a
    invalid = Left . InvalidClangArgs

    unknownClangVersion :: Text -> Either InvalidClangArgs a
    unknownClangVersion version = Left . InvalidClangArgs $
      "Unknown clang version: " ++ show version
