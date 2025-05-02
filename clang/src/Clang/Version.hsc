module Clang.Version (
    -- * Definition
    ClangVersion(..)
    -- * Current version
  , clangVersion
    -- * Version requirements
  , Requires
  , requireClangVersion
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Class
import Foreign.C
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import System.IO.Unsafe (unsafePerformIO)

import Clang.Enum.Simple
import Clang.Internal.Results (callFailed)

#include "clang_wrappers.h"

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Clang version
--
-- The (derived) 'Ord' instance is semantically meaningful (useful for version
-- comparisons).
--
-- See 'getClangVersion' for discussion.
data ClangVersion =
    ClangOlderThan3_2   -- ^ @clang  <  3.2@
  | Clang3              -- ^ @clang >=  3.2 && <  4.0@
  | Clang4              -- ^ @clang >=  4.0 && <  5.0@
  | Clang5              -- ^ @clang >=  5.0 && <  6.0@
  | Clang6              -- ^ @clang >=  6.0 && <  7.0@
  | Clang7              -- ^ @clang >=  7.0 && <  8.0@
  | Clang8              -- ^ @clang >=  8.0 && <  9.0@
  | Clang9_or_10        -- ^ @clang >=  9.0 && < 11.0@
  | Clang11             -- ^ @clang >= 11.0 && < 11.1@
  | Clang11_or_12       -- ^ @clang >= 11.1 && < 13.0@
  | Clang13_or_14_or_15 -- ^ @clang >= 13.0 && < 16.0@
  | Clang16             -- ^ @clang >= 16.0 && < 17.0@
  | Clang17_or_18_or_19 -- ^ @clang >= 17.0 && < 20.0@
  | ClangNewerThan19    -- ^ @clang >= 20.0@
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

{-------------------------------------------------------------------------------
  Current version
-------------------------------------------------------------------------------}

-- | Get `clang` version
--
-- This is not a standard @libclang@ function; @libclang@ offers only
-- 'clang_getClangVersion', but this is described as
--
-- > Return a version string, suitable for showing to a user, but not intended
-- > to be parsed (the format is not guaranteed to be stable).
--
-- For this reason we provide 'getClangVersion', which /is/ intended to be
-- stable. We base this on @CINDEX_VERSION_MINOR@ (and @CINDEX_VERSION_MAJOR@,
-- which is always expected to be @0@). Unfortunately the mapping from
-- @CINDEX_VERSION_MINOR@ to @clang@ version is not one-to-one:
--
-- * Some @clang@ versions share the same @CINDEX_VERSION_MINOR@ version;
--   e.g. @clang-9@ and @clang-10@ both have a @CINDEX_VERSION_MINOR@ of @59@.
-- * Some @CINDEX_VERSION_MINOR@ numbers don't correspond to any (official)
--   @clang@ release; for example, the last 8.x release is 8.0.1, with a
--   @CINDEX_VERSION_MINOR@ of @50@, and the first 9.x release, 9.0.0, has
--   @CINDEX_VERSION_MINOR@ set to @59@.
--
-- On the assumption that 'getClangVersion' is called to check if the @clang@
-- version is /at least/ some minimum bound, we take the conservative approach
-- in both these cases, returning 'Clang9_or_10' also for @clang-10@, and
-- returning 'Clang8' for a @CINDEX_VERSION_MINOR@ of @51..58@.
--
-- Throws 'CallFailed' if @CINDEX_VERSION_MINOR@ or @CINDEX_VERSION_MAJOR@ are
-- not defined, or if @CINDEX_VERSION_MAJOR@ is not equal to zero.
foreign import capi unsafe "clang_wrappers.h getClangVersion"
  getClangVersion :: IO (SimpleEnum ClangVersion)

clangVersion :: ClangVersion
clangVersion = unsafePerformIO $ do
    mVersion <- fromSimpleEnum <$> getClangVersion
    case mVersion of
      Left  _cint   -> callFailed mVersion
      Right version -> return version

{-------------------------------------------------------------------------------
  'IsSimpleEnum' instance
-------------------------------------------------------------------------------}

instance IsSimpleEnum ClangVersion where
  simpleToC ClangOlderThan3_2   = #const ClangOlderThan3_2
  simpleToC Clang3              = #const Clang3
  simpleToC Clang4              = #const Clang4
  simpleToC Clang5              = #const Clang5
  simpleToC Clang6              = #const Clang6
  simpleToC Clang7              = #const Clang7
  simpleToC Clang8              = #const Clang8
  simpleToC Clang9_or_10        = #const Clang9_or_10
  simpleToC Clang11             = #const Clang11
  simpleToC Clang11_or_12       = #const Clang11_or_12
  simpleToC Clang13_or_14_or_15 = #const Clang13_or_14_or_15
  simpleToC Clang16             = #const Clang16
  simpleToC Clang17_or_18_or_19 = #const Clang17_or_18_or_19
  simpleToC ClangNewerThan19    = #const ClangNewerThan19

  simpleFromC (#const ClangOlderThan3_2)   = Just ClangOlderThan3_2
  simpleFromC (#const Clang3)              = Just Clang3
  simpleFromC (#const Clang4)              = Just Clang4
  simpleFromC (#const Clang5)              = Just Clang5
  simpleFromC (#const Clang6)              = Just Clang6
  simpleFromC (#const Clang7)              = Just Clang7
  simpleFromC (#const Clang8)              = Just Clang8
  simpleFromC (#const Clang9_or_10)        = Just Clang9_or_10
  simpleFromC (#const Clang11)             = Just Clang11
  simpleFromC (#const Clang11_or_12)       = Just Clang11_or_12
  simpleFromC (#const Clang13_or_14_or_15) = Just Clang13_or_14_or_15
  simpleFromC (#const Clang16)             = Just Clang16
  simpleFromC (#const Clang17_or_18_or_19) = Just Clang17_or_18_or_19
  simpleFromC (#const ClangNewerThan19)    = Just ClangNewerThan19

  simpleFromC _otherwise = Nothing

{-------------------------------------------------------------------------------
  Version requirements
-------------------------------------------------------------------------------}

-- | Version requirement
--
-- @Requires a@ means that version @a@ or later is required.  For example,
-- @Requires 'Clang17_or_18_or_19'@ means that Clang 17 or later is required.
newtype Requires a = Requires a
  deriving stock (Show)

-- | Throw a
-- @'Clang.Internal.Results.CallFailed' ('Requires' 'ClangVersion')@ exception
-- if the current Clang version is not greater than or equal to the specified
-- Clang version
requireClangVersion :: (MonadIO m, HasCallStack) => ClangVersion -> m ()
requireClangVersion v = unless (clangVersion >= v) $ callFailed (Requires v)
