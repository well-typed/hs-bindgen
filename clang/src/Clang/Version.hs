module Clang.Version (
    -- * Definition
    ClangVersion(..)
    -- * Current version
  , clangVersion
    -- * Version requirements
  , Requires
  , requireClangVersion
    -- * Low-level API
  , parseClangVersion
  , clang_getClangVersion
  ) where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

import Clang.Internal.ByValue
import Clang.Internal.CXString ()
import Clang.Internal.Results
import Clang.LowLevel.Core.Structs

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Clang version
--
-- We're intentionally /not/ deriving 'Ord', to ensure that version comparisons
-- take 'ClangVersionUnknown' into account.
data ClangVersion =
    -- | Parsed version number (major, minor, patch)
    ClangVersion (Int, Int, Int)

    -- | Unknown version
    --
    -- We get the version by parsing the result of 'clang_getClangVersion',
    -- which explicitly says
    --
    -- > Return a version string, suitable for showing to a user, but not
    -- > intended to be parsed (the format is not guaranteed to be stable).
    --
    -- Unfortunately, @libclang@ does not provide any other means of getting
    -- the version number. We therefore parse the string anyway, and use
    -- 'UnknownClangVersion' when that fails.
    --
    -- NOTE: While @Index.h@ does provide @CINDEX_VERSION_MAJOR@ (which is
    -- always zero) and @CINDEX_VERSION_MINOR@, unfortunately they do not map
    -- cleanly to @clang@ versions, and do not provide sufficient resolution.
    -- For example, a @CINDEX_VERSION_MINOR@ value of 64 could be any version
    -- between @17.0.0@ and @20.1.7@ (and possibly more still).
  | ClangVersionUnknown Text
  deriving stock (Show, Eq) -- No 'Ord'

{-------------------------------------------------------------------------------
  Current version
-------------------------------------------------------------------------------}

clangVersion :: ClangVersion
clangVersion = unsafePerformIO $ parseClangVersion <$> clang_getClangVersion

-- | Parse clang version string
--
-- 'clang_getClangVersion' may return something like
--
-- > "Ubuntu clang version 14.0.0-1ubuntu1.1"
-- > "Ubuntu clang version 18.1.3 (1ubuntu1)"
-- > "Ubuntu clang version 19.1.1 (1ubuntu1~24.04.2)"
-- > "clang version 14.0.6"
-- > "clang version 14.0.6 (git@github.com:llvm/llvm-project.git f28c006a5895fc0e329fe15fead81e37457cb1d1)"
-- > "clang version 18.1.8 (https://github.com/llvm/llvm-project.git ad36915a8c42d51218eee4b53f2c0aae80eb17e9)"
-- > "clang version 20.1.4 (https://github.com/llvm/llvm-project ec28b8f9cc7f2ac187d8a617a6d08d5e56f9120e)"
--
-- We try to find the proper version (14.0.0, 18.1.3, ..) in this string.
parseClangVersion :: Text -> ClangVersion
parseClangVersion versionString =
      maybe (ClangVersionUnknown versionString) ClangVersion
    . (>>= readParts)
    . listToMaybe
    . mapMaybe exactlyThree
    . map (splitOn '.' . takeWhile isPartOfVersionProper)
    . words
    $ Text.unpack versionString
  where
    isPartOfVersionProper :: Char -> Bool
    isPartOfVersionProper c = (c >= '0' && c <= '9') || (c == '.')

    exactlyThree :: [a] -> Maybe (a, a, a)
    exactlyThree [x, y, z]  = Just (x, y, z)
    exactlyThree _otherwise = Nothing

    readParts :: (String, String, String) -> Maybe (Int, Int, Int)
    readParts (x, y, z) = (,,) <$> readMaybe x <*> readMaybe y <*> readMaybe z

{-------------------------------------------------------------------------------
  Version requirements
-------------------------------------------------------------------------------}

-- | Version requirement
--
-- @Requires a@ means that version @a@ or later is required.  For example,
-- @Requires 'Clang17_or_18_or_19'@ means that Clang 17 or later is required.
newtype Requires a = Requires a
  deriving stock (Show)

-- | Check @clang@ major version
--
-- Throw 'CallFailed' if the current Clang version is not greater than or equal
-- to the specified Clang version, or if the clang version is unknown.
requireClangVersion :: (MonadIO m, HasCallStack) => (Int, Int, Int) -> m ()
requireClangVersion v =
    case clangVersion of
      ClangVersion version | version >= v ->
       return ()
      _otherwise ->
        callFailedShow (Requires v)

{-------------------------------------------------------------------------------
  Low-level
-------------------------------------------------------------------------------}

foreign import capi "clang_wrappers.h wrap_getClangVersion"
  wrap_getClangVersion :: W CXString_ -> IO ()

clang_getClangVersion :: MonadIO m => m Text
clang_getClangVersion = liftIO $ preallocate_ $ wrap_getClangVersion

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Split on every occurrence of the separator
--
-- > splitOn '.' "18.1.3" == ["18","1","3"]
splitOn :: forall a. Eq a => a -> [a] -> [[a]]
splitOn sep = go
  where
    go :: [a] -> [[a]]
    go [] = []
    go xs = case break (== sep) xs of
              (pref, [])        -> [pref]
              (pref, _sep:rest) -> pref : go rest
