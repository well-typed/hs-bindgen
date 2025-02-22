{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module HsBindgen.Clang.Args (
    ClangArgs(..)
  , CStandard(..)
  , defaultClangArgs
  , fromClangArgs
    -- * Cross-compilation
  , Target(..)
  , TargetEnv(..)
  , targetTriple
  ) where

import Control.Monad.Except

import HsBindgen.Clang.Paths
import HsBindgen.Clang.Version

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | @libclang@ command line arguments
--
-- The default standard when one is not specified depends on the Clang version
-- and has GNU extensions enabled.
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/83> (also #10 and #71).
-- We should support more of the command line arguments of @clang@.
data ClangArgs = ClangArgs {
      -- | Target architecture ('Nothing' to compile for the host architecture)
      --
      -- The environment can be overriden separately, if necessary.
      clangTarget :: Maybe (Target, TargetEnv)

      -- | C standard
    , clangCStandard :: Maybe CStandard

      -- | Enablee both standard system @#include@ directories and builtin @#include@ directories (@False@ will pass @-nostdinc@)
    , clangStdInc :: Bool

      -- | Enable GNU extensions when 'True'
    , clangEnableGnu :: Bool

      -- | Directories in the system include search path
    , clangSystemIncludePathDirs :: [CIncludePathDir]

      -- | Directories in the non-system include search path
    , clangIncludePathDirs :: [CIncludePathDir]

      -- | Other arguments
      --
      -- See https://clang.llvm.org/docs/ClangCommandLineReference.html
    , clangOtherArgs :: [String]
    }
  deriving stock (Show, Eq)

-- | C standard
--
-- References:
--
-- * "C Support in Clang"
--   <https://clang.llvm.org/c_status.html>
-- * "Differences between various standard modes" in the clang user manual
--   <https://clang.llvm.org/docs/UsersManual.html#differences-between-various-standard-modes>
--
-- We don't currently support @C2y@ because it requires @clang-19@ or later and
-- we have no reliable way to test for that (see 'ClangVersion').
data CStandard =
    C89
  | C99
  | C11
  | C17
  | C23
  deriving stock (Bounded, Enum, Eq, Ord, Show)

defaultClangArgs :: ClangArgs
defaultClangArgs = ClangArgs {
      clangTarget                = Nothing
    , clangStdInc                = False
    , clangCStandard             = Nothing
    , clangEnableGnu             = False
    , clangSystemIncludePathDirs = []
    , clangIncludePathDirs       = []
    , clangOtherArgs             = []
    }

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

fromClangArgs :: ClangArgs -> IO (Either String [String])
fromClangArgs ClangArgs{..} = aux [
      ifGiven (uncurry targetTriple <$> clangTarget) $ \target ->
        return ["-target", target]

    , ifGiven clangCStandard $ \case
        C89
          | clangEnableGnu -> return ["-std=gnu89"]
          | otherwise      -> return ["-std=c89"]
        C99
          | clangEnableGnu -> return ["-std=gnu99"]
          | otherwise      -> return ["-std=c99"]
        C11
          | clangEnableGnu -> return $
              if clangVersion == ClangOlderThan3_2
                then ["-std=gnu1x"]
                else ["-std=gnu11"]
          | otherwise      -> return $
              if clangVersion == ClangOlderThan3_2
                then ["-std=c1x"]
                else ["-std=c11"]
        C17
          | clangVersion < Clang6 -> throwError "C17 requires clang-6 or later"
          | clangEnableGnu -> return ["-std=gnu17"]
          | otherwise      -> return ["-std=c17"]
        -- We can use @-std=c23@ in @clang-18@ or later, but we have no reliable
        -- way of testing for that.
        C23
          | clangVersion < Clang9_or_10 ->
              throwError "C23 requires clang-9 or later"
          | clangEnableGnu -> return ["-std=gnu2x"]
          | otherwise      -> return ["-std=c2x"]

    , do
        sysIncPathDirs <- fmap (map getCIncludeAbsPathDir) . ExceptT $
          resolveCIncludeAbsPathDirs clangSystemIncludePathDirs
        incPathDirs <- fmap (map getCIncludeAbsPathDir) . ExceptT $
          resolveCIncludeAbsPathDirs clangIncludePathDirs
        return $
             [ "-nostdinc" | not clangStdInc ]
          ++ concat [["-isystem", path] | path <- sysIncPathDirs]
          ++ concat [["-I",       path] | path <- incPathDirs]

    , return clangOtherArgs
    ]
  where
    aux :: [ExceptT String IO [String]] -> IO (Either String [String])
    aux = runExceptT . fmap concat . sequence

    ifGiven ::
         Maybe a
      -> (a -> Except  String    [String])
      ->       ExceptT String IO [String]
    ifGiven Nothing  _ = return []
    ifGiven (Just a) f = ExceptT . return . runExcept $ f a

{-------------------------------------------------------------------------------
  Cross-compilation

  <https://llvm.org/doxygen/Triple_8h_source.html> is a useful reference for
  all choices supported by @clang@:

  * 'TargetArch' is a subset of @ArchType@
  * 'TargetSys' is a subset of @OSType@

  We do not explicitly specify a @SubArchType@ or @VendorType@, but they are
  implied by other choices.

  Notes on ARM:

  * @aarch@ stands for "ARM archicture"
  * @aarch64@ was introduced with Armv8-A.
    <https://en.wikipedia.org/wiki/AArch64>
  * This architecture also has a big-endian mode and a 32-bit mode, which
    @clang@ refers to as @aarch64_be@ and @aarch64_32@, respectively
    (neither of which we currently support).
  * The @clang@ @arm@ target refers /I think/ to older versions of the ARM
    architecture; but the situation is a bit unclear here.

    - For example, <https://www.freebsd.org/platforms/> uses

        @armv6@ and @armv7@ to refer to 32-bit ARMv6 and ARMv7 respectively
          (both of which are FreeBSD tier 2 platforms)
        @arm@ for the 32-bit little-endian ARM v4/v5
          (unsupported by FreeBSD)
        @armeb@ to "32-bit big-endian ARM"
          (for which they dropped support even earlier)

    - Of these only ARMv7 is supported at all by GHC (as a tier 2 platform),
      but @ghc@ calls this simply @arm@.

    For now we simply don't support any of these, but if we did, being explicit
    (FreeBSD style) might be advisable.
-------------------------------------------------------------------------------}

-- | Target platform
--
-- We don't use raw strings to denote platforms:
--
-- * In some cases /we/ need to make decisions based on the platform; for
--   example, we need ao import the appropriate module from @c-expr@ (Win64,
--   Posix32, Posix64). Unfortunately, although @clang-c@ has
--   @clang_getTranslationUnitTargetInfo@, the resulting type is opaque, and the
--   associated query funcitons are insufficient.
--
-- * There are some assumptions about the target platform embedded in the
--   design of @hs-bindgen@; for example, we currently assume that the target is
--   little endian. If assumptions like these are ever challenged because we
--   support another architecture, careful testing will be essential.
--
-- * Supporting anything outside the GHC tier 1 platforms would be difficult
--   <https://gitlab.haskell.org/ghc/ghc/-/wikis/platforms#tier-1-platforms>,
--   although some tier 2 platforms /might/ also be possible.
--
-- Notes on the translation to @clang@ triples:
--
-- * @X86_64@, @X86@ and @AArch64@ correspond to @x86_64@, @i386@ and @aarch64@,
--   respectively. Note that @arm64@ is an alias for @aarch64@ in @clang@.
-- * @MacOS@ corresponds to @macosx@ in @clang@; @clang@ also recognizes
--   @macos@ as an alias for @macosx@, but @darwin@ refers to a /different/
--   (presumably older) system.
-- * @Windows@ corresponds to @windows@ (confusingly, the internal name for this
--   in @clang@ is @Win32@, and @win32@ is an alias for @windows@).
--
-- For the vendor, we default to @pc@ on Linux and Windows and @apple@ for
-- MacOS.
data Target =
    Target_Linux_X86_64
  | Target_Linux_X86
  | Target_Linux_AArch64
  | Target_Windows_X86_64
  | Target_MacOS_X86_64
  | Target_MacOS_AArch64
  deriving stock (Show, Eq, Enum, Bounded)

-- | Target environment
--
-- For example, on Windows valid choices are @msvc@ (for Visual C++) or @gnu@
-- (for @gcc@); see <https://wetmelon.github.io/clang-on-windows.html>.
data TargetEnv =
    TargetEnvDefault
  | TargetEnvOverride String
  deriving stock (Show, Eq)

-- | Target triple, for use in cross-compilation
--
-- See <https://clang.llvm.org/docs/CrossCompilation.html>
targetTriple :: Target -> TargetEnv -> String
targetTriple target mEnv = concat [
      case target of
        Target_Linux_X86_64   -> "x86_64-pc-linux"
        Target_Linux_X86      -> "i386-pc-linux"
        Target_Linux_AArch64  -> "aarch64-pc-linux"
        Target_Windows_X86_64 -> "x86_64-pc-windows"
        Target_MacOS_X86_64   -> "x86_64-apple-macosx"
        Target_MacOS_AArch64  -> "aarch64-apple-macosx"
    , case mEnv of
        TargetEnvDefault      -> ""
        TargetEnvOverride env -> "-" ++ env
    ]
