module HsBindgen.Config.ClangArgs (
    -- * Types
    ClangArgsConfig(..)
    -- ** C standard
  , CStandard(..)
  , Gnu(..)
  , getStdClangArg
    -- ** Cross-compilation
  , Target(..)
  , TargetEnv(..)
  , targetTriple
    -- ** Builtin include directory
  , BuiltinIncDirConfig(..)
    -- * Translation
  , InvalidClangArgs
  , getClangArgs
  ) where

import Data.List qualified as List

import Clang.Args
import Clang.Paths

import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Configuration of @libclang@ command-line arguments
--
-- The default standard when one is not specified depends on the Clang version
-- and if GNU extensions are enabled or not.
--
-- This is not intended to be complete; we have added the arguments that are
-- most relevant to @hs-bindgen@.
data ClangArgsConfig = ClangArgsConfig {
      -- | Target architecture ('Nothing' to compile for the host architecture)
      --
      -- The environment can be overriden separately, if necessary.
      clangTarget :: Maybe (Target, TargetEnv)

      -- | C standard
    , clangCStandard :: Maybe CStandard

      -- | Enable GNU extensions?
    , clangGnu :: Gnu

      -- | Builtin include directory configuration
    , clangBuiltinIncDir :: BuiltinIncDirConfig

      -- | Directories that will be added to the include search path
      --
      -- This corresponds to the @-I@ clang argument.  See
      -- <https://clang.llvm.org/docs/ClangCommandLineReference.html#include-path-management>.
    , clangExtraIncludeDirs :: [CIncludeDir]

      -- | Preprocessor macro definitions
      --
      -- A definition of form @<macro>=<value>@ defines a macro with the
      -- specified value.  A definition of form @<macro>@ defines a macro with
      -- value @1@.
      --
      -- This corresponds to the @-D@ clang argument.  See
      -- <https://clang.llvm.org/docs/ClangCommandLineReference.html#preprocessor-options>.
    , clangDefineMacros :: [String]

      -- | Enable block support
      --
      -- NOTE: Running code that uses blocks will need the blocks runtime. This
      -- is not always installed with clang; for example, on Ubuntu this is a
      -- separate package @libblocksruntime-dev@. This package also provides the
      -- @Block.h@ header.
    , clangEnableBlocks :: Bool

      -- | Arguments to prepend when calling @libclang@
      --
      -- See 'clangArgsInner'.
    , clangArgsBefore :: [String]

      -- | Arguments passed to @libclang@
      --
      -- Complete list of arguments passed to @libclang@:
      --
      -- @
      -- concat [
      --     'clangArgsBefore'
      --   , clangArgsInternal  -- other ClangArgsConfig options
      --   , 'clangArgsInner'
      --   , clangArgsExtra     -- BINDGEN_EXTRA_CLANG_ARGS
      --   , 'clangArgsAfter'
      --   , clangArgsBuiltin   -- builtin include directory
      --   ]
      -- @
      --
      -- See https://clang.llvm.org/docs/ClangCommandLineReference.html
    , clangArgsInner :: [String]

      -- | Arguments to append when calling @libclang@
      --
      -- See 'clangArgsInner'.
    , clangArgsAfter :: [String]
    }
  deriving stock (Show, Eq)

instance Default ClangArgsConfig where
 def = ClangArgsConfig {
      clangTarget           = Nothing
    , clangCStandard        = Nothing
    , clangGnu              = DisableGnu
    , clangBuiltinIncDir    = def
    , clangExtraIncludeDirs = []
    , clangDefineMacros     = []
    , clangEnableBlocks     = False
    , clangArgsBefore       = []
    , clangArgsInner        = []
    , clangArgsAfter        = []
    }

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

{-------------------------------------------------------------------------------
  Builtin include directory
-------------------------------------------------------------------------------}

-- | Configure builtin include directory automatic configuration
data BuiltinIncDirConfig =
    -- | Do not configure the builtin include directory
    BuiltinIncDirDisable

    -- | Configure the builtin include directory using the resource directory
    -- from @clang@
  | BuiltinIncDirClang
  deriving (Eq, Show)

instance Default BuiltinIncDirConfig where
  def = BuiltinIncDirClang

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

getClangArgs :: ClangArgsConfig -> Either InvalidClangArgs ClangArgs
getClangArgs config = do
    clangArgsInternal <- getClangArgsInternal config
    return . ClangArgs $ concat [
        clangArgsBefore   config
      , clangArgsInternal
      , clangArgsInner    config
      , clangArgsAfter    config
      ]

getClangArgsInternal :: ClangArgsConfig -> Either InvalidClangArgs [String]
getClangArgsInternal ClangArgsConfig{..} = concat <$> sequence [
      ifGiven (uncurry targetTriple <$> clangTarget) $ \target ->
        return ["-target", target]

    , ifGiven clangCStandard $ \cStandard ->
        List.singleton <$> getStdClangArg cStandard clangGnu

    , return $ concat $ [
          [ "-fblocks" | clangEnableBlocks ]
        ]

    , return $ concat [
          ["-I", getCIncludeDir path]
        | path <- clangExtraIncludeDirs
        ]

    , return $ concat [
          ["-D" ++ defn]
        | defn <- clangDefineMacros
        ]
    ]
  where
    ifGiven :: Maybe a -> (a -> Either e [String]) -> Either e [String]
    ifGiven Nothing  _ = Right []
    ifGiven (Just a) f = f a
