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

import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Configuration of @libclang@ command-line arguments
--
-- We demand specification of the C standard since the default C standard
-- depends on the Clang version and if GNU extensions are enabled.
--
-- `ClangArgsConfig` is not intended to be complete; instead, we have added
-- configuration options most relevant to @hs-bindgen@. Pass other
-- configurations options directly using command line arguments ('argsBefore',
-- 'argsInner', and 'argsAfter').
data ClangArgsConfig path = ClangArgsConfig {
      -- | Target architecture
      --
      -- 'Nothing' compiles for the host architecture. The environment can be
      -- overriden separately, if necessary.
      target :: Maybe (Target, TargetEnv)

      -- | C standard
    , cStandard :: CStandard

      -- | Enable GNU extensions?
    , gnu :: Gnu

      -- | Builtin include directory configuration
    , builtinIncDir :: BuiltinIncDirConfig

      -- | Directories that will be added to the include search path
      --
      -- This corresponds to the [@-I@ Clang
      -- argument](https://clang.llvm.org/docs/ClangCommandLineReference.html#include-path-management).
    , extraIncludeDirs :: [path]

      -- | Preprocessor macro definitions
      --
      -- A definition of form @<macro>=<value>@ defines a macro with the
      -- specified value. A definition of form @<macro>@ defines a macro with
      -- value @1@.
      --
      -- This corresponds to the [@-D@ Clang
      -- argument](https://clang.llvm.org/docs/ClangCommandLineReference.html#preprocessor-options).
    , defineMacros :: [String]

      -- | Enable block support
      --
      -- Running code that uses blocks will need the blocks runtime. This is not
      -- always installed with Clang; for example, on Ubuntu this is a separate
      -- package @libblocksruntime-dev@. This package also provides the
      -- @Block.h@ header.
    , enableBlocks :: Bool

      -- | Arguments to prepend when calling @libclang@
      --
      -- See 'argsInner'.
    , argsBefore :: [String]

      -- | Arguments passed to @libclang@
      --
      -- Complete list of arguments passed to @libclang@:
      --
      -- @
      -- concat [
      --     'argsBefore'
      --   ,  argsInternal  -- other ClangArgsConfig options
      --   , 'argsInner'
      --   ,  argsExtra     -- BINDGEN_EXTRA_CLANG_ARGS
      --   , 'argsAfter'
      --   ,  argsBuiltin   -- builtin include directory
      --   ]
      -- @
      --
      -- See the [Clang command line
      -- reference](https://clang.llvm.org/docs/ClangCommandLineReference.html).
    , argsInner :: [String]

      -- | Arguments to append when calling @libclang@
      --
      -- See 'argsInner'.
    , argsAfter :: [String]
    }
  deriving stock (Show, Eq, Generic, Functor, Foldable, Traversable)

instance Default (ClangArgsConfig path) where
 def = ClangArgsConfig {
      target           = Nothing
    , cStandard        = C17
    , gnu              = DisableGnu
    , builtinIncDir    = def
    , extraIncludeDirs = []
    , defineMacros     = []
    , enableBlocks     = False
    , argsBefore       = []
    , argsInner        = []
    , argsAfter        = []
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
-- * Supporting anything outside the [GHC tier 1
--   platforms](https://gitlab.haskell.org/ghc/ghc/-/wikis/platforms#tier-1-platforms)
--   would be difficult , although some tier 2 platforms /might/ also be
--   possible.
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
-- (for @gcc@); see [Using Clang on
-- Windows](https://wetmelon.github.io/clang-on-windows.html).
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

getClangArgs :: ClangArgsConfig FilePath -> Either InvalidClangArgs ClangArgs
getClangArgs config = do
    argsInternal <- getClangArgsInternal config
    return . ClangArgs $ concat [
        argsBefore   config
      , argsInternal
      , argsInner    config
      , argsAfter    config
      ]

getClangArgsInternal :: ClangArgsConfig FilePath -> Either InvalidClangArgs [String]
getClangArgsInternal ClangArgsConfig{..} = concat <$> sequence [
      ifGiven (uncurry targetTriple <$> target) $ \t ->
        return ["-target", t]

    , List.singleton <$> getStdClangArg cStandard gnu

    , return $ concat $ [
          [ "-fblocks" | enableBlocks ]
        ]

    , return $ concat [
          ["-I", path]
        | path <- extraIncludeDirs
        ]

    , return $ concat [
          ["-D" ++ defn]
        | defn <- defineMacros
        ]
    ]
  where
    ifGiven :: Maybe a -> (a -> Either e [String]) -> Either e [String]
    ifGiven Nothing  _ = Right []
    ifGiven (Just a) f = f a
