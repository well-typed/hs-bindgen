module HsBindgen.Config.ClangArgs (
    -- * Types
    ClangArgsConfig(..)
    -- ** C standard
  , CStandard(..)
  , Gnu(..)
  , getStdClangArg
    -- ** Cross-compilation
  , Target(..)
  , targetTriple
  , parseTargetTriple
  , parseTargetTripleLenient
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
      -- 'Nothing' compiles for the host architecture.
      target :: Maybe Target

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
-- We support a fixed set of target platforms.  Supporting anything outside the
-- [GHC tier 1 platforms](https://gitlab.haskell.org/ghc/ghc/-/wikis/platforms#tier-1-platforms)
-- would be difficult, although some tier 2 platforms /might/ also be possible.
--
-- There are some assumptions about the target platform embedded in the design
-- of @hs-bindgen@.  For example, we currently assume that the target is little
-- endian.  If assumptions like these are ever challenged because we support
-- another architecture, careful testing will be essential.
--
-- Machine architectures:
--
-- * @X86_64@: @x86_64@, @amd64@
-- * @X86@: @i386@, @i486@, @i586@, @i686@
-- * @AArch64@: @aarch64@, @arm64@
data Target =
    Target_Linux_GNU_X86_64
  | Target_Linux_GNU_X86
  | Target_Linux_GNU_AArch64
  | Target_Linux_Musl_X86_64
  | Target_Linux_Musl_AArch64
  | Target_Windows_MSVC_X86_64
  | Target_Windows_GNU_X86_64
  | Target_Darwin_X86_64
  | Target_Darwin_AArch64
  deriving stock (Show, Eq, Enum, Bounded)

-- | Target triple string for a given 'Target'
--
-- The function returns the canonical target triple supported by @hs-bindgen@.
-- Target triples specified on the command line or in a binding specification
-- must be one of these.
targetTriple :: Target -> String
targetTriple = \case
    Target_Linux_GNU_X86_64    -> "x86_64-pc-linux-gnu"
    Target_Linux_GNU_X86       -> "i386-pc-linux-gnu"
    Target_Linux_GNU_AArch64   -> "aarch64-pc-linux-gnu"
    Target_Linux_Musl_X86_64   -> "x86_64-pc-linux-musl"
    Target_Linux_Musl_AArch64  -> "aarch64-pc-linux-musl"
    Target_Windows_MSVC_X86_64 -> "x86_64-pc-windows-msvc"
    Target_Windows_GNU_X86_64  -> "x86_64-pc-windows-gnu"
    Target_Darwin_X86_64       -> "x86_64-apple-darwin"
    Target_Darwin_AArch64      -> "aarch64-apple-darwin"

-- | Parse a target triple string
--
-- This function only recognizes the canonical target triples supported by
-- @hs-bindgen@.
parseTargetTriple :: String -> Maybe Target
parseTargetTriple = (`lookup` targets)
  where
    targets :: [(String, Target)]
    targets = [
        (targetTriple target, target)
      | target <- [minBound..]
      ]

-- | Parse a target triple string leniently
--
-- This function should only be used to parse the host target triple as reported
-- by LLVM/Clang, translating it to one of the canonical target triples
-- supported by @hs-bindgen@.  Target triples are a mess, so this parser is
-- /not/ expected to be perfect.  In cases where the reported target triple
-- cannot be parsed, users should configure one of the canonical target triples.
parseTargetTripleLenient :: String -> Maybe Target
parseTargetTripleLenient tt =
    -- The target triple string is split by the dash character.  The following
    -- components are expected:
    --
    -- 1. A single component (1) is the machine architecture.
    -- 2. Any number of componenents (0+) are vendor information.
    -- 3. A single component (1) is the operating system.
    -- 4. Any number of componenents (0+) is the operating system environment.
    --
    -- The operating system component decides how the other components are
    -- interpreted.
    --
    -- With @darwin@ and @macos@ target triples, the operating system version is
    -- generally appended to the operating system component (without a dash).
    --
    -- With @windows@ @msvc@ target triples, the MSVC version may be appended to
    -- the operating system environment component (without a dash).
    case splitDash tt of
      (arch : ss)
        | Just (vendor, env) <- splitExact  "linux"   ss ->
            parseLinux arch vendor env
        | Just (vendor, env) <- splitExact  "windows" ss ->
            parseWindows arch vendor env
        | Just (vendor, env) <- splitPrefix "darwin"  ss ->
            parseDarwin arch vendor env
        | Just (vendor, env) <- splitPrefix "macosx"  ss ->
            parseDarwin arch vendor env
        | otherwise -> Nothing
      [] -> Nothing
  where
    parseLinux :: String -> [String] -> [String] -> Maybe Target
    parseLinux arch vendor env
      | vendor `notElem` [[], ["pc"], ["unknown"]] = Nothing
      | arch `elem` x86_64 = case env of
          ["gnu"]    -> Just Target_Linux_GNU_X86_64
          ["musl"]   -> Just Target_Linux_Musl_X86_64
          _otherwise -> Nothing
      | arch `elem` x86 = case env of
          ["gnu"]    -> Just Target_Linux_GNU_X86
          _otherwise -> Nothing
      | arch `elem` aarch64 = case env of
          ["gnu"]    -> Just Target_Linux_GNU_AArch64
          ["musl"]   -> Just Target_Linux_Musl_AArch64
          _otherwise -> Nothing
      | otherwise = Nothing

    parseWindows :: String -> [String] -> [String] -> Maybe Target
    parseWindows arch vendor env
      | arch `notElem` x86_64 = Nothing
      | vendor `notElem` [[], ["pc"], ["w64"], ["unknown"]] = Nothing
      | otherwise = case env of
          [s]
            | "msvc" `List.isPrefixOf` s -> Just Target_Windows_MSVC_X86_64
            | s == "gnu"                 -> Just Target_Windows_GNU_X86_64
            | s == "mingw32"             -> Just Target_Windows_GNU_X86_64
          _otherwise                     -> Nothing

    parseDarwin :: String -> [String] -> [String] -> Maybe Target
    parseDarwin arch vendor _env
      | vendor /= ["apple"] = Nothing
      | arch `elem` x86_64 = Just Target_Darwin_X86_64
      | arch `elem` aarch64 = Just Target_Darwin_AArch64
      | otherwise = Nothing

    x86_64, x86, aarch64 :: [String]
    x86_64   = ["x86_64", "amd64"]
    x86      = ["i386", "i486", "i586", "i686"]
    aarch64  = ["aarch64", "arm64"]

    splitDash :: String -> [String]
    splitDash "" = []
    splitDash s  = case List.break (== '-') s of
      (sL, '-' : sR) -> sL : splitDash sR
      (sL, _empty)   -> [sL]

    splitExact :: String -> [String] -> Maybe ([String], [String])
    splitExact k ss = case List.break (== k) ss of
      (ls,  _k : rs) -> Just (ls, rs)
      (_ls, [])      -> Nothing

    splitPrefix :: String -> [String] -> Maybe ([String], [String])
    splitPrefix prefix = aux []
      where
        aux :: [String] -> [String] -> Maybe ([String], [String])
        aux _acc [] = Nothing
        aux acc (s : ss) = case List.stripPrefix prefix s of
          Nothing -> aux (s : acc) ss
          Just "" -> Just (reverse acc, ss)
          Just s' -> Just (reverse acc, s' : ss)

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
      ifGiven (targetTriple <$> target) $ \t ->
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
