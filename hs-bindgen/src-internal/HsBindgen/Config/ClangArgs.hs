module HsBindgen.Config.ClangArgs (
    -- * Types
    ClangArgsConfig(..)
    -- ** Builtin include directory
  , BuiltinIncDirConfig(..)
    -- * Translation
  , InvalidClangArgs
  , clangArgsConfigToClangArgs
  ) where

import Clang.Args
import Clang.Discover

import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Configuration of @libclang@ command-line arguments
--
-- t'ClangArgsConfig' is not intended to be complete; instead, we have added
-- configuration options most relevant to @hs-bindgen@. Pass other
-- configurations options directly using command line arguments ('argsBefore',
-- 'argsInner', and 'argsAfter').
--
-- Configuration of the C standard must be done via one of these options.
-- @hs-bindgen@ queries @libclang@ to get the C standard.
data ClangArgsConfig path = ClangArgsConfig {
      -- | Builtin include directory configuration
      builtinIncDir :: BuiltinIncDirConfig

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
      builtinIncDir    = BuiltinIncDirClang
    , extraIncludeDirs = []
    , defineMacros     = []
    , enableBlocks     = False
    , argsBefore       = []
    , argsInner        = []
    , argsAfter        = []
    }

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

clangArgsConfigToClangArgs :: ClangArgsConfig FilePath -> ClangArgs
clangArgsConfigToClangArgs config = ClangArgs $ concat [
      config.argsBefore
    , argsInternal
    , config.argsInner
    , config.argsAfter
    ]
  where
    argsInternal :: [String]
    argsInternal = concat [
        concat [
            ["-I", path]
          | path <- config.extraIncludeDirs
          ]
      , concat [
            ["-D" ++ defn]
          | defn <- config.defineMacros
          ]
      , [ "-fblocks" | config.enableBlocks ]
      ]
