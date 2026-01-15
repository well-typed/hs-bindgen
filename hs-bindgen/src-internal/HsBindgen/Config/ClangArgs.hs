module HsBindgen.Config.ClangArgs (
    -- * Types
    ClangArgsConfig(..)
    -- ** C standard
  , CStandard(..)
  , Gnu(..)
  , getStdClangArg
    -- ** Builtin include directory
  , BuiltinIncDirConfig(..)
    -- * Translation
  , InvalidClangArgs
  , clangArgsConfigToClangArgs
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
      -- | C standard
      cStandard :: CStandard

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
      cStandard        = C17
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

clangArgsConfigToClangArgs ::
     ClangArgsConfig FilePath
  -> Either InvalidClangArgs ClangArgs
clangArgsConfigToClangArgs config = do
    argsInternal <- getArgsInternal
    return . ClangArgs $ concat [
        config.argsBefore
      , argsInternal
      , config.argsInner
      , config.argsAfter
      ]
  where
    getArgsInternal :: Either InvalidClangArgs [String]
    getArgsInternal = concat <$> sequence [
        List.singleton <$> getStdClangArg config.cStandard config.gnu

      , return $ concat $ [
            [ "-fblocks" | config.enableBlocks ]
          ]

      , return $ concat [
            ["-I", path]
          | path <- config.extraIncludeDirs
          ]

      , return $ concat [
            ["-D" ++ defn]
          | defn <- config.defineMacros
          ]
      ]
