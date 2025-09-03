-- | @hs-bindgen-cli dev binding-spec stdlib@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Dev.BindingSpec.StdLib qualified as StdLib
module HsBindgen.Cli.Dev.BindingSpec.StdLib (
    -- * CLI help
    info
    -- * Options
  , Opts(..)
  , parseOpts
    -- * Execution
  , exec
  ) where

import Control.Monad ((<=<))
import Data.ByteString qualified as BS

import Options.Applicative hiding (info)

import HsBindgen.Clang.BuiltinIncDir
import HsBindgen.Imports
import HsBindgen.Lib

import HsBindgen.App

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Write stdlib external binding specification"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      builtinIncDirConfig :: BuiltinIncDirConfig
    , clangArgs           :: ClangArgs
    }

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseBuiltinIncDirConfig
      <*> parseClangArgs

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec GlobalOpts{..} Opts{..} = do
    spec <- either throwIO pure <=< withTracer tracerConfig $ \tracer -> do
      clangArgs' <- applyBuiltinIncDir clangArgs <$>
        getBuiltinIncDir
          (contramap TraceBuiltinIncDir tracer)
          builtinIncDirConfig'
      getStdlibBindingSpec
        (contramap (TraceBoot . BootBindingSpec) tracer)
        clangArgs'
    BS.putStr $ encodeBindingSpecYaml spec
  where
    builtinIncDirConfig' :: BuiltinIncDirConfig
    builtinIncDirConfig' = case builtinIncDirConfig of
      BuiltinIncDirAuto -> BuiltinIncDirAutoWithOverflow ""
      config            -> config
