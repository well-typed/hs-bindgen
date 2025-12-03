-- | @hs-bindgen-cli info builtin-macros@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Info.BuiltinMacros qualified as BuiltinMacros
module HsBindgen.Cli.Info.BuiltinMacros (
    -- * CLI help
    info
    -- * Options
  , Opts(..)
  , parseOpts
    -- * Execution
  , exec
  ) where

import Data.Text qualified as Text
import Options.Applicative hiding (info)

import Clang.Args
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core

import HsBindgen.App
import HsBindgen.Boot
import HsBindgen.Clang
import HsBindgen.Config.ClangArgs
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Imports
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "List LLVM/Clang builtin macros"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

newtype Opts = Opts {
      clangArgsConfig :: ClangArgsConfig FilePath
    }

parseOpts :: Parser Opts
parseOpts = Opts <$> parseClangArgsConfig

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec GlobalOpts{..} Opts{..} =
    void . withTracer tracerConfig $ \tracer _ -> do
      let tracerBoot  = contramap TraceBoot tracer
          tracerClang = contramap (TraceFrontend . FrontendClang) tracer
      (clangArgs, _target) <- getClangArgsAndTarget tracerBoot clangArgsConfig
      names <- getBuiltinMacroNames tracerClang clangArgs
      mapM_ (putStrLn . Text.unpack . C.getName) names

getBuiltinMacroNames :: Tracer ClangMsg -> ClangArgs -> IO [C.Name]
getBuiltinMacroNames tracer clangArgs =
    fmap (fromMaybe []) . withClang' tracer setup $ \unit -> do
      root <- clang_getTranslationUnitCursor unit
      Just <$> HighLevel.clang_visitChildren root visit
  where
    setup :: ClangSetup
    setup = defaultClangSetup clangArgs $
      ClangInputMemory "hs-bindgen-builtins.h" ""

    visit :: Fold IO C.Name
    visit = simpleFold $ \curr ->
      C.getPrelimDeclId curr C.NameKindOrdinary >>= \case
        C.PrelimDeclIdBuiltin name _kind -> foldContinueWith name
        _otherwise                       -> foldBreak
