-- | Wrapper for calling `libclang`.
--

module HsBindgen.C.Wrapper (
    withTranslationUnit
  , withTranslationUnit2
  ) where


import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC.Stack (HasCallStack)
import System.Environment (lookupEnv)
import Text.Parsec (ParseError, parse)

import Clang.Args
import Clang.Enum.Bitfield
import Clang.Enum.Simple (SimpleEnum)
import Clang.HighLevel qualified as Clang
import Clang.LowLevel.Core
import Clang.Paths
import HsBindgen.Util.Parsec (arguments)

extraClangArgsEnvName :: String
extraClangArgsEnvName = "BINDGEN_EXTRA_CLANG_ARGS"

parseStringArguments :: String -> Either ParseError [String]
parseStringArguments str = parse arguments "" str

getExtraClangArgs :: MonadIO m => m [String]
getExtraClangArgs = do
  extraClangArgsStr <- liftIO $ lookupEnv extraClangArgsEnvName
  case extraClangArgsStr of
    Nothing -> pure []
    Just str -> case parseStringArguments str of
      -- TODO: Do we have proper logging? Use Tracer?
      Left err -> do
        liftIO $ putStrLn errMsg
        liftIO $ print err
        pure []
      Right args -> do
        liftIO $ putStrLn okMsg
        liftIO $ print args
        pure args
  where
    errMsg = "Failed parsing " <> extraClangArgsEnvName <> " with error:"
    okMsg  = "Picked up extra arguments from " <> extraClangArgsEnvName <> ":"

-- | Main @hs-bindgen@ entry point to @libclang@.
--
-- Like 'Clang.withTranslationUnit', but honors @hs-bindgen@-specific
-- environment variables.
withTranslationUnit ::
     (MonadIO m, MonadMask m, HasCallStack)
  => CXIndex
  -> SourcePath
  -> ClangArgs
  -> [CXUnsavedFile]
  -> BitfieldEnum CXTranslationUnit_Flags
  -> (CXTranslationUnit -> m a)
  -> m a
withTranslationUnit index src args unsavedFiles options action = do
  extraClangArgs <- getExtraClangArgs
  let args' = args { clangOtherArgs = clangOtherArgs args <> extraClangArgs }
  Clang.withTranslationUnit index src args' unsavedFiles options action

-- | Main @hs-bindgen@ entry point to @libclang@.
--
-- Like 'Clang.withTranslationUnit2', but honors @hs-bindgen@-specific
-- environment variables.
withTranslationUnit2 ::
     (MonadIO m, MonadMask m, HasCallStack)
  => CXIndex
  -> SourcePath
  -> ClangArgs
  -> [CXUnsavedFile]
  -> BitfieldEnum CXTranslationUnit_Flags
  -> (Either (SimpleEnum CXErrorCode) CXTranslationUnit -> m a)
  -> m a
withTranslationUnit2 index src args unsavedFiles options action = do
  extraClangArgs <- getExtraClangArgs
  let args' = args { clangOtherArgs = clangOtherArgs args <> extraClangArgs }
  Clang.withTranslationUnit2 index src args' unsavedFiles options action
