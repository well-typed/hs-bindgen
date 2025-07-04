module Main (main) where

import Control.Exception (Exception (..), SomeException (..), fromException,
                          handle, throwIO)
import Control.Monad (forM_, (<=<))
import Data.ByteString qualified as BS
import Data.Char (isLetter)
import System.Exit (ExitCode, exitFailure)
import Text.Read (readMaybe)

import HsBindgen.App.Cli
import HsBindgen.App.Common
import HsBindgen.Errors
import HsBindgen.Lib

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = handle exceptionHandler $ execCli =<< getCli

data LiterateFileException = LiterateFileException FilePath String
  deriving Show

instance Exception LiterateFileException where
    toException = hsBindgenExceptionToException
    fromException = hsBindgenExceptionFromException
    displayException (LiterateFileException path err) =
      "error loading " ++ path ++ ": " ++ err

execCli :: Cli -> IO ()
execCli Cli{..} = case cliCmd of
    CliCmdPreprocess  cmdOpts -> execPreprocess      cliGlobalOpts cmdOpts
    CliCmdGenTests    cmdOpts -> execGenTests        cliGlobalOpts cmdOpts
    CliCmdLiterate    cmdOpts -> execLiterate                      cmdOpts
    CliCmdBindingSpec subCmd  -> execBindingSpec     cliGlobalOpts subCmd
    CliCmdResolve     cmdOpts -> execResolve         cliGlobalOpts cmdOpts

execPreprocess :: GlobalOpts -> PreprocessOpts -> IO ()
execPreprocess globalOpts PreprocessOpts{..} = do
    hsDecls <- fromMaybeWithFatalError <=< withTracer globalOpts $ \tracer -> do
      extSpec <- loadExtBindingSpecs' tracer globalOpts
      pSpec   <- loadPrescriptiveBindingSpec' tracer globalOpts
      translateCHeaders mu tracer config extSpec pSpec preprocessInputs

    preprocessIO config preprocessOutput hsDecls

    case preprocessGenBindingSpec of
      Nothing   -> return ()
      Just path -> genBindingSpec config preprocessInputs path hsDecls
  where
    mu     = getModuleUnique preprocessModuleOpts
    config = (getConfig globalOpts) {
        configTranslation  = preprocessTranslationOpts
      , configHsModuleOpts = preprocessModuleOpts
      , configHsRenderOpts = preprocessRenderOpts
      }

execGenTests :: GlobalOpts -> GenTestsOpts -> IO ()
execGenTests globalOpts GenTestsOpts{..} = do
    hsDecls <-
      fromMaybeWithFatalError <=< withTracer globalOpts $ \tracer -> do
        extSpec <- loadExtBindingSpecs' tracer globalOpts
        pSpec   <- loadPrescriptiveBindingSpec' tracer globalOpts
        translateCHeaders mu tracer config extSpec pSpec genTestsInputs

    genTests config genTestsInputs genTestsOutput hsDecls
  where
    mu     = getModuleUnique genTestsModuleOpts
    config = (getConfig globalOpts) {
        configTranslation  = genTestsTranslationOpts
      , configHsModuleOpts = genTestsModuleOpts
      , configHsRenderOpts = genTestsRenderOpts
      }

execLiterate :: LiterateOpts -> IO ()
execLiterate LiterateOpts{..} = do
    args <- maybe (throw' "cannot parse literate file") return . readMaybe
      =<< readFile literateInput
    case pureParseCmdPreprocess args of
      Just cli -> execCli cli {
          cliCmd = case cliCmd cli of
            CliCmdPreprocess cmdOpts -> CliCmdPreprocess $
              cmdOpts { preprocessOutput = Just literateOutput }
            cliCmd'                  -> cliCmd'
        }
      Nothing -> throw' "cannot parse arguments in literate file"
  where
    throw' :: String -> IO a
    throw' = throwIO . LiterateFileException literateInput

execBindingSpec :: GlobalOpts -> BindingSpecCmd -> IO ()
execBindingSpec globalOpts@GlobalOpts{..} BindingSpecCmdStdlib = do
    spec <- fromMaybeWithFatalError =<< withTracer globalOpts (\tracer ->
      getStdlibBindingSpec tracer globalOptsClangArgs)
    BS.putStr $ encodeBindingSpecYaml spec

execResolve :: GlobalOpts -> ResolveOpts -> IO ()
execResolve globalOpts@GlobalOpts{..} ResolveOpts{..} = do
    mErr <- withTracer globalOpts $ \tracer -> do
      let tracerResolve = contramap TraceResolveHeader  tracer
      forM_ resolveInputs $ \header -> do
        mPath <- resolveHeader tracerResolve globalOptsClangArgs header
        putStrLn . unwords $ case mPath of
          Just path -> [show header, "resolves to", show path]
          Nothing   -> [show header, "not found"]
    fromMaybeWithFatalError mErr

-- to avoid potential issues it would be great to include unitid in module
-- unique but AFAIK there is no way to get one for preprocessor
-- https://github.com/well-typed/hs-bindgen/issues/502
getModuleUnique :: HsModuleOpts -> ModuleUnique
getModuleUnique = ModuleUnique . filter isLetter . hsModuleOptsName

{-------------------------------------------------------------------------------
  Exception handling
-------------------------------------------------------------------------------}

exceptionHandler :: SomeException -> IO ()
exceptionHandler e@(SomeException e')
    | Just _ <- fromException e :: Maybe ExitCode
    = throwIO e'

    | Just (HsBindgenException e'') <- fromException e = do
      putStrLn $ displayException e''
      exitFailure

    -- truly unexpected exceptions
    | otherwise = do
      -- Note: displayException of internal exception
      -- this will ensure uniform behavior while `base`/GHC figures out the ending of exceptions and backtrace story
      putStrLn $ "Uncaught exception: " ++ displayException e'
      putStrLn "Please report this at https://github.com/well-typed/hs-bindgen/issues"
      -- TODO: we could print exception context here, but it seems to be empty for IOExceptions anyway.
      exitFailure
