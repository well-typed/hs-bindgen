module Main (main) where

import Control.Exception (Exception (..), SomeException (..), fromException,
                          handle, throwIO)
import Control.Monad (forM_)
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
execCli Cli{..} = case cliMode of
    CliModePreprocess  mode -> execPreprocess      cliGlobalOpts mode
    CliModeGenTests    mode -> execGenTests        cliGlobalOpts mode
    CliModeLiterate    mode -> execLiterate                      mode
    CliModeBindingSpec mode -> execModeBindingSpec cliGlobalOpts mode
    CliModeResolve     mode -> execModeResolve     cliGlobalOpts mode

execPreprocess :: GlobalOpts -> PreprocessMode -> IO ()
execPreprocess globalOpts PreprocessMode{..} = do
    hsDecls <- doTranslate
    preprocessIO ppOpts preprocessOutput hsDecls
    case preprocessGenBindingSpec of
      Nothing   -> return ()
      Just path -> withTracer globalOpts $ \tracer ->
        genBindingSpec tracer ppOpts preprocessInputs path hsDecls

  where
    doTranslate :: IO HsDecls
    doTranslate = withTracer globalOpts $ \tracer -> do
      extBindingSpec <- loadExtBindingSpecs' tracer globalOpts
      let mu = getModuleUnique preprocessModuleOpts
          opts = (getOpts globalOpts) {
              optsExtBindingSpec = extBindingSpec
            , optsTranslation    = preprocessTranslationOpts
            , optsTracer         = tracer
            }
      translateCHeaders mu opts preprocessInputs

    ppOpts :: PPOpts
    ppOpts = def {
        ppOptsModule = preprocessModuleOpts
      , ppOptsRender = preprocessRenderOpts
      }

execGenTests :: GlobalOpts -> GenTestsMode -> IO ()
execGenTests globalOpts GenTestsMode{..} = do
    hsDecls <- doTranslate
    genTests ppOpts genTestsInputs genTestsOutput hsDecls
  where
    doTranslate :: IO HsDecls
    doTranslate = withTracer globalOpts $ \tracer -> do
      extBindingSpec <- loadExtBindingSpecs' tracer globalOpts
      let mu = getModuleUnique genTestsModuleOpts
          opts = (getOpts globalOpts) {
              optsExtBindingSpec = extBindingSpec
            , optsTranslation    = genTestsTranslationOpts
            , optsTracer         = tracer
            }
      translateCHeaders mu opts genTestsInputs

    ppOpts :: PPOpts
    ppOpts = def {
        ppOptsModule = genTestsModuleOpts
      , ppOptsRender = genTestsRenderOpts
      }

execLiterate :: LiterateMode -> IO ()
execLiterate LiterateMode{..} = do
    args <- maybe (throw' "cannot parse literate file") return . readMaybe
      =<< readFile literateInput
    case pureParseModePreprocess args of
      Just cli -> execCli cli {
          cliMode = case cliMode cli of
            CliModePreprocess mode -> CliModePreprocess $
              mode { preprocessOutput = Just literateOutput }
            cliMode'               -> cliMode'
        }
      Nothing -> throw' "cannot parse arguments in literate file"
  where
    throw' :: String -> IO a
    throw' = throwIO . LiterateFileException literateInput

execModeBindingSpec :: GlobalOpts -> BindingSpecMode -> IO ()
execModeBindingSpec globalOpts@GlobalOpts{..} BindingSpecModeStdlib = do
    spec <- withTracer globalOpts $ \tracer ->
      getStdlibBindingSpec tracer globalOptsClangArgs
    BS.putStr $ encodeBindingSpecYaml spec

execModeResolve :: GlobalOpts -> ResolveMode -> IO ()
execModeResolve globalOpts@GlobalOpts{..} ResolveMode{..} =
    withTracer globalOpts $ \tracer -> do
      let tracerResolve = contramap TraceResolveHeader  tracer
      forM_ resolveInputs $ \header -> do
        mPath <- resolveHeader tracerResolve globalOptsClangArgs header
        putStrLn . unwords $ case mPath of
          Just path -> [show header, "resolves to", show path]
          Nothing   -> [show header, "not found"]

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
