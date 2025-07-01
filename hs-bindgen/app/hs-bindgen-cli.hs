module Main (main) where

import Control.Exception (Exception (..), SomeException (..), fromException,
                          handle, throwIO)
import Control.Monad (foldM, unless)
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
main = handle exceptionHandler $ do
    cli <- getCli
    execMode cli

data LiterateFileException = LiterateFileException FilePath String
  deriving Show

instance Exception LiterateFileException where
    toException = hsBindgenExceptionToException
    fromException = hsBindgenExceptionFromException
    displayException (LiterateFileException path err) =
      "error loading " ++ path ++ ": " ++ err

execMode :: Cli -> IO ()
execMode Cli{cliGlobalOpts=GlobalOpts{..}, ..} = case cliMode of
    ModePreprocess{..} -> do
      hsDecls <- withTracer $ \tracer -> do
        extBindingSpec <-
          loadExtBindingSpecs
            tracer
            globalOptsClangArgs
            globalOptsStdlibSpecConf
            globalOptsExtBindings
        -- to avoid potential issues it would be great to include unitid in module
        -- unique but AFAIK there is no way to get one for preprocessor
        -- https://github.com/well-typed/hs-bindgen/issues/502
        let mu :: ModuleUnique
            mu = ModuleUnique $
              filter isLetter (hsModuleOptsName preprocessModuleOpts)
            opts = cmdOpts {
                optsExtBindingSpec = extBindingSpec
              , optsTranslation    = preprocessTranslationOpts
              , optsTracer         = tracer
              }
        translateCHeaders mu opts preprocessInputs
      withTracer $ \tracer -> do
        let ppOpts = (def :: PPOpts) {
                ppOptsModule = preprocessModuleOpts
              , ppOptsRender = preprocessRenderOpts
              }
        preprocessIO ppOpts preprocessOutput hsDecls
        case preprocessGenBindingSpec of
          Nothing   -> return ()
          Just path ->
            genBindingSpec tracer ppOpts preprocessInputs path hsDecls

    ModeGenTests{..} -> do
      extBindingSpec <- withTracer $ \tracer ->
        loadExtBindingSpecs tracer
          globalOptsClangArgs
          globalOptsStdlibSpecConf
          globalOptsExtBindings
      let opts = cmdOpts {
              optsExtBindingSpec = extBindingSpec
            }
          ppOpts = (def :: PPOpts) {
              ppOptsModule = genTestsModuleOpts
            , ppOptsRender = genTestsRenderOpts
            }
      genTests ppOpts genTestsInputs genTestsOutput
        =<< translateCHeaders "TODO" opts genTestsInputs

    ModeLiterate input output -> execLiterate input output

    ModeBindingSpec BindingSpecModeStdlib -> do
      spec <- withTracer $ \tracer ->
        getStdlibBindingSpec tracer globalOptsClangArgs
      BS.putStr $ encodeBindingSpecYaml spec

    ModeResolve{..} -> do
      isSuccess <- withTracer $ \tracer ->
        let tracerResolve = contramap TraceResolveHeader  tracer
            args          = optsClangArgs cmdOpts
            step isSuccess header =
              resolveHeader tracerResolve args header >>= \case
                Just path -> isSuccess <$ putStrLn path
                Nothing ->
                  False <$ putStrLn ("header not found: " ++ show header)
        in  foldM step True resolveInputs
      unless isSuccess exitFailure
  where
    cmdOpts :: Opts
    cmdOpts = def {
        optsClangArgs       = globalOptsClangArgs
      , optsPredicate       = globalOptsPredicate
      , optsProgramSlicing  = globalOptsProgramSlicing
      }
    withTracer :: (Tracer IO TraceMsg -> IO b) -> IO b
    withTracer = withTracerStdOut globalOptsTracerConf DefaultLogLevel

execLiterate :: FilePath -> FilePath -> IO ()
execLiterate input output = do
    args <- maybe (throw' "cannot parse literate file") return . readMaybe
      =<< readFile input
    case pureParseModePreprocess args of
      Just cli -> execMode cli { cliMode = case cliMode cli of
        mode@ModePreprocess{} -> mode { preprocessOutput = Just output }
        mode                  -> mode
        }
      Nothing -> throw' "cannot parse arguments in literate file"
  where
    throw' :: String -> IO a
    throw' = throwIO . LiterateFileException input

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
