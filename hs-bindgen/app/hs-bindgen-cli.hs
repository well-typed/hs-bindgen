module Main (main) where

import Control.Exception (Exception (..), SomeException (..), fromException,
                          handle, throwIO)
import Control.Tracer (Tracer)
import Data.Char (isLetter)
import GHC.Stack (HasCallStack)
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
    cli@Cli{..} <- getCli
    execMode cli cliMode

data LiterateFileException = LiterateFileException FilePath String
  deriving Show

instance Exception LiterateFileException where
    toException = hsBindgenExceptionToException
    fromException = hsBindgenExceptionFromException
    displayException (LiterateFileException path err) =
      "error loading " ++ path ++ ": " ++ err

execMode :: HasCallStack => Cli -> Mode -> IO ()
execMode Cli{..} = \case
    ModePreprocess{..} -> genDecls >>= outputDecls
      where
        genDecls = withTracer $ \tracer -> do
          extBindings <- loadExtBindings' tracer cliGlobalOpts
          let opts = cmdOpts {
                  optsExtBindings = extBindings
                , optsTranslation = preprocessTranslationOpts
                , optsTracer      = tracer
                }
          -- to avoid potential issues it would be great to include unitid in module unique
          -- but AFAIK there is no way to get one for preprocessor
          -- https://github.com/well-typed/hs-bindgen/issues/502
          let mu :: ModuleUnique
              mu = ModuleUnique $ filter isLetter $ hsModuleOptsName $ preprocessModuleOpts
          translateCHeaders mu opts preprocessInputs
        outputDecls decls = do
          let ppOpts = (def :: PPOpts) {
                ppOptsModule = preprocessModuleOpts
              , ppOptsRender = preprocessRenderOpts
              }
          preprocessIO ppOpts preprocessOutput decls
          case preprocessGenExtBindings of
            Nothing   -> return ()
            Just path -> genExtBindings ppOpts preprocessInputs path decls

    ModeGenTests{..} -> do
      extBindings <- withTracer $ \tracer -> loadExtBindings' tracer cliGlobalOpts
      let opts = (def :: Opts) {
              optsExtBindings = extBindings
            }
          ppOpts = (def :: PPOpts) {
              ppOptsModule = genTestsModuleOpts
            , ppOptsRender = genTestsRenderOpts
            }
      genTests ppOpts genTestsInputs genTestsOutput
        =<< translateCHeaders "TODO" opts genTestsInputs

    ModeLiterate input output -> execLiterate input output
  where
    cmdOpts :: Opts
    cmdOpts = def {
        optsClangArgs  = globalOptsClangArgs cliGlobalOpts
      , optsPredicate  = globalOptsPredicate cliGlobalOpts
      }
    withTracer :: (Tracer IO (TraceWithCallStack Trace) -> IO b) -> IO b
    withTracer = withTracerStdOut (globalOptsTracerConf cliGlobalOpts) DefaultLogLevel

execLiterate :: FilePath -> FilePath -> IO ()
execLiterate input output = do
    args <- maybe (throw' "cannot parse literate file") return . readMaybe
      =<< readFile input
    case pureParseModePreprocess args of
      Just cli -> execMode cli $ case cliMode cli of
        mode@ModePreprocess{} -> mode { preprocessOutput = Just output }
        mode                  -> mode
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
