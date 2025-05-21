module Main (main) where

import Control.Exception (Exception (..), SomeException (..), fromException,
                          handle, throwIO)
import Control.Tracer (Tracer)
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
    withTracerStdOut (globalOptsTracerConf cliGlobalOpts) $ \tracer ->
      execMode cli tracer cliMode

data LiterateFileException = LiterateFileException FilePath String
  deriving Show

instance Exception LiterateFileException where
    toException = hsBindgenExceptionToException
    fromException = hsBindgenExceptionFromException
    displayException (LiterateFileException path err) =
      "error loading " ++ path ++ ": " ++ err

execMode :: HasCallStack => Cli -> Tracer IO (TraceWithCallStack Trace) -> Mode -> IO ()
execMode Cli{..} tracer = \case
    ModePreprocess{..} -> do
      extBindings <- loadExtBindings' resolveHeaderTracer cliGlobalOpts
      let opts = cmdOpts {
              optsExtBindings = extBindings
            , optsTranslation = preprocessTranslationOpts
            }
          ppOpts = defaultPPOpts {
              ppOptsModule = preprocessModuleOpts
            , ppOptsRender = preprocessRenderOpts
            }
      -- to avoid potential issues it would be great to include unitid in module unique
      -- but AFAIK there is no way to get one for preprocessor
      -- https://github.com/well-typed/hs-bindgen/issues/502
      let mu :: ModuleUnique
          mu = ModuleUnique $ hsModuleOptsName $ preprocessModuleOpts
      decls <- translateCHeader mu opts preprocessInput
      preprocessIO ppOpts preprocessOutput decls
      case preprocessGenExtBindings of
        Nothing   -> return ()
        Just path -> genExtBindings ppOpts preprocessInput path decls

    ModeGenTests{..} -> do
      extBindings <- loadExtBindings' resolveHeaderTracer cliGlobalOpts
      let opts = defaultOpts {
              optsExtBindings = extBindings
            }
          ppOpts = defaultPPOpts {
              ppOptsModule = genTestsModuleOpts
            , ppOptsRender = genTestsRenderOpts
            }
      genTests ppOpts genTestsInput genTestsOutput
        =<< translateCHeader "TODO" opts genTestsInput

    ModeLiterate input output -> execLiterate input output tracer
  where
    resolveHeaderTracer :: Tracer IO (TraceWithCallStack ResolveHeaderException)
    resolveHeaderTracer = useTrace TraceResolveHeader tracer

    cmdOpts :: Opts
    cmdOpts = defaultOpts {
        optsClangArgs  = globalOptsClangArgs cliGlobalOpts
      , optsPredicate  = globalOptsPredicate cliGlobalOpts
      , optsTracer     = tracer
      }

execLiterate :: FilePath -> FilePath -> Tracer IO (TraceWithCallStack Trace) -> IO ()
execLiterate input output tracer = do
    args <- maybe (throw' "cannot parse literate file") return . readMaybe
      =<< readFile input
    case pureParseModePreprocess args of
      Just cli -> execMode cli tracer $ case cliMode cli of
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
