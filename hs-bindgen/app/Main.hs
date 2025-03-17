{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Exception (handle, SomeException (..), Exception (..), fromException, throwIO)
import Text.Read (readMaybe)
import Text.Show.Pretty qualified as Pretty
import System.Exit (ExitCode, exitFailure)

import HsBindgen.App.Cmdline
import HsBindgen.Errors
import HsBindgen.Lib
import HsBindgen.Pipeline qualified as Pipeline

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = handle exceptionHandler $ do
    cmdline@Cmdline{..} <- getCmdline

    let tracer :: Tracer IO String
        tracer = mkTracerIO cmdVerbosity

    execMode cmdline tracer (cmdMode)

data PackageNameRequiredException = PackageNameRequiredException
  deriving Show

instance Exception PackageNameRequiredException where
    toException = hsBindgenExceptionToException
    fromException = hsBindgenExceptionFromException
    displayException PackageNameRequiredException =
      "--package must be specified when using --gen-external-bindings"

data LiterateFileException = LiterateFileException FilePath String
  deriving Show

instance Exception LiterateFileException where
    toException = hsBindgenExceptionToException
    fromException = hsBindgenExceptionFromException
    displayException (LiterateFileException path err) =
      "error loading " ++ path ++ ": " ++ err

execMode :: Cmdline -> Tracer IO String -> Mode -> IO ()
execMode cmdline@Cmdline{..} tracer = \case
    ModePreprocess{..} -> do
      mGenExtBindings <-
        case (preprocessGenExtBindings, preprocessPackageName) of
          (Nothing, _packageName) -> return Nothing
          (Just extBindingsPath, Just packageName) ->
            return $ Just (extBindingsPath, packageName)
          (Just{}, Nothing) -> throwIO PackageNameRequiredException
      extBindings <- loadExtBindings' tracer cmdClangArgs cmdExtBindings
      let opts = cmdOpts {
              optsExtBindings = extBindings
            , optsTranslation = preprocessTranslationOpts
            }
          ppOpts = defaultPPOpts {
              ppOptsModule = preprocessModuleOpts
            , ppOptsRender = preprocessRenderOpts
            }
      decls <- translateCHeader opts preprocessInput
      preprocessIO ppOpts preprocessOutput decls
      case mGenExtBindings of
        Nothing -> return ()
        Just (path, packageName) ->
          genExtBindings ppOpts preprocessInput packageName path decls

    ModeGenTests{..} -> do
      extBindings <- loadExtBindings' tracer cmdClangArgs cmdExtBindings
      let opts = defaultOpts {
              optsExtBindings = extBindings
            }
          ppOpts = defaultPPOpts {
              ppOptsModule = genTestsModuleOpts
            , ppOptsRender = genTestsRenderOpts
            }
      genTests ppOpts genTestsInput genTestsOutput
        =<< translateCHeader opts genTestsInput

    ModeLiterate input output -> execLiterate input output tracer

    Dev devMode -> execDevMode cmdline tracer devMode
  where
    cmdOpts :: Opts
    cmdOpts = defaultOpts {
        optsClangArgs  = cmdClangArgs
      , optsPredicate  = cmdPredicate
      , optsDiagTracer = tracer
      , optsSkipTracer = tracer
      }

execLiterate :: FilePath -> FilePath -> Tracer IO String -> IO ()
execLiterate input output tracer = do
    args <- maybe (throw' "cannot parse literate file") return . readMaybe
      =<< readFile input
    case pureParseModePreprocess args of
      Just cmdline -> execMode cmdline tracer $ case cmdMode cmdline of
        mode@ModePreprocess{} -> mode { preprocessOutput = Just output }
        mode                  -> mode
      Nothing -> throw' "cannot parse arguments in literate file"
  where
    throw' :: String -> IO a
    throw' = throwIO . LiterateFileException input


execDevMode :: Cmdline -> Tracer IO String -> DevMode -> IO ()
execDevMode Cmdline{..} tracer = \case
    DevModeParseCHeader{..} -> do
      extBindings <- loadExtBindings' tracer cmdClangArgs cmdExtBindings
      let opts = cmdOpts {
              optsExtBindings = extBindings
            }
      Pretty.dumpIO . snd =<< Pipeline.parseCHeader opts parseCHeaderInput
  where
    cmdOpts :: Opts
    cmdOpts = defaultOpts {
        optsClangArgs  = cmdClangArgs
      , optsPredicate  = cmdPredicate
      , optsDiagTracer = tracer
      , optsSkipTracer = tracer
      }

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

loadExtBindings' ::
     Tracer IO String
  -> ClangArgs
  -> [FilePath]
  -> IO ExtBindings
loadExtBindings' tracer args paths = do
    (resolveErrs, extBindings) <- loadExtBindings args paths
    mapM_ (traceWith tracer Warning . displayException) resolveErrs
    return extBindings

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
