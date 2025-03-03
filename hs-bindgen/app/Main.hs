{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Exception (handle, SomeException (..), Exception (..), fromException, throwIO)
import Control.Monad
import Text.Read (readMaybe)
import System.Exit (ExitCode, exitFailure)
import System.IO qualified as IO

import HsBindgen.App.Cmdline
import HsBindgen.Clang.Paths
import HsBindgen.Lib
import HsBindgen.Errors

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = handle exceptionHandler $ do
    cmdline@Cmdline{..} <- getCmdline

    let tracer :: Tracer IO String
        tracer = mkTracerIO cmdVerbosity

    execMode cmdline tracer (cmdMode)

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
      src <- resolveHeader cmdClangArgs preprocessInput
      extBindings <- either (throwIO . HsBindgenException) return
        =<< loadExtBindings cmdClangArgs cmdExtBindings
      cHeader <- parseC cmdline tracer extBindings src
      let hsModl = genModule preprocessInput preprocessTranslationOpts preprocessModuleOpts cHeader
      prettyHs preprocessRenderOpts preprocessOutput hsModl
    ModeGenTests{..} -> do
      src <- resolveHeader cmdClangArgs genTestsInput
      extBindings <- either (throwIO . HsBindgenException) return
        =<< loadExtBindings cmdClangArgs cmdExtBindings
      cHeader <- parseC cmdline tracer extBindings src
      genTests genTestsInput cHeader genTestsModuleOpts genTestsRenderOpts genTestsOutput
    ModeLiterate input output -> do
      lit <- readFile input
      args <- maybe (throwIO $ LiterateFileException input "cannot parse literate file") return $ readMaybe lit
      mode <- maybe (throwIO $ LiterateFileException input "cannot parse arguments in literate file") return $ pureParseModePreprocess args
      execMode cmdline tracer $ case mode of
          ModePreprocess {} -> mode { preprocessOutput = Just output }
          _ -> mode

    Dev devMode ->
      execDevMode cmdline tracer devMode

execDevMode :: Cmdline -> Tracer IO String -> DevMode -> IO ()
execDevMode cmdline@Cmdline{..} tracer = \case
    DevModeParseCHeader{..} -> do
      src <- resolveHeader cmdClangArgs parseCHeaderInput
      extBindings <- either (throwIO . HsBindgenException) return
        =<< loadExtBindings cmdClangArgs cmdExtBindings
      prettyC =<< parseC cmdline tracer extBindings src
    DevModePrelude{..} -> do
      src <- resolveHeader cmdClangArgs preludeInput
      IO.withFile preludeLogPath IO.WriteMode $ \logHandle -> do
        void . withC cmdline tracer src $
          bootstrapPrelude tracer (preludeLogTracer logHandle)
  where
    preludeLogPath :: FilePath
    preludeLogPath = "macros-recognized.log"

    preludeLogTracer :: IO.Handle -> Tracer IO String
    preludeLogTracer logHandle =
      mkTracer
        (IO.hPutStrLn logHandle . ("Error: "   ++))
        (IO.hPutStrLn logHandle . ("Warning: " ++))
        (IO.hPutStrLn logHandle)
        True

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

withC ::
     Cmdline
  -> Tracer IO String
  -> SourcePath
  -> (CXTranslationUnit -> IO r)
  -> IO r
withC cmdline tracer src =
    withTranslationUnit traceWarnings (cmdClangArgs cmdline) src
  where
    traceWarnings :: Tracer IO Diagnostic
    traceWarnings = contramap show tracer

parseC ::
     Cmdline
  -> Tracer IO String
  -> ExtBindings
  -> SourcePath
  -> IO CHeader
parseC cmdline tracer extBindings src =
    withC cmdline tracer src $
      parseCHeader traceSkipped (cmdPredicate cmdline) extBindings
  where
    traceSkipped :: Tracer IO Skipped
    traceSkipped = (contramap prettyLogMsg tracer)

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
