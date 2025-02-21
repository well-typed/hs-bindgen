{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Exception (handle, SomeException (..), displayException, fromException)
import Control.Monad
import Text.Read (readMaybe)
import System.Exit (ExitCode)
import System.IO qualified as IO

import HsBindgen.App.Cmdline
import HsBindgen.Clang.Paths
import HsBindgen.Lib

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = handle exceptionHandler $ do
    cmdline@Cmdline{..} <- getCmdline

    let tracer :: Tracer IO String
        tracer = mkTracerIO cmdVerbosity

    execMode cmdline tracer (cmdMode)

execMode :: Cmdline -> Tracer IO String -> Mode -> IO ()
execMode cmdline@Cmdline{..} tracer = \case
    ModePreprocess{..} -> do
      includePath <- either fail return <=< resolveCIncludeAbsPathDirs $
        clangSystemIncludePathDirs cmdClangArgs
          ++ clangIncludePathDirs cmdClangArgs
      absPath <- either fail return
        =<< resolveHeader includePath preprocessInput
      cHeader <- parseC cmdline tracer absPath
      let hsModl = genModule preprocessInput preprocessTranslationOpts preprocessModuleOpts cHeader
      prettyHs preprocessRenderOpts preprocessOutput hsModl
    ModeGenTests{..} -> do
      includePath <- either fail return <=< resolveCIncludeAbsPathDirs $
        clangSystemIncludePathDirs cmdClangArgs
          ++ clangIncludePathDirs cmdClangArgs
      absPath <- either fail return
        =<< resolveHeader includePath genTestsInput
      cHeader <- parseC cmdline tracer absPath
      genTests genTestsInput cHeader genTestsModuleOpts genTestsRenderOpts genTestsOutput
    ModeLiterate input output -> do
      lit <- readFile input
      args <- maybe (fail "cannot parse literate file") return $ readMaybe lit
      mode <- maybe (fail "cannot parse arguments in literate file") return $ pureParseModePreprocess args
      execMode cmdline tracer $ case mode of
          ModePreprocess {} -> mode { preprocessOutput = Just output }
          _ -> mode

    Dev devMode ->
      execDevMode cmdline tracer devMode

execDevMode :: Cmdline -> Tracer IO String -> DevMode -> IO ()
execDevMode cmdline@Cmdline{..} tracer = \case
    DevModeParseCHeader{..} -> do
      includePath <- either fail return <=< resolveCIncludeAbsPathDirs $
        clangSystemIncludePathDirs cmdClangArgs
          ++ clangIncludePathDirs cmdClangArgs
      absPath <- either fail return
        =<< resolveHeader includePath parseCHeaderInput
      prettyC =<< parseC cmdline tracer absPath
    DevModePrelude{..} -> do
      includePath <- either fail return <=< resolveCIncludeAbsPathDirs $
        clangSystemIncludePathDirs cmdClangArgs
          ++ clangIncludePathDirs cmdClangArgs
      absPath <- either fail return =<< resolveHeader includePath preludeInput
      IO.withFile preludeLogPath IO.WriteMode $ \logHandle -> do
        void . withC cmdline tracer absPath $
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
  -> CHeaderAbsPath
  -> (CXTranslationUnit -> IO r)
  -> IO r
withC cmdline tracer headerPath =
    withTranslationUnit traceWarnings (cmdClangArgs cmdline) headerPath
  where
    traceWarnings :: Tracer IO Diagnostic
    traceWarnings = contramap show tracer

parseC ::
     Cmdline
  -> Tracer IO String
  -> CHeaderAbsPath
  -> IO CHeader
parseC cmdline tracer headerPath =
    withC cmdline tracer headerPath $
      parseCHeader traceSkipped (cmdPredicate cmdline)
  where
    traceSkipped :: Tracer IO Skipped
    traceSkipped = (contramap prettyLogMsg tracer)

{-------------------------------------------------------------------------------
  Exception handling
-------------------------------------------------------------------------------}

exceptionHandler :: SomeException -> IO ()
exceptionHandler e@(SomeException e')
    | Just _ <- fromException e :: Maybe ExitCode
    = return ()

    -- truly unexpected exceptions
    | otherwise = do
      -- Note: displayException of internal exception
      -- this will ensure uniform behavior while `base`/GHC figures out the ending of exceptions and backtrace story
      putStrLn $ "Uncaught exception: " ++ displayException e'
      putStrLn "Please report this at https://github.com/well-typed/hs-bindgen/issues"
      -- TODO: we could print exception context here, but it seems to be empty for IOExceptions anyway.
