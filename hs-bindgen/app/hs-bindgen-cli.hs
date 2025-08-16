{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import Control.Exception (Exception (..), SomeException (..), fromException,
                          handle, throwIO)
import Control.Monad (forM, void, (<=<))
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Optics (set)
import System.Exit (ExitCode, exitFailure, exitSuccess)
import Text.Read (readMaybe)

import HsBindgen.Lib

-- NOTE: HsBindgen.Errors is an internal library.
import HsBindgen.Errors

import HsBindgen.App.Cli
import HsBindgen.App.Common

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = handle exceptionHandler $ execCli =<< getCli

execCli :: Cli -> IO ()
execCli Cli{..} = case cliCmd of
    CliCmdPreprocess  cmdOpts -> execPreprocess      cliGlobalOpts cmdOpts
    CliCmdGenTests    cmdOpts -> execGenTests        cliGlobalOpts cmdOpts
    CliCmdLiterate    cmdOpts -> execLiterate                      cmdOpts
    CliCmdBindingSpec subCmd  -> execBindingSpec     cliGlobalOpts subCmd
    CliCmdResolve     cmdOpts -> execResolve         cliGlobalOpts cmdOpts

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

execPreprocess :: GlobalOpts -> PreprocessOpts -> IO ()
execPreprocess GlobalOpts{..} PreprocessOpts{..} = do
    case outputBindingSpec of
      -- NOTE: We can not assemble the heterogeneous list of artefacts before
      -- evaluating `hsBindgen`. The types don't line up. (We even have to pull
      -- 'void' inside the case statement).
      Nothing ->
        let artefacts = writeBindings output :* Nil
        in  void $ run artefacts
      Just file ->
        let artefacts = writeBindings output :* writeBindingSpec file :* Nil
        in  void $ run $ artefacts
  where
    run :: Artefacts IO as -> IO (NP I as)
    run = hsBindgen tracerConfig bindgenConfig inputs

execGenTests :: GlobalOpts -> GenTestsOpts -> IO ()
execGenTests GlobalOpts{..} GenTestsOpts{..} = do
  let artefacts = writeTests output :* Nil
  void $ hsBindgen tracerConfig bindgenConfig inputs artefacts

execLiterate :: LiterateOpts -> IO ()
execLiterate opts = do
    args <- maybe (throw' "cannot parse literate file") return . readMaybe
      =<< readFile opts.input
    case pureParseCmdPreprocess args of
      Just cli -> execCli cli {
          cliCmd = case cli.cliCmd of
            CliCmdPreprocess cmdOpts -> CliCmdPreprocess $
              set #output (Just opts.output) cmdOpts
            cliCmd'                  -> cliCmd'
        }
      Nothing -> throw' "cannot parse arguments in literate file"
  where
    throw' :: String -> IO a
    throw' = throwIO . LiterateFileException opts.input

execBindingSpec :: GlobalOpts -> BindingSpecCmd -> IO ()
execBindingSpec GlobalOpts{..} BindingSpecCmdStdlib{..} = do
    spec <- either throwIO pure <=< withTracer tracerConfig $ \tracer ->
      getStdlibBindingSpec (contramap (TraceBoot . BootBindingSpec) tracer) clangArgs
    BS.putStr $ encodeBindingSpecYaml spec

-- TODO: Header resolution should be a (boot) artefact;
-- https://github.com/well-typed/hs-bindgen/issues/990.
execResolve :: GlobalOpts -> ResolveOpts -> IO ()
execResolve GlobalOpts{..} ResolveOpts{..} = do
    eErr <- withTracer tracerConfig' $ \tracer -> do
      hashIncludeArgs <- checkInputs tracer inputs
      includes <-
        resolveHeaders
          (contramap TraceResolveHeader tracer)
          clangArgs
          (Set.fromList hashIncludeArgs)
      fmap or . forM hashIncludeArgs $ \header ->
        case Map.lookup header includes of
          Just path -> (False <$) . putStrLn $
            "#include <" ++ getHashIncludeArg header
              ++ "> resolved to " ++ show path
          Nothing   -> (True  <$) . putStrLn $
            "#include <" ++ getHashIncludeArg header
              ++ "> could not be resolved (header not found)"
    case eErr of
      Right False -> exitSuccess
      Right True  -> exitFailure
      Left e      -> throwIO e
  where
    tracerConfig' :: TracerConfig IO Level TraceMsg
    tracerConfig' = tracerConfig{
        tCustomLogLevel = customLogLevel <> tCustomLogLevel tracerConfig
      }

    customLogLevel :: CustomLogLevel Level TraceMsg
    customLogLevel = CustomLogLevel $ \case
      TraceResolveHeader ResolveHeaderFound{}    -> Just Debug
      TraceResolveHeader ResolveHeaderNotFound{} -> Just Debug
      _otherTrace -> Nothing

{-------------------------------------------------------------------------------
  Exception handling
-------------------------------------------------------------------------------}

data LiterateFileException = LiterateFileException FilePath String
  deriving Show

instance Exception LiterateFileException where
    toException = hsBindgenExceptionToException
    fromException = hsBindgenExceptionFromException
    displayException (LiterateFileException path err) =
      "error loading " ++ path ++ ": " ++ err

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
