{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import Control.Exception (Exception (..), SomeException (..), fromException,
                          handle, throwIO)
import Control.Monad (forM_, void, (<=<))
import Data.ByteString qualified as BS
import Data.Char (isLetter)
import Optics
import System.Exit (ExitCode, exitFailure)
import Text.Read (readMaybe)

import HsBindgen.Lib

-- NOTE: HsBindgen.Errors is an internal library.
import HsBindgen.Errors

import HsBindgen
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
      -- TODO: We can not assemble the heterogeneous list of artefacts before
      -- evaluating `hsBindgen`. The types don't line up. (We even have to pull
      -- 'void' inside the case statement).
      Nothing   -> void $ run $ (writeBindings output) :* Nil
      Just file -> void $ run $ (writeBindings output) :* writeBindingSpec file :* Nil
  where
    moduleUnique = getModuleUnique config.configHsModuleOpts
    run :: Artefacts as -> IO (NP I as)
    run = hsBindgen tracerConfig moduleUnique config bindingSpecConfig inputs

execGenTests :: GlobalOpts -> GenTestsOpts -> IO ()
execGenTests GlobalOpts{..} GenTestsOpts{..} = do
  let artefacts = writeTests output :* Nil
  void $ hsBindgen tracerConfig moduleUnique config bindingSpecConfig inputs artefacts
  where
    moduleUnique = getModuleUnique config.configHsModuleOpts

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
execBindingSpec globalOpts BindingSpecCmdStdlib{..} = do
    spec <- fromMaybeWithFatalError <=< withCliTracer globalOpts $ \tracer ->
      getStdlibBindingSpec (contramap TraceBindingSpec tracer) clangArgs
    BS.putStr $ encodeBindingSpecYaml spec

execResolve :: GlobalOpts -> ResolveOpts -> IO ()
execResolve globalOpts opts = do
    mErr <- withCliTracer globalOpts $ \tracer -> do
      let tracerResolve = contramap TraceResolveHeader  tracer
      inputs <- checkInputs tracer opts.inputs
      forM_ inputs $ \header -> do
        mPath <- resolveHeader tracerResolve opts.clangArgs header
        putStrLn . unwords $ case mPath of
          Just path -> [show header, "resolves to", show path]
          Nothing   -> [show header, "not found"]
    fromMaybeWithFatalError mErr

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- to avoid potential issues it would be great to include unitid in module
-- unique but AFAIK there is no way to get one for preprocessor
-- https://github.com/well-typed/hs-bindgen/issues/502
getModuleUnique :: HsModuleOpts -> ModuleUnique
getModuleUnique = ModuleUnique . filter isLetter . hsModuleOptsName

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
