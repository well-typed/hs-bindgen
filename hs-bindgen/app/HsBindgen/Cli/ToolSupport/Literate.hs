{-# LANGUAGE ApplicativeDo #-}

-- | @hs-bindgen-cli tool-support literate@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.ToolSupport.Literate qualified as Literate
module HsBindgen.Cli.ToolSupport.Literate (
    -- * CLI help
    info
    -- * Options (provided by @cabal-install@ on the command line)
  , Opts(..)
  , parseOpts
    -- * Execution
  , exec
  ) where

import Control.Exception (throwIO)
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Exception (Exception (..))
import Options.Applicative hiding (info)
import Options.Applicative qualified as O
import Text.Read (readMaybe)

import HsBindgen
import HsBindgen.App
import HsBindgen.App.Output (OutputMode (..), OutputOptions,
                             SingleFileCategory (..), buildCategoryChoice,
                             parseOutputOptions)
import HsBindgen.Config
import HsBindgen.Config.Internal (BindgenConfig)
import HsBindgen.DelayedIO
import HsBindgen.Errors
import HsBindgen.Frontend.RootHeader

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc $ mconcat [
      "Generate Haskell module from C header, acting as literate Haskell"
    , " preprocessor"
    ]

{-------------------------------------------------------------------------------
  Options (provided by @cabal-install@ on the command line)
-------------------------------------------------------------------------------}

data Opts = Opts {
      input               :: FilePath
    , output              :: FilePath
    , fileOverwritePolicy :: FileOverwritePolicy
    }
  deriving (Show, Eq)

parseOpts :: Parser Opts
parseOpts = do
    -- When @cabal-install@ calls GHC and the preprocessor, it passes some
    -- standard flags, which we do not (all) use.  In particular, it passes
    -- @-hide-all-packages@.
    _ <- strOption @String $ mconcat [
             short 'h'
           , metavar "IGNORED"
           , help "Ignore some preprocessor options provided by cabal-install"
           ]

    input  <- strArgument $ metavar "IN"
    output <- strArgument $ metavar "OUT"
    policy <- parseFileOverwritePolicy

    return Opts{
        input               = input
      , output              = output
      , fileOverwritePolicy = policy
      }

{-------------------------------------------------------------------------------
  Options (provided at the top of literate Haskell files)
-------------------------------------------------------------------------------}

data Lit = Lit {
      globalOpts     :: GlobalOpts
    , config         :: Config
    , uniqueId       :: UniqueId
    , baseModuleName :: BaseModuleName
    , outputOptions  :: OutputOptions
    , inputs         :: [UncheckedHashIncludeArg]
    }

parseLit :: Parser Lit
parseLit = Lit
  <$> parseGlobalOpts
  <*> parseConfig
  <*> parseUniqueId
  <*> parseBaseModuleName
  <*> parseOutputOptions (SingleFile (SingleFileSafe "" :| []))
  <*> parseInputs

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: Opts -> IO ()
exec opts = do
    args <- maybe (throwIO' "cannot parse literate file") return . readMaybe
      =<< readFile opts.input
    lit <- maybe (throwIO' "cannot parse arguments in literate file") return $
      pureParseLit args

    let bindgenConfig :: BindgenConfig
        bindgenConfig =
          toBindgenConfig
            lit.config
            lit.uniqueId
            lit.baseModuleName
            (buildCategoryChoice lit.outputOptions)

        artefact :: Artefact ()
        artefact = writeBindings opts.fileOverwritePolicy opts.output

    hsBindgen
      lit.globalOpts.unsafe
      lit.globalOpts.safe
      bindgenConfig
      lit.inputs
      artefact
  where
    throwIO' :: String -> IO a
    throwIO' = throwIO . LiterateFileException opts.input

    pureParseLit :: [String] -> Maybe Lit
    pureParseLit =
        getParseResult
      . execParserPure (prefs subparserInline) (O.info parseLit mempty)

{-------------------------------------------------------------------------------
  Exception
-------------------------------------------------------------------------------}

data LiterateFileException = LiterateFileException FilePath String
  deriving Show

instance Exception LiterateFileException where
    toException = hsBindgenExceptionToException
    fromException = hsBindgenExceptionFromException
    displayException (LiterateFileException path err) =
      "error loading " ++ path ++ ": " ++ err
