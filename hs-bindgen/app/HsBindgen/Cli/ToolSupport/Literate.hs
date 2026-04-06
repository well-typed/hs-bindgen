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
import Data.Char (isUpper)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import GHC.Exception (Exception (..))
import Options.Applicative hiding (info)
import Options.Applicative qualified as O
import System.FilePath (dropExtension, splitDirectories)
import Text.Read (readMaybe)

import HsBindgen
import HsBindgen.App
import HsBindgen.App.Output (OutputMode (..), OutputOptions,
                             SingleFileCategory (..), buildCategoryChoice,
                             parseOutputOptions)
import HsBindgen.ArtefactM
import HsBindgen.Config
import HsBindgen.Config.Internal (BindgenConfig)
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

-- | Command line options when the literate preprocessor is invoked
--
-- NOTE: Most of the /actual/ @hs-bindgen-cli@ arguments come from parsing the
-- /contents/ of the file we are processing.
data Opts = Opts {
      input  :: FilePath
    , output :: FilePath
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

    return Opts{
        input  = input
      , output = output
      }

{-------------------------------------------------------------------------------
  Options (provided at the top of literate Haskell files)
-------------------------------------------------------------------------------}

data Lit = Lit {
      globalOpts     :: GlobalOpts
    , config         :: Config
    , uniqueId       :: UniqueId
    , baseModuleName :: BaseModuleName
    , qualifiedStyle :: QualifiedStyle
    , outputOptions  :: OutputOptions
    , inputs         :: [UncheckedHashIncludeArg]
    }

parseLit :: BaseModuleName -> Parser Lit
parseLit defaultModule = Lit
  <$> parseGlobalOpts
  <*> parseConfig
  <*> parseUniqueId
  <*> parseBaseModuleName defaultModule
  <*> parseQualifiedStyle
  <*> parseOutputOptions (SingleFile (SingleFileSafe "" :| []))
  <*> parseInputs

{-------------------------------------------------------------------------------
  Deriving module name from file path
-------------------------------------------------------------------------------}

-- | Derive module name from file path using the standard Haskell convention:
-- take path components starting from the first that begins with an uppercase
-- letter, and join them with dots.
--
-- For example:
--
-- > baseModuleNameFromFilePath "test/literate/Test/Literate/Test01.lhs"
-- >   == BaseModuleName "Test.Literate.Test01"
-- > baseModuleNameFromFilePath "app/SimpleBindings.lhs"
-- >   == BaseModuleName "SimpleBindings"
baseModuleNameFromFilePath :: FilePath -> BaseModuleName
baseModuleNameFromFilePath path =
    case moduleComponents of
      [] -> BaseModuleName "Generated"
      _  -> BaseModuleName . Text.pack $ intercalate "." moduleComponents
  where
    components :: [String]
    components = splitDirectories (dropExtension path)

    moduleComponents :: [String]
    moduleComponents = dropWhile (not . startsWithUpper) components

    startsWithUpper :: String -> Bool
    startsWithUpper []    = False
    startsWithUpper (c:_) = isUpper c

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: Opts -> IO ()
exec opts = do
    let defaultModule = baseModuleNameFromFilePath opts.input
    args <- maybe (throwIO' "cannot parse literate file") return . readMaybe
      =<< readFile opts.input
    lit <- handleParseResult $ pureParseLit defaultModule args

    let bindgenConfig :: BindgenConfig
        bindgenConfig =
          toBindgenConfig
            lit.config
            lit.uniqueId
            lit.baseModuleName
            (buildCategoryChoice lit.outputOptions)

        -- It is understood that literate mode will overwrite existing files
        -- (generated files will anyway live in @dist-newstyle@ or similar)
        filePolicy :: FilePolicy
        filePolicy = AllowFileOverwrite

        mrc :: ModuleRenderConfig
        mrc = ModuleRenderConfig {
            qualifiedStyle = lit.qualifiedStyle
          }

        artefact :: Artefact ()
        artefact = writeBindings mrc filePolicy DoNotCreateOutputDirs opts.output

    hsBindgen
      lit.globalOpts.unsafe
      lit.globalOpts.safe
      bindgenConfig
      lit.inputs
      artefact
  where
    throwIO' :: String -> IO a
    throwIO' = throwIO . LiterateFileException opts.input

    pureParseLit :: BaseModuleName -> [String] -> ParserResult Lit
    pureParseLit defaultModule =
        execParserPure (prefs subparserInline)
          (O.info (parseLit defaultModule) mempty)

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
