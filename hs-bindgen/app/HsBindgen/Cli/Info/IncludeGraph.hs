-- | @hs-bindgen-cli info include-graph@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Info.IncludeGraph qualified as IncludeGraph
module HsBindgen.Cli.Info.IncludeGraph (
    -- * CLI help
    info
    -- * Options
  , Opts(..)
  , parseOpts
    -- * Execution
  , exec
  ) where

import Data.Either (partitionEithers)
import Options.Applicative hiding (info)

import HsBindgen
import HsBindgen.App
import HsBindgen.ArtefactM
import HsBindgen.Backend.Category
import HsBindgen.Config
import HsBindgen.Config.Internal (BindgenConfig)
import HsBindgen.Frontend.Predicate
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Output the include graph"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      config         :: Config
    , predicate      :: Boolean Regex
    , showPaths      :: Bool
    , output         :: Maybe FilePath
    , inputs         :: [UncheckedHashIncludeArg]
    , filePolicy     :: FilePolicy
    , dirPolicy      :: DirPolicy
    }

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseConfig
      <*> parsePredicate
      <*> parseShowPaths
      <*> optional parseOutput'
      <*> parseInputs
      <*> parseFilePolicy
      <*> parseDirPolicy

parsePredicate :: Parser (Boolean Regex)
parsePredicate = fmap merge . many . asum $ [
      fmap (Right . BIf) $ strOption $ mconcat [
          long "include"
        , metavar "PCRE"
        , help "Only include headers with paths that match PCRE (by default, include all)"
        ]
    , fmap (Left . BIf) $ strOption $ mconcat [
          long "exclude"
        , metavar "PCRE"
        , help "Exclude headers with paths that match PCRE"
        ]
    ]
  where
    merge :: Eq a => [Either (Boolean a) (Boolean a)] -> Boolean a
    merge = uncurry mergeBooleans . fmap defaultIncludeAll . partitionEithers

    defaultIncludeAll :: [Boolean a] -> [Boolean a]
    defaultIncludeAll = \case
      [] -> [BTrue]
      xs -> xs

parseShowPaths :: Parser Bool
parseShowPaths = switch $ mconcat [
      long "show-paths"
    , help "Show paths of include header files instead of their '#include' arguments"
    ]

parseOutput' :: Parser FilePath
parseOutput' = strOption $ mconcat [
      short 'o'
    , long "output"
    , metavar "PATH"
    , help "Output path for the graph"
    ]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec global opts =
    hsBindgen
      global.unsafe
      global.safe
      bindgenConfig
      opts.inputs
      artefact
  where
    artefact :: Artefact ()
    artefact =
      writeIncludeGraph
        opts.predicate
        opts.showPaths
        opts.filePolicy
        opts.dirPolicy
        opts.output

    bindgenConfig :: BindgenConfig
    bindgenConfig =
        toBindgenConfig
          opts.config
          (UniqueId       "unused-unique-id")
          (BaseModuleName "unused-module-name")
          (def :: ByCategory Choice)
