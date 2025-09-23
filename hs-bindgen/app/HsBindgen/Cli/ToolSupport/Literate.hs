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
import Control.Monad (void)
import GHC.Exception (Exception (..))
import Options.Applicative hiding (info)
import Options.Applicative qualified as O
import Text.Read (readMaybe)

import HsBindgen.App
import HsBindgen.Backend.SHs.AST
import HsBindgen.Config
import HsBindgen.Errors
import HsBindgen.Lib

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
    return Opts{..}

{-------------------------------------------------------------------------------
  Options (provided at the top of literate Haskell files)
-------------------------------------------------------------------------------}

data Lit = Lit {
      globalOpts    :: GlobalOpts
    , bindgenConfig :: BindgenConfig
    , safety        :: Safety
    , inputs        :: [UncheckedHashIncludeArg]
    }

parseLit :: Parser Lit
parseLit = Lit
  <$> parseGlobalOpts
  <*> parseBindgenConfig
  <*> parseSafety
  <*> parseInputs

parseSafety :: Parser Safety
parseSafety = asum [
      flag' Safe $ mconcat [
          long "safe"
        , help "Use _safe_ foreign function imports (default)"
        ]
    , flag' Unsafe $ mconcat [
          long "unsafe"
        , help "Use _unsafe_ foreign function imports"
        ]
    , pure Safe
    ]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: Opts -> IO ()
exec literateOpts = do
    args <- maybe (throwIO' "cannot parse literate file") return . readMaybe
      =<< readFile literateOpts.input
    Lit{..} <- maybe (throwIO' "cannot parse arguments in literate file") return $
      pureParseLit args
    let GlobalOpts{..} = globalOpts
    void $ hsBindgen tracerConfig tracerConfigBackend bindgenConfig inputs $
      writeBindings safety (Just literateOpts.output) :* Nil
  where
    throwIO' :: String -> IO a
    throwIO' = throwIO . LiterateFileException literateOpts.input

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
