{-# LANGUAGE ApplicativeDo #-}

-- | @hs-bindgen-cli tool-support literate@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.ToolSupport.Literate qualified as Literate
module HsBindgen.Cli.ToolSupport.Literate (
    -- * CLI help
    info
    -- * Options
  , Opts(..)
  , parseOpts
  ) where

import Options.Applicative hiding (info)

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc $ mconcat [
      "Generate Haskell module from C header, acting as literate Haskell"
    , " preprocessor"
    ]

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      input  :: FilePath
    , output :: FilePath
    }

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
