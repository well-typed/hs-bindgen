-- | @hs-bindgen-cli internal frontend@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Internal.Frontend qualified as Frontend
module HsBindgen.Cli.Internal.Frontend (
    -- * CLI help
    info
    -- * Options
  , Opts(..)
  , parseOpts
    -- * Execution
  , exec
  ) where

import Data.Default (Default (..))
import Data.List (intercalate)
import Options.Applicative hiding (info)

import HsBindgen
import HsBindgen.App
import HsBindgen.Artefact
import HsBindgen.Config
import HsBindgen.Config.Internal (BindgenConfig)
import HsBindgen.DelayedIO
import HsBindgen.Frontend.RootHeader

{-------------------------------------------------------------------------------
  Existential wrapper
-------------------------------------------------------------------------------}

-- | Existential wrapper pairing 'FrontendPass' with a 'Show' constraint.
data SomeFrontendPass where
  SomeFrontendPass :: Show result => FrontendPass result -> SomeFrontendPass

-- | Parse a frontend pass name (inverse of 'frontendPassName').
--
-- Returns 'Left' with an error message listing valid names on failure.
parseFrontendPassName :: String -> Either String SomeFrontendPass
parseFrontendPassName s = case lookup s knownPasses of
    Just d  -> Right d
    Nothing -> Left $
      "unknown pass " ++ show s ++ "; valid passes: "
        ++ intercalate ", " (map fst knownPasses)
  where
    knownPasses :: [(String, SomeFrontendPass)]
    knownPasses = [
          mk DumpParse
        , mk DumpSimplifyAST
        , mk DumpAssignAnonIds
        , mk DumpConstructTranslationUnit
        , mk DumpHandleMacros
        , mk DumpResolveBindingSpecs
        , mk DumpMangleNames
        , mk DumpSelect
        , mk DumpAdjustTypes
        ]

    mk :: Show result => FrontendPass result -> (String, SomeFrontendPass)
    mk d = (frontendPassName d, SomeFrontendPass d)

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Dump the result of a frontend pass"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      dump                :: SomeFrontendPass
    , config              :: Config
    , uniqueId            :: UniqueId
    , baseModuleName      :: BaseModuleName
    , inputs              :: [UncheckedHashIncludeArg]
    , fileOverwritePolicy :: FileOverwritePolicy
    }

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseDump
      <*> parseConfig
      <*> parseUniqueId
      <*> parseBaseModuleName
      <*> parseInputs
      <*> parseFileOverwritePolicy

parseDump :: Parser SomeFrontendPass
parseDump = option (eitherReader parseFrontendPassName) $ mconcat [
      long "pass"
    , value (SomeFrontendPass DumpAdjustTypes)
    , showDefaultWith (\(SomeFrontendPass d) -> frontendPassName d)
    , help "Frontend pass to dump"
    , metavar "PASS"
    ]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec global opts = case opts.dump of
    SomeFrontendPass pass ->
      hsBindgen
        global.unsafe
        global.safe
        bindgenConfig
        opts.inputs
        artefact
      where
        artefact :: Artefact ()
        artefact = do
            result <- RunFrontendPass pass
            Lift $ delay . WriteToStdOut . StringContent $ show result

        bindgenConfig :: BindgenConfig
        bindgenConfig =
            toBindgenConfig
              opts.config
              opts.uniqueId
              opts.baseModuleName
              def
