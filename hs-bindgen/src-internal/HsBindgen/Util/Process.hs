module HsBindgen.Util.Process (
    -- * Read actions
    cmdSpecReadAction
  , cmdSpecPrettyForTrace
  , checkOutput
  , parseSingleLine
  , parseFirstLine
  , parseNonEmpty
  ) where

import Data.Maybe (listToMaybe)
import System.IO.Error (tryIOError)
import System.Process qualified as Proc
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Read actions
-------------------------------------------------------------------------------}

-- | Interpret a 'Proc.CmdSpec' as a read action
cmdSpecReadAction :: Proc.CmdSpec -> IO String
cmdSpecReadAction = \case
    Proc.ShellCommand cmd    -> Proc.readCreateProcess (Proc.shell cmd) ""
    Proc.RawCommand exe args -> Proc.readProcess exe args ""

-- | Render a 'Proc.CmdSpec' for tracing
cmdSpecPrettyForTrace :: Proc.CmdSpec -> PP.CtxDoc
cmdSpecPrettyForTrace = PP.string . \case
    Proc.ShellCommand cmd    -> cmd
    Proc.RawCommand exe args -> unwords (exe : args)

-- | Run a read action and check the output
checkOutput ::
     Tracer msg
  -> (String  -> msg)      -- ^ Unexpected output constructor
  -> (IOError -> msg)      -- ^ Error constructor
  -> (String  -> Maybe a)  -- ^ Output parser
  -> IO String             -- ^ Read action
  -> IO (Maybe a)
checkOutput tracer mkUnexpected mkError parse action =
    tryIOError action >>= \case
      Right s -> case parse s of
        x@Just{} -> return x
        Nothing  -> Nothing <$ traceWith tracer (mkUnexpected (abbr s))
      Left  e -> Nothing <$ traceWith tracer (mkError e)
  where
    -- Abbreviate arbitrarily long strings in trace messages
    abbr :: String -> String
    abbr s = case splitAt 60 s of
      (_, []) -> s
      (s', _) -> s' ++ " ..."

-- | Parse a single line of output
parseSingleLine :: String -> Maybe String
parseSingleLine s = case lines s of
    [s'] -> Just s'
    _    -> Nothing

-- | Parse the first line of output
parseFirstLine :: String -> Maybe String
parseFirstLine = listToMaybe . lines

-- | Parse non-empty output
parseNonEmpty :: String -> Maybe String
parseNonEmpty = \case
    "" -> Nothing
    s  -> Just s
