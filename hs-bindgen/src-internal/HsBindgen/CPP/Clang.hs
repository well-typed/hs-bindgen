-- | Helpers for macro text replacement on @libclang@ tokens
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.CPP.Clang qualified as CPP.Clang
module HsBindgen.CPP.Clang (
    preprocess
  , preprocessWith
    -- * Macro definitions
  , MacroDefinitions
  , emptyMacroDefinitions
  , addMacroDefinition
    -- * Pretty-print tokens
  , prettyTokens
  , prettyToken
  ) where

import Data.Text qualified as Text
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Process

import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.Frontend.Pass.Parse.IsPass

{-------------------------------------------------------------------------------
  Preprocessor: macro expansion
-------------------------------------------------------------------------------}

data PreprocessorError = PreprocessorError Int String
  deriving stock Show

preprocess :: String -> IO (Either PreprocessorError String)
preprocess source = runPreprocess source

preprocessWith :: MacroDefinitions -> String -> IO (Either PreprocessorError String)
preprocessWith defs source = runPreprocess source'
  where
    source' = defs.unwrap source

runPreprocess :: String -> IO (Either PreprocessorError String)
runPreprocess source =
    withSystemTempDirectory @IO "clang_preprocess" $ \dir -> do
      let path = dir </> "temp.h"
      writeFile path source
      (ex, out, err) <- runClangExecutable ["-E", "-P", path] ""
      case ex of
        ExitSuccess -> pure $ Right out
        ExitFailure n -> pure $ Left (PreprocessorError n err)

runClangExecutable :: [String] -> String -> IO (ExitCode, String, String)
runClangExecutable = readProcessWithExitCode clangExecutable

clangExecutable :: FilePath
clangExecutable = binDir </> "clang"

{-------------------------------------------------------------------------------
  Macro definitions
-------------------------------------------------------------------------------}

data MacroDefinitions = MacroDefinitions { unwrap :: ShowS }

emptyMacroDefinitions :: MacroDefinitions
emptyMacroDefinitions = MacroDefinitions id

addMacroDefinition ::
     UnparsedMacro
  -> MacroDefinitions
  -> MacroDefinitions
addMacroDefinition mac defs =
    MacroDefinitions { unwrap = defs.unwrap . showsDefinition . showNewline }
  where
    showsDefinition =
      showString "#define" . showSpace .  prettyTokens mac.tokens

{-------------------------------------------------------------------------------
  Pretty-print tokens
-------------------------------------------------------------------------------}

-- TODO: warn if tokens are out of order?
prettyTokens :: [Token TokenSpelling] -> ShowS
prettyTokens = \case
    [] -> id
    ts@(t:_) -> go (start t) ts
  where
    go :: SingleLoc -> [Token TokenSpelling] -> ShowS
    go _loc [] = id
    go loc (t:ts) =
          whitespace loc (start t)
        . showSpelling t
        . go (end t) ts

    whitespace :: SingleLoc -> SingleLoc -> ShowS
    whitespace loc1 loc2
      | loc1.singleLocLine == loc2.singleLocLine
      = spaces loc1 loc2
      | otherwise
      = showSpaces 1

    spaces :: SingleLoc -> SingleLoc -> ShowS
    spaces loc1 loc2 = showSpaces (loc2.singleLocColumn - loc1.singleLocColumn)

prettyToken :: Token TokenSpelling -> ShowS
prettyToken = showSpelling

{-------------------------------------------------------------------------------
  ShowS helpers
-------------------------------------------------------------------------------}

showSpelling :: Token TokenSpelling -> ShowS
showSpelling tok = showString $ Text.unpack tok.tokenSpelling.getTokenSpelling

showNewline :: ShowS
showNewline = showNewlines 1

showNewlines :: Int -> ShowS
showNewlines n = showString $ replicate n '\n'

showSpace :: ShowS
showSpace = showSpaces 1

showSpaces :: Int -> ShowS
showSpaces n = showString $ replicate n ' '

{-------------------------------------------------------------------------------
  Location helpers
-------------------------------------------------------------------------------}

start :: Token TokenSpelling -> SingleLoc
start tok = tok.tokenExtent.rangeStart.multiLocExpansion

end :: Token TokenSpelling -> SingleLoc
end tok = tok.tokenExtent.rangeEnd.multiLocExpansion
