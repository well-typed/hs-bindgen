-- | Helpers for macro text replacement on @libclang@ tokens
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.CPP.Clang qualified as CPP.Clang
module HsBindgen.CPP.Clang (
    PreprocessorContext
  , withPreprocessorContext
  , addMacroDefinition
  , preprocess
    -- * Pretty-print tokens
  , prettyTokens
  , prettyToken
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Text qualified as Text
import System.Exit
import System.FilePath
import System.IO
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

preprocess :: PreprocessorContext -> String -> IO (Either PreprocessorError String)
preprocess ctx source = do
    close ctx.definitions
    setReparseHeader ctx source
    (ex, out, err) <- runClangExecutable ["-P", "-E", ctx.reparse.path] ""
    case ex of
      ExitSuccess -> pure $ Right out
      ExitFailure n -> pure $ Left (PreprocessorError n err)

runClangExecutable :: [String] -> String -> IO (ExitCode, String, String)
runClangExecutable = readProcessWithExitCode clangExecutable

clangExecutable :: FilePath
clangExecutable = binDir </> "clang"

{-------------------------------------------------------------------------------
  Preprocessor context
-------------------------------------------------------------------------------}

data PreprocessorContext = PreprocessorContext {
      tempDirectory :: FilePath
    , definitions   :: MacroDefinitionsHeader
    , reparse       :: ReparseHeader
    }

withPreprocessorContext :: (PreprocessorContext -> IO a) -> IO a
withPreprocessorContext k =
    withSystemTempDirectory @IO "hs_bindgen_clang_preprocessor" $ \dir -> do
      definitions <- openMacroDefinitionsHeader dir
      reparse <- openReparseHeader dir
      x <- k PreprocessorContext {
          tempDirectory = dir
        , definitions = definitions
        , reparse = reparse
        }

      pure x

addMacroDefinition ::
     UnparsedMacro
  -> PreprocessorContext
  -> IO ()
addMacroDefinition mac ctx = do
    reopen ctx.definitions
    withMVar ctx.definitions.handleVar $ \h ->
      hPutStrLn h $ showsDefinition ""
  where
    showsDefinition =
      showString "#define" . showSpace .  prettyTokens mac.tokens

setReparseHeader :: PreprocessorContext -> String -> IO ()
setReparseHeader ctx contents = do
    writeFile ctx.reparse.path reparseContents
  where
    reparseContents =
        showString "#include"
      . showSpace
      . showChar '\"'
      . showString ctx.definitions.name
      . showChar '\"'
      . showChar '\n'
      $ contents

{-------------------------------------------------------------------------------
  Macro definitions header
-------------------------------------------------------------------------------}

data MacroDefinitionsHeader = MacroDefinitionsHeader {
      name      :: FilePath
    , path      :: FilePath
    , mode      :: IOMode
    , handleVar :: MVar Handle
    }

openMacroDefinitionsHeader :: FilePath -> IO MacroDefinitionsHeader
openMacroDefinitionsHeader tempDir =
    bracketOnError (openFile path mode) hClose $ \h -> do
      handleVar <- newMVar h
      pure MacroDefinitionsHeader {
          name = name
        , path = path
        , mode = mode
        , handleVar = handleVar
        }
  where
    name = "macro_definitions" <.> "h"
    path = tempDir </> name
    mode = AppendMode

close :: MacroDefinitionsHeader -> IO ()
close header = mask_ $ do
    h <- tryTakeMVar header.handleVar
    forM_ h $ hClose

reopen :: MacroDefinitionsHeader -> IO ()
reopen header = mask_ $ do
    b <- isEmptyMVar header.handleVar
    when b $ do
      mask_ $ do
        h <- openFile header.path header.mode
        putMVar header.handleVar h

{-------------------------------------------------------------------------------
  Reparse header
-------------------------------------------------------------------------------}

data ReparseHeader = ReparseHeader {
      name :: FilePath
    , path :: FilePath
    }

openReparseHeader :: FilePath -> IO ReparseHeader
openReparseHeader tempDir =
    pure ReparseHeader {
        name = name
      , path = path
      }
  where
    name = "reparse" <.> "h"
    path = tempDir </> name

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
