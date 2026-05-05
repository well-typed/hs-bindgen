-- | Flattening @libclang@ tokens
--
-- This module is intended to be imported unqualified. It is also intended to
-- only be imported from within the "HsBindgen.Frontend.Pass.PrepareReparse"
-- module hierarchy.
--
-- > import HsBindgen.Frontend.Pass.PrepareReparse.Flatten
--
module HsBindgen.Frontend.Pass.PrepareReparse.Flatten (
    flattenDefault
  , flattenFunction
  ) where

import Clang.Enum.Simple (fromSimpleEnum)
import Clang.HighLevel.Types (Token (tokenCursorKind, tokenKind), TokenSpelling)
import Clang.LowLevel.Core (CXCursorKind (CXCursor_CompoundStmt),
                            CXTokenKind (CXToken_Comment))

import HsBindgen.Clang.Tokens qualified as Clang
import HsBindgen.Frontend.Pass.PrepareReparse.Printer.Util qualified as Printer

{-------------------------------------------------------------------------------
  Flattening
-------------------------------------------------------------------------------}

-- | Flatten tokens and add a semicolon at the end, while skipping over tokens
-- that are comments.
flattenDefault :: [Token TokenSpelling] -> String
flattenDefault tokens = prettyTokens tokens ""

-- | Flatten tokens and add a semicolon at the end, while skipping over tokens
-- that are comments or part of a function body.
flattenFunction :: [Token TokenSpelling] -> String
flattenFunction tokens = prettyTokens (skipFunctionBody tokens) ""

{-------------------------------------------------------------------------------
  Tokens
-------------------------------------------------------------------------------}

-- | Pretty-print tokens and add a semicolon at the end
prettyTokens :: [Token TokenSpelling] -> ShowS
prettyTokens ts = Clang.prettyTokens (skipComments ts) . Printer.semicolon

-- | Skip tokens that are comments
skipComments :: [Token TokenSpelling] -> [Token TokenSpelling]
skipComments ts = filter p ts
  where
    p :: Token TokenSpelling -> Bool
    p t = case fromSimpleEnum t.tokenKind of
        Right CXToken_Comment -> False
        _ -> True

-- | Skip tokens that form a function body
skipFunctionBody :: [Token TokenSpelling] -> [Token TokenSpelling]
skipFunctionBody = go
  where
    go [] = []
    go (t:ts) = case fromSimpleEnum t.tokenCursorKind of
        Right CXCursor_CompoundStmt -> []
        _ -> t : go ts
