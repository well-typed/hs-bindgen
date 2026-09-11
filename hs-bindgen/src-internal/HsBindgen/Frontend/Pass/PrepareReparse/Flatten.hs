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
import Clang.Paths (SourcePath)

import HsBindgen.Clang.Tokens qualified as Clang
import HsBindgen.Frontend.Pass.PrepareReparse.Printer.Util qualified as P

{-------------------------------------------------------------------------------
  Flattening
-------------------------------------------------------------------------------}

-- | Flatten tokens and add a semicolon at the end, while skipping over tokens
-- that are comments.
flattenDefault :: [Token SourcePath TokenSpelling] -> String
flattenDefault tokens = prettyTokens tokens ""

-- | Flatten tokens and add a semicolon at the end, while skipping over tokens
-- that are comments or part of a function body.
flattenFunction :: [Token SourcePath TokenSpelling] -> String
flattenFunction tokens = prettyTokens (skipFunctionBody tokens) ""

{-------------------------------------------------------------------------------
  Tokens
-------------------------------------------------------------------------------}

-- | Pretty-print tokens and add a semicolon at the end
prettyTokens :: [Token SourcePath TokenSpelling] -> ShowS
prettyTokens ts = Clang.prettyTokens (skipComments ts) . P.semicolon

-- | Skip tokens that are comments
skipComments :: [Token SourcePath TokenSpelling] -> [Token SourcePath TokenSpelling]
skipComments ts = filter p ts
  where
    p :: Token SourcePath TokenSpelling -> Bool
    p t = case fromSimpleEnum t.tokenKind of
        Right CXToken_Comment -> False
        _ -> True

-- | Skip tokens that form a function body
skipFunctionBody :: [Token SourcePath TokenSpelling] -> [Token SourcePath TokenSpelling]
skipFunctionBody = go
  where
    go [] = []
    go (t:ts) = case fromSimpleEnum t.tokenCursorKind of
        Right CXCursor_CompoundStmt -> []
        _ -> t : go ts
