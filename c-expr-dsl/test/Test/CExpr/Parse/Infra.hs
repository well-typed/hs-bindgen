-- | Test infrastructure for the c-expr-dsl parser tests
module Test.CExpr.Parse.Infra (
    -- * Token constructors
    kw
  , ident
  , punc
  , lit
    -- * Running parsers
  , checkType
  , checkMacro
  , parseTestWith
    -- * Results
  , tyLit
  ) where

import Data.Set qualified as Set
import Data.Text (Text)
import Text.Parsec (eof)

import Clang.CStandard
import Clang.Enum.Simple
import Clang.HighLevel.Types
import Clang.LowLevel.Core

import C.Expr.Parse
import C.Expr.Syntax

{-------------------------------------------------------------------------------
  Token constructors
-------------------------------------------------------------------------------}

-- | Construct a keyword token
kw :: Text -> Token TokenSpelling
kw = mkToken CXToken_Keyword

-- | Construct an identifier token
ident :: Text -> Token TokenSpelling
ident = mkToken CXToken_Identifier

-- | Construct a punctuation token
punc :: Text -> Token TokenSpelling
punc = mkToken CXToken_Punctuation

-- | Construct a literal token
lit :: Text -> Token TokenSpelling
lit = mkToken CXToken_Literal

mkToken :: CXTokenKind -> Text -> Token TokenSpelling
mkToken kind spelling = Token{
      tokenKind       = simpleEnum kind
    , tokenSpelling   = TokenSpelling spelling
    , tokenExtent     = Range nullLoc nullLoc
    , tokenCursorKind = simpleEnum CXCursor_UnexposedDecl
    }
  where
    nullLoc :: MultiLoc
    nullLoc = MultiLoc{
          multiLocExpansion = SingleLoc{
              singleLocPath   = "<test>"
            , singleLocLine   = 1
            , singleLocColumn = 1
            , singleLocOffset = 1
            }
        , multiLocPresumed  = Nothing
        , multiLocSpelling  = Nothing
        , multiLocFile      = Nothing
        }

{-------------------------------------------------------------------------------
  Running parsers
-------------------------------------------------------------------------------}

-- | Run the type parser on a sequence of tokens
--
-- Adds 'eof' so that trailing tokens are rejected as parse failures.
checkType ::
  ClangCStandard -> [Token TokenSpelling] -> Either MacroParseError (Expr Ps)
checkType cStd = runParser (parseMacroType cStd Set.empty <* eof)

-- | Run the macro parser on a complete token sequence
--
-- The first token must be the macro name (an identifier).  'parseMacro'
-- itself calls 'eof', so no trailing tokens are allowed.
checkMacro ::
  ClangCStandard -> [Token TokenSpelling] -> Either MacroParseError Macro
checkMacro cStd = runParser (parseMacro cStd)

-- | Run a parser on a list of (kind, spelling) pairs and print the result.
--
-- Useful for interactive debugging in GHCi:
--
-- > parseTestWith (parseMacro C17) [(CXToken_Identifier, "M"), (CXToken_Literal, "1")]
parseTestWith ::
     Show a
  => Parser a
  -> [(CXTokenKind, Text)]
  -> IO ()
parseTestWith p pairs = print $ runParser p (map (uncurry mkToken) pairs)

{-------------------------------------------------------------------------------
  Results
-------------------------------------------------------------------------------}

tyLit :: TypeLit -> Expr Ps
tyLit = Term . Literal . TypeLit
