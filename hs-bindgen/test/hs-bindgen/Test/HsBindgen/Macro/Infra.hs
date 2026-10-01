-- | Test infrastructure for the macro parsers
--
-- Builds @libclang@ token streams without running @libclang@: a macro
-- definition is written as a list of 'Piece's, which 'layout' turns into tokens
-- with the extents they would have in a source file.
--
-- The extents matter: 'HsBindgen.Macro.Syntax.splitMacro' tells a function-like
-- macro from an object-like one by looking for white space before the @(@, so
-- 'spc' is part of the input, not decoration.
module Test.HsBindgen.Macro.Infra (
    -- * Pieces of a macro definition
    Piece
  , comment
  , ident
  , kw
  , lit
  , punc
  , spc
    -- * Laying out tokens
  , layout
  , extentOf
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as Text

import Clang.Enum.Simple (simpleEnum)
import Clang.HighLevel.Types (MultiLoc (..), Range (..), SingleLoc (..),
                              SourcePath, Token (..), TokenSpelling (..))
import Clang.LowLevel.Core (CXCursorKind (CXCursor_UnexposedDecl),
                            CXTokenKind (CXToken_Comment, CXToken_Identifier, CXToken_Keyword, CXToken_Literal, CXToken_Punctuation))

{-------------------------------------------------------------------------------
  Pieces of a macro definition
-------------------------------------------------------------------------------}

-- | A token of a macro definition, or the white space between two tokens
data Piece =
    PieceToken CXTokenKind Text
  | PieceSpace

-- | A comment
--
-- @libclang@ reports comments as tokens, so a comment between two tokens is not
-- white space: it makes them non-adjacent /and/ it ends up in the body.
comment :: Text -> Piece
comment = PieceToken CXToken_Comment

-- | An identifier
ident :: Text -> Piece
ident = PieceToken CXToken_Identifier

-- | A C keyword
--
-- Which spellings @libclang@ reports as keywords rather than identifiers
-- depends on the LLVM version and the C standard (@bool@ is the notorious one);
-- here the classification is an input, so tests can pin either case.
kw :: Text -> Piece
kw = PieceToken CXToken_Keyword

-- | A literal
lit :: Text -> Piece
lit = PieceToken CXToken_Literal

-- | Punctuation
--
-- The spelling may contain a line continuation (@\\@ followed by a newline),
-- which is how @libclang@ sometimes reports punctuation that a spliced line put
-- next to the preceding token.
punc :: Text -> Piece
punc = PieceToken CXToken_Punctuation

-- | A single space
spc :: Piece
spc = PieceSpace

{-------------------------------------------------------------------------------
  Laying out tokens
-------------------------------------------------------------------------------}

-- | Lay the pieces out as source text, starting at line 1, column 1
--
-- Each token gets the extent it would have there, ending one past its last
-- character, as @libclang@ reports it.
layout :: [Piece] -> [Token SourcePath TokenSpelling]
layout = go SourceLoc{line = 1, column = 1, offset = 0}
  where
    go :: SourceLoc -> [Piece] -> [Token SourcePath TokenSpelling]
    go _   []                           = []
    go loc (PieceSpace        : pieces) = go (advance loc " ") pieces
    go loc (PieceToken kind s : pieces) = mkToken kind s loc end : go end pieces
      where
        end = advance loc s

-- | The extent spanned by laid-out tokens
--
-- 'HsBindgen.Macro.Syntax.MacroDefinition' and
-- 'HsBindgen.Macro.Syntax.MacroInvocation' carry the range of the whole
-- construct. No parser reads it, so any faithful range will do; an empty token
-- list gets the start of the synthetic source.
extentOf :: [Token SourcePath TokenSpelling] -> Range (MultiLoc SourcePath)
extentOf = \case
    []     -> Range start start
    t : ts -> Range
                (rangeStart (tokenExtent t))
                (rangeEnd   (tokenExtent (NE.last (t :| ts))))
  where
    start :: MultiLoc SourcePath
    start = multiLoc SourceLoc{line = 1, column = 1, offset = 0}

-- | A location in the synthetic source text
data SourceLoc = SourceLoc {
      line   :: Int
    , column :: Int
    , offset :: Int
    }

-- | The location just past the given text, which may span lines
advance :: SourceLoc -> Text -> SourceLoc
advance loc text = SourceLoc {
      line   = loc.line + Text.count "\n" text
    , column = case Text.breakOnEnd "\n" text of
                 ("", _)        -> loc.column + Text.length text
                 (_, lastLine)  -> 1 + Text.length lastLine
    , offset = loc.offset + Text.length text
    }

mkToken :: CXTokenKind -> Text -> SourceLoc -> SourceLoc -> Token SourcePath TokenSpelling
mkToken kind spelling start end = Token {
      tokenKind       = simpleEnum kind
    , tokenSpelling   = TokenSpelling spelling
    , tokenExtent     = Range (multiLoc start) (multiLoc end)
    , tokenCursorKind = simpleEnum CXCursor_UnexposedDecl
    }

multiLoc :: SourceLoc -> (MultiLoc SourcePath)
multiLoc loc = MultiLoc {
      multiLocExpansion = SingleLoc {
          singleLocPath   = "<test>"
        , singleLocLine   = loc.line
        , singleLocColumn = loc.column
        , singleLocOffset = loc.offset
        }
    , multiLocPresumed  = Nothing
    , multiLocSpelling  = Nothing
    , multiLocFile      = Nothing
    }
