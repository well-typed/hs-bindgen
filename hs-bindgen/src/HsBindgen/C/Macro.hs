{-# LANGUAGE OverloadedStrings #-}

-- | C macros of a certain shape
--
-- Intended for qualified import.
--
-- > import HsBindgen.C.Macro (Macro, UnrecognizedMacro)
-- > import HsBindgen.C.Macro qualified as Macro
module HsBindgen.C.Macro (
    -- * Definition
    Macro(..)
  , Expansion(..)
  , MacroName
  , Arg
  , Expr(..)
  , Atom(..)
    -- * Unrecognized macros
  , UnrecognizedMacro(..)
  , Token(..)
  , TokenSpelling(..)
    -- * Parse
  , parse
  ) where

import Control.Exception
import Control.Monad
import Data.Bifunctor
import Data.Char (toUpper)
import Data.Functor.Identity
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import System.FilePath (takeBaseName)
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Expr
import Text.Parsec.Pos qualified as Parsec
import Text.Parsec.Prim (Parsec, (<?>))
import Text.Parsec.Prim qualified as Parsec
import Text.Read (readMaybe)
import Text.Show.Pretty (PrettyVal)

import HsBindgen.Clang.Core
import HsBindgen.Clang.Util.SourceLoc
import HsBindgen.Clang.Util.Tokens
import HsBindgen.Patterns

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Macro =
    IncludeGuard MacroName
  | ObjectLike MacroName Expansion
  | FunctionLike MacroName [Arg] Expr
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

data Expansion =
    Empty
  | Integer Integer
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

data Expr =
    EAtom Atom
  | EUnaryPlus Expr        -- ^ @+@
  | EUnaryMinus Expr       -- ^ @-@
  | ELogicalNot Expr       -- ^ @!@
  | EBitwiseNot Expr       -- ^ @~@
  | EMult Expr Expr        -- ^ @*@
  | EDiv Expr Expr         -- ^ @/@
  | ERem Expr Expr         -- ^ @%@
  | EAdd Expr Expr         -- ^ @+@
  | ESub Expr Expr         -- ^ @-@
  | EShiftLeft Expr Expr   -- ^ @<<@
  | EShiftRight Expr Expr  -- ^ @>>@
  | ERelLT Expr Expr       -- ^ @<@
  | ERelLE Expr Expr       -- ^ @<=@
  | ERelGT Expr Expr       -- ^ @>@
  | ERelGE Expr Expr       -- ^ @>=@
  | ERelEQ Expr Expr       -- ^ @==@
  | ERelNE Expr Expr       -- ^ @!=@
  | EBitwiseAnd Expr Expr  -- ^ @&@
  | EBitwiseXor Expr Expr  -- ^ @^@
  | EBitwiseOr Expr Expr   -- ^ @|@
  | ELogicalAnd Expr Expr  -- ^ @&&@
  | ELogicalOr Expr Expr   -- ^ @||@
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

data Atom =
    -- | Integer literal
    AInt Integer

    -- | Variable
    --
    -- This might be a macro argument, or another marco.
  | AVar Name

    -- | Stringizing
    --
    -- See
    --
    -- * Section 6.10.3.2, "The # operator" of the spec
    -- * <https://gcc.gnu.org/onlinedocs/cpp/Stringizing.html>
  | AStringize Name

    -- | Concatenation
    --
    -- See
    --
    -- * Section 6.10.3.3, "The ## operator" of the spec
    -- * <https://gcc.gnu.org/onlinedocs/cpp/Concatenation.html>
  | AConcat Atom Atom
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

type MacroName = Text
type Arg       = Text
type Name      = Text

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

data UnrecognizedMacro = UnrecognizedMacro {
      unrecognizedMacroTokens :: [Token TokenSpelling]
    , unrecognizedMacroError  :: String
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal, Exception)

parse :: [Token TokenSpelling] -> Either UnrecognizedMacro Macro
parse [] = error "HsBindgen.C.Macro.parse: impossible empty macro"
parse tokens@(macroName:_) =
    first unrecognized $
      Parsec.runParser
        (parseMacro sourcePath)
        initParserState
        sourcePath
        tokens
  where
    sourcePath :: FilePath
    sourcePath =
        Text.unpack . getSourcePath . sourceLocFile $
          sourceRangeStart (tokenExtent macroName)

    unrecognized :: ParseError -> UnrecognizedMacro
    unrecognized err = UnrecognizedMacro{
          unrecognizedMacroTokens = tokens
        , unrecognizedMacroError  = show err
        }

{-------------------------------------------------------------------------------
  Parser proper

  Some useful references:

  - Section 6.10 "Preprocessing directives" of the C standard
    <https://fog.misty.com/perry/osp/standard/preproc.pdf>
  - Section 3 "Macros" of the @cpp@ documtation
    <https://gcc.gnu.org/onlinedocs/cpp/Macros.html>
  - "C operator precedence"
    <https://en.cppreference.com/w/c/language/operator_precedence>
-------------------------------------------------------------------------------}

type Parser = Parsec [Token TokenSpelling] ParserState

parseMacro :: FilePath -> Parser Macro
parseMacro sourcePath = do
    result <- parseMacro'
    case result of
      ObjectLike name Empty | Text.unpack name `elem` includeGuards ->
        return $ IncludeGuard name
      ObjectLike name (Integer 1) | Text.unpack name `elem` includeGuards ->
        return $ IncludeGuard name
      _otherwise ->
        return result
  where
    includeGuards :: [String]
    includeGuards = possibleIncludeGuards (takeBaseName sourcePath)

-- | Like 'macro', but without the special case for include guards
parseMacro' :: Parser Macro
parseMacro' = do
    name <- parseMacroName
    choice [
        ObjectLike name <$> parseExpansion
      , FunctionLike name <$> parseArgs <*> parseExpr
      ] <* eof

parseMacroName :: Parser MacroName
parseMacroName = tokenOfKind CXToken_Identifier Just

-- | Possible names for include guards, given the file (base) name
possibleIncludeGuards :: String -> [String]
possibleIncludeGuards baseName = [
             map toUpper baseName ++ "_H"
    , "_" ++ map toUpper baseName ++ "_H" -- this would be a reserved name
    ,        map toUpper baseName ++ "_INCLUDED"
    ]

{-------------------------------------------------------------------------------
  Object-like macros
-------------------------------------------------------------------------------}

parseExpansion :: Parser Expansion
parseExpansion = choice [
      Empty <$ eof
    , Integer <$> parseInteger
    ]

-- | TODO: This is wrong, we should not parse C int literals with Haskell rules
parseInteger :: Parser Integer
parseInteger = tokenOfKind CXToken_Literal (readMaybe . Text.unpack)

{-------------------------------------------------------------------------------
  Function-like macros
-------------------------------------------------------------------------------}

parseArgs :: Parser [Arg]
parseArgs = parens $ parseArg `sepBy` comma

parseArg :: Parser Arg
parseArg = tokenOfKind CXToken_Identifier Just

{-------------------------------------------------------------------------------
  Expressions

  This is currently only a subset of the operators described in
  <https://en.cppreference.com/w/c/language/operator_precedence>, but we do
  follow the same structure.
-------------------------------------------------------------------------------}

parseExpr :: Parser Expr
parseExpr =
    buildExpressionParser ops term <?> "expression"
  where
    term :: Parser Expr
    term = choice [
          parens parseExpr
        , EAtom <$> parseAtom
        ] <?> "simple expression"

    -- 'OperatorTable' expects the list in descending precedence
    ops :: OperatorTable [Token TokenSpelling] ParserState Identity Expr
    ops = [
        -- Precedence 1 (all left-to-right)
        []

        -- Precedence 2 (all right-to-left)
      , [ Prefix (EUnaryPlus  <$ punctuation "+")
        , Prefix (EUnaryMinus <$ punctuation "-")
        , Prefix (ELogicalNot <$ punctuation "!")
        , Prefix (EBitwiseNot <$ punctuation "~")
        ]

        -- Precedence 3 (precedence 3 .. 12 are all left-to-right)
      , [ Infix (EMult <$ punctuation "*") AssocLeft
        , Infix (EDiv  <$ punctuation "/") AssocLeft
        , Infix (ERem  <$ punctuation "%") AssocLeft
        ]

        -- Precedence 4
      , [ Infix (EAdd <$ punctuation "+") AssocLeft
        , Infix (ESub <$ punctuation "-") AssocLeft
        ]

        -- Precedence 5
      , [ Infix (EShiftLeft  <$ punctuation "<<") AssocLeft
        , Infix (EShiftRight <$ punctuation ">>") AssocLeft
        ]

        -- Precedence 6
      , [ Infix (ERelLT <$ punctuation "<")  AssocLeft
        , Infix (ERelLE <$ punctuation "<=") AssocLeft
        , Infix (ERelGT <$ punctuation ">")  AssocLeft
        , Infix (ERelGE <$ punctuation ">=") AssocLeft
        ]

        -- Precedence 7
      , [ Infix (ERelEQ <$ punctuation "==") AssocLeft
        , Infix (ERelNE <$ punctuation "!=") AssocLeft
        ]

        -- Precedence 8 .. 12
      , [ Infix (EBitwiseAnd <$ punctuation "&")  AssocLeft ]
      , [ Infix (EBitwiseXor <$ punctuation "^")  AssocLeft ]
      , [ Infix (EBitwiseOr  <$ punctuation "|")  AssocLeft ]
      , [ Infix (ELogicalAnd <$ punctuation "&&") AssocLeft ]
      , [ Infix (ELogicalOr  <$ punctuation "||") AssocLeft ]
      ]

parseAtom :: Parser Atom
parseAtom =
    buildExpressionParser ops term <?> "atomic expression"
  where
    term :: Parser Atom
    term = choice [
        AInt <$> parseInteger
      , AVar <$> tokenOfKind CXToken_Identifier Just
      ] <?> "simple atomic expression"

    ops :: OperatorTable [Token TokenSpelling] ParserState Identity Atom
    ops = [
        -- [Prefix (AStringize <$ punctuation "#")]
        [Infix (AConcat <$ punctuation "##") AssocLeft]
      ]

{-------------------------------------------------------------------------------
  Parser state
-------------------------------------------------------------------------------}

data ParserState = ParserState

initParserState :: ParserState
initParserState = ParserState

{-------------------------------------------------------------------------------
  Simple tokens
-------------------------------------------------------------------------------}

parens :: Parser a -> Parser a
parens p = punctuation "(" *> p <* punctuation ")"

comma :: Parser ()
comma = punctuation ","

{-------------------------------------------------------------------------------
  Low-level interface for tokens
-------------------------------------------------------------------------------}

punctuation :: Text -> Parser ()
punctuation = exact CXToken_Punctuation

exact :: CXTokenKind -> Text -> Parser ()
exact kind expected = tokenOfKind kind (\actual -> guard $ expected == actual)

tokenOfKind :: CXTokenKind -> (Text -> Maybe a) -> Parser a
tokenOfKind kind f = token $ \t ->
    if fromSimpleEnum (tokenKind t) == Right kind
      then f $ getTokenSpelling (tokenSpelling t)
      else Nothing

token :: (Token TokenSpelling -> Maybe a) -> Parser a
token = Parsec.token tokenPretty tokenSourcePos

tokenPretty :: Token TokenSpelling -> String
tokenPretty Token{tokenKind, tokenSpelling} = concat [
      show $ Text.unpack (getTokenSpelling tokenSpelling)
    , " ("
    , show tokenKind
    ,  ")"
    ]

tokenSourcePos :: Token a -> Parsec.SourcePos
tokenSourcePos t =
    Parsec.newPos
      (Text.unpack $ getSourcePath sourceLocFile)
      sourceLocLine
      sourceLocColumn
  where
    SourceLoc{
        sourceLocFile
      , sourceLocLine
      , sourceLocColumn
      } = sourceRangeStart (tokenExtent t)

