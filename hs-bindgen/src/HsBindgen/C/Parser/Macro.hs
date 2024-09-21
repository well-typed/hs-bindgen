{-# LANGUAGE OverloadedStrings #-}

module HsBindgen.C.Parser.Macro (
    UnrecognizedMacro(..)
  , parse
  ) where

import Control.Exception (Exception)
import Control.Monad
import Data.Bifunctor
import Data.Functor.Identity
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Expr
import Text.Parsec.Pos qualified as Parsec
import Text.Parsec.Prim (Parsec, (<?>), many)
import Text.Parsec.Prim qualified as Parsec
import Text.Read (readMaybe)
import Text.Show.Pretty (PrettyVal)

import HsBindgen.C.AST.Macro
import HsBindgen.C.AST.Name
import HsBindgen.Clang.Core
import HsBindgen.Clang.Util.SourceLoc
import HsBindgen.Clang.Util.Tokens
import HsBindgen.Patterns
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

data UnrecognizedMacro = UnrecognizedMacro {
      unrecognizedMacroTokens :: [Token TokenSpelling]
    , unrecognizedMacroError  :: String
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal, Exception)

instance PrettyLogMsg UnrecognizedMacro where
  prettyLogMsg UnrecognizedMacro{
          unrecognizedMacroTokens
        , unrecognizedMacroError
        } = unlines [
        unrecognizedMacroError
      , "#define " ++ intercalate " " (
            map
              (Text.unpack . getTokenSpelling . tokenSpelling)
              unrecognizedMacroTokens
          )
      ]

parse :: [Token TokenSpelling] -> Either UnrecognizedMacro Macro
parse tokens =
    first unrecognized $
      Parsec.runParser parseMacro initParserState sourcePath tokens
  where
    sourcePath :: FilePath
    sourcePath =
        case tokens of
          []  -> error "parse: impossible" -- must have the macro name
          t:_ -> Text.unpack . getSourcePath . sourceLocFile $
                   sourceRangeStart (tokenExtent t)

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

parseMacro :: Parser Macro
parseMacro = do
    (macroLoc, macroName) <- parseMacroName
    macroArgs <- option [] parseFormalArgs
    macroBody <- parseMExpr
    return Macro{macroLoc, macroName, macroArgs, macroBody}

parseMacroName :: Parser (SourceLoc, CName)
parseMacroName = parseName

parseName :: Parser (SourceLoc, CName)
parseName = token $ \t -> do
    guard $ fromSimpleEnum (tokenKind t) == Right CXToken_Identifier
    return (
        sourceRangeStart (tokenExtent t)
      , CName $ getTokenSpelling (tokenSpelling t)
      )

{-------------------------------------------------------------------------------
  Simple expressions
-------------------------------------------------------------------------------}

parseMTerm :: Parser MTerm
parseMTerm =
    buildExpressionParser ops term <?> "simple expression"
  where
    term :: Parser MTerm
    term = choice [
        MEmpty <$ eof
      , MInt <$> parseInteger
      , MVar <$> parseVar <*> option [] parseActualArgs
      , MAttr <$> parseAttribute <*> parseMTerm
      , MStringize <$ punctuation "#" <*> parseVar
      ]

    ops :: OperatorTable [Token TokenSpelling] ParserState Identity MTerm
    ops = [[Infix (MConcat <$ punctuation "##") AssocLeft]]

parseVar :: Parser CName
parseVar = snd <$> parseName

parseInteger :: Parser Integer
parseInteger = tokenOfKind CXToken_Literal aux
  where
    -- TODO: This is wrong, we should not parse C literals with Haskell rules
    aux :: Text -> Maybe Integer
    aux = readMaybe . dropSuffix . Text.unpack

    -- TODO: Should we preserve this suffix in some way..? are we losing info?
    dropSuffix :: String -> String
    dropSuffix str = reverse $
       case reverse str of
         'l':'l':xs -> xs
         'L':'L':xs -> xs
         'u':xs     -> xs
         'U':xs     -> xs
         'l':xs     -> xs
         'L':xs     -> xs
         'z':xs     -> xs
         'Z':xs     -> xs
         xs         -> xs

{-------------------------------------------------------------------------------
  Attributes

  We don't really parse attributes, but just skip over them.
-------------------------------------------------------------------------------}

-- | Parse attribute
--
-- > __attribute__ (( .. ))
parseAttribute :: Parser Attribute
parseAttribute = fmap Attribute $ do
    exact CXToken_Keyword "__attribute__"
    doubleOpenParens *> anythingMatchingBrackets <* doubleCloseParens
  where
    doubleOpenParens, doubleCloseParens :: Parser ()
    doubleOpenParens  = (punctuation "(" >> punctuation "(") <?> "(("
    doubleCloseParens = (punctuation ")" >> punctuation ")") <?> "))"

-- | Any sequence of tokens, as long as the brackets inside are matched
anythingMatchingBrackets :: Parser [Token TokenSpelling]
anythingMatchingBrackets =
    concat <$> many go
  where
    go :: Parser [Token TokenSpelling]
    go = choice [
          do open   <- token isOpenParens
             inside <- concat <$> many go
             close  <- token isCloseParens
             return $ [open] ++ inside ++ [close]
        , (:[]) <$> token nonParens
        ]

    isOpenParens :: Token TokenSpelling -> Maybe (Token TokenSpelling)
    isOpenParens t = do
        guard $ fromSimpleEnum (tokenKind t) == Right CXToken_Punctuation
        guard $ getTokenSpelling (tokenSpelling t) == "("
        return t

    isCloseParens :: Token TokenSpelling -> Maybe (Token TokenSpelling)
    isCloseParens t = do
        guard $ fromSimpleEnum (tokenKind t) == Right CXToken_Punctuation
        guard $ getTokenSpelling (tokenSpelling t) == ")"
        return t

    nonParens :: Token TokenSpelling -> Maybe (Token TokenSpelling)
    nonParens t =
        case (isOpenParens t, isCloseParens t) of
          (Nothing, Nothing) -> Just t
          _otherwise         -> Nothing

{-------------------------------------------------------------------------------
  Function-like macros
-------------------------------------------------------------------------------}

parseFormalArgs :: Parser [CName]
parseFormalArgs = parens $ parseFormalArg `sepBy` comma

parseFormalArg :: Parser CName
parseFormalArg = snd <$> parseName

parseActualArgs :: Parser [MExpr]
parseActualArgs = parens $ parseMExpr `sepBy` comma

{-------------------------------------------------------------------------------
  Expressions

  This is currently only a subset of the operators described in
  <https://en.cppreference.com/w/c/language/operator_precedence>, but we do
  follow the same structure.
-------------------------------------------------------------------------------}

parseMExpr :: Parser MExpr
parseMExpr =
    buildExpressionParser ops term <?> "expression"
  where
    term :: Parser MExpr
    term = choice [
          parens parseMExpr
        , MTerm <$> parseMTerm
        ]

    -- 'OperatorTable' expects the list in descending precedence
    ops :: OperatorTable [Token TokenSpelling] ParserState Identity MExpr
    ops = [
        -- Precedence 1 (all left-to-right)
        []

        -- Precedence 2 (all right-to-left)
      , [ Prefix (MUnaryPlus  <$ punctuation "+")
        , Prefix (MUnaryMinus <$ punctuation "-")
        , Prefix (MLogicalNot <$ punctuation "!")
        , Prefix (MBitwiseNot <$ punctuation "~")
        ]

        -- Precedence 3 (precedence 3 .. 12 are all left-to-right)
      , [ Infix (MMult <$ punctuation "*") AssocLeft
        , Infix (MDiv  <$ punctuation "/") AssocLeft
        , Infix (MRem  <$ punctuation "%") AssocLeft
        ]

        -- Precedence 4
      , [ Infix (MAdd <$ punctuation "+") AssocLeft
        , Infix (MSub <$ punctuation "-") AssocLeft
        ]

        -- Precedence 5
      , [ Infix (MShiftLeft  <$ punctuation "<<") AssocLeft
        , Infix (MShiftRight <$ punctuation ">>") AssocLeft
        ]

        -- Precedence 6
      , [ Infix (MRelLT <$ punctuation "<")  AssocLeft
        , Infix (MRelLE <$ punctuation "<=") AssocLeft
        , Infix (MRelGT <$ punctuation ">")  AssocLeft
        , Infix (MRelGE <$ punctuation ">=") AssocLeft
        ]

        -- Precedence 7
      , [ Infix (MRelEQ <$ punctuation "==") AssocLeft
        , Infix (MRelNE <$ punctuation "!=") AssocLeft
        ]

        -- Precedence 8 .. 12
      , [ Infix (MBitwiseAnd <$ punctuation "&")  AssocLeft ]
      , [ Infix (MBitwiseXor <$ punctuation "^")  AssocLeft ]
      , [ Infix (MBitwiseOr  <$ punctuation "|")  AssocLeft ]
      , [ Infix (MLogicalAnd <$ punctuation "&&") AssocLeft ]
      , [ Infix (MLogicalOr  <$ punctuation "||") AssocLeft ]
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

