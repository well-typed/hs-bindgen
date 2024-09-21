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
  , MacroName
  , Arg
  , Expr(..)
  , SimpleExpr(..)
  , Attribute(..)
    -- * Classification
  , isIncludeGuard
    -- * Parsing
  , parse
    -- ** Unrecognized macros
  , UnrecognizedMacro(..)
  , Token(..)
  , TokenSpelling(..)
  ) where

import Control.Exception (Exception)
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
import Text.Parsec.Prim (Parsec, (<?>), many)
import Text.Parsec.Prim qualified as Parsec
import Text.Read (readMaybe)
import Text.Show.Pretty (PrettyVal)

import HsBindgen.Clang.Core
import HsBindgen.Clang.Util.SourceLoc
import HsBindgen.Clang.Util.Tokens
import HsBindgen.Patterns
import HsBindgen.Util.Tracer
import Data.List (intercalate)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Macro = Macro {
      macroLoc  :: SourceLoc
    , macroName :: MacroName
    , macroArgs :: [Arg]
    , macroBody :: Expr
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

-- | Body of a function-like macro
data Expr =
    ESimple SimpleExpr
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

data SimpleExpr =
    -- | Empty
    SEmpty

    -- | Integer literal
  | SInt Integer

    -- | Variable
    --
    -- This might be a macro argument, or another marco.
  | SVar Var [Expr]

    -- | Attribute
  | SAttr Attribute SimpleExpr

    -- | Stringizing
    --
    -- See
    --
    -- * Section 6.10.3.2, "The # operator" of the spec
    -- * <https://gcc.gnu.org/onlinedocs/cpp/Stringizing.html>
  | SStringize Name

    -- | Concatenation
    --
    -- See
    --
    -- * Section 6.10.3.3, "The ## operator" of the spec
    -- * <https://gcc.gnu.org/onlinedocs/cpp/Concatenation.html>
  | SConcat SimpleExpr SimpleExpr
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

-- | Attribute
--
-- See Section 5.25, "Attribute syntax" of the gcc manual.
-- <https://gcc.gnu.org/onlinedocs/gcc-4.1.2/gcc/Attribute-Syntax.html#Attribute-Syntax>.
--
-- For now we make no attempt to parse what's actually inside the attribute.
data Attribute = Attribute [Token TokenSpelling]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

type MacroName = Name
type Arg       = Name
type Var       = Name
type Name      = Text

{-------------------------------------------------------------------------------
  Classification
-------------------------------------------------------------------------------}

isIncludeGuard :: Macro -> Bool
isIncludeGuard Macro{macroLoc, macroName, macroArgs, macroBody} =
    and [
        Text.unpack macroName `elem` includeGuards
      , null macroArgs
      , case macroBody of
          ESimple SEmpty   -> True
          ESimple (SInt 1) -> True
          _otherwise       -> False
      ]
  where
    sourcePath :: FilePath
    sourcePath = Text.unpack . getSourcePath $ sourceLocFile macroLoc

    includeGuards :: [String]
    includeGuards = possibleIncludeGuards (takeBaseName sourcePath)

    -- | Possible names for include guards, given the file (base) name
    possibleIncludeGuards :: String -> [String]
    possibleIncludeGuards baseName = [
                 map toUpper baseName ++ "_H"
        , "_" ++ map toUpper baseName ++ "_H" -- this would be a reserved name
        ,        map toUpper baseName ++ "_INCLUDED"
        ]

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
    macroBody <- parseExpr
    return Macro{macroLoc, macroName, macroArgs, macroBody}

parseMacroName :: Parser (SourceLoc, MacroName)
parseMacroName = parseName

parseName :: Parser (SourceLoc, MacroName)
parseName = token $ \t -> do
    guard $ fromSimpleEnum (tokenKind t) == Right CXToken_Identifier
    return (
        sourceRangeStart (tokenExtent t)
      , getTokenSpelling (tokenSpelling t)
      )

{-------------------------------------------------------------------------------
  Simple expressions
-------------------------------------------------------------------------------}

parseSimpleExpr :: Parser SimpleExpr
parseSimpleExpr =
    buildExpressionParser ops term <?> "simple expression"
  where
    term :: Parser SimpleExpr
    term = choice [
        SEmpty <$ eof
      , SInt <$> parseInteger
      , SVar <$> parseVar <*> option [] parseActualArgs
      , SAttr <$> parseAttribute <*> parseSimpleExpr
      , SStringize <$ punctuation "#" <*> parseVar
      ]

    ops :: OperatorTable [Token TokenSpelling] ParserState Identity SimpleExpr
    ops = [[Infix (SConcat <$ punctuation "##") AssocLeft]]

parseVar :: Parser Var
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

parseFormalArgs :: Parser [Arg]
parseFormalArgs = parens $ parseFormalArg `sepBy` comma

parseFormalArg :: Parser Arg
parseFormalArg = snd <$> parseName

parseActualArgs :: Parser [Expr]
parseActualArgs = parens $ parseExpr `sepBy` comma

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
        , ESimple <$> parseSimpleExpr
        ]

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

