{-# LANGUAGE OverloadedStrings #-}

module C.Expr.Parse.Expr (parseExpr) where

import Control.Monad
import Data.Functor.Identity
import Data.Type.Nat
import Data.Vec.Lazy
import Text.Parsec
import Text.Parsec.Expr

import Clang.HighLevel.Types
import Clang.LowLevel.Core

import C.Expr.Parse.Infra
import C.Expr.Parse.Literal
import C.Expr.Parse.Name
import C.Expr.Syntax

{-------------------------------------------------------------------------------
  Top-level

  Some useful references:

  - Section 6.10 "Preprocessing directives" of the C standard
    <https://fog.misty.com/perry/osp/standard/preproc.pdf>
  - Section 3 "Macros" of the @cpp@ documtation
    <https://gcc.gnu.org/onlinedocs/cpp/Macros.html>
  - "C operator precedence"
    <https://en.cppreference.com/w/c/language/operator_precedence>
-------------------------------------------------------------------------------}

parseExpr :: Parser (Macro Ps)
parseExpr = do
    (macroLoc, macroName) <- parseLocName
    (args, res) <-
      choice [
          -- When we see an opening bracket it might be the start of an argument
          -- list, or it might be the start of the body, wrapped in parentheses.
          try $ functionLike
        , objectLike
        ]
    eof
    return $ Macro macroLoc macroName args res
  where
    body :: Parser (MExpr Ps)
    body = mExprTuple

    functionLike, objectLike :: Parser ([Name], MExpr Ps)
    functionLike = (,) <$> formalArgs <*> body
    objectLike   = ([], ) <$> body

formalArgs :: Parser [Name]
formalArgs = parens $ formalArg `sepBy` comma

formalArg :: Parser Name
formalArg = parseName

{-------------------------------------------------------------------------------
  Simple expressions
-------------------------------------------------------------------------------}

mTerm :: Parser (MTerm Ps)
mTerm =
    buildExpressionParser ops term <?> "simple expression"
  where
    term :: Parser (MTerm Ps)
    term = choice [
        MInt        <$> literalInteger
      , MFloat      <$> literalFloat
      , MChar       <$> literalChar
      , MString     <$> literalString
      , MVar NoXVar <$> var <*> option [] actualArgs
      ]

    ops :: OperatorTable [Token TokenSpelling] ParserState Identity (MTerm Ps)
    ops = []

var :: Parser Name
var = parseName

-- | Parse integer literal
literalInteger :: Parser IntegerLiteral
literalInteger = do
  (txt, (val, ty)) <-
    parseTokenOfKind CXToken_Literal parseLiteralInteger
  return $
    IntegerLiteral
      { integerLiteralText  = txt
      , integerLiteralType  = ty
      , integerLiteralValue = val
      }

-- | Parse floating point literal
literalFloat :: Parser FloatingLiteral
literalFloat = do
  (txt, (fltVal, dblVal, ty)) <-
    parseTokenOfKind CXToken_Literal parseLiteralFloating
  return $
    FloatingLiteral
      { floatingLiteralText = txt
      , floatingLiteralType = ty
      , floatingLiteralFloatValue = fltVal
      , floatingLiteralDoubleValue = dblVal
      }

-- | Parse character literal
literalChar :: Parser CharLiteral
literalChar = do
  (txt, val) <-
    parseTokenOfKind CXToken_Literal parseLiteralChar
  return $
    CharLiteral
      { charLiteralText  = txt
      , charLiteralValue = val
      }

-- | Parse string literal
literalString :: Parser StringLiteral
literalString = do
  (txt, val) <-
    parseTokenOfKind CXToken_Literal parseLiteralString
  return $
    StringLiteral
      { stringLiteralText  = txt
      , stringLiteralValue = val
      }

actualArgs :: Parser [MExpr Ps]
actualArgs = parens $ mExpr `sepBy` comma

{-------------------------------------------------------------------------------
  Expressions

  This is currently only a subset of the operators described in
  <https://en.cppreference.com/w/c/language/operator_precedence>, but we do
  follow the same structure.
-------------------------------------------------------------------------------}

mExprTuple :: Parser (MExpr Ps)
mExprTuple = try tuple <|> mExpr
  where
    tuple = do
      openParen <- optionMaybe $ punctuation "("
      (e1, e2, es) <- mExpr `sepBy2` comma
      case openParen of
        Nothing -> return ()
        Just {} -> punctuation ")"
      return $
        reifyList es $ \es' ->
           MApp NoXApp MTuple ( e1 ::: e2 ::: es' )

mExpr :: Parser (MExpr Ps)
mExpr = buildExpressionParser ops term <?> "expression"
  where

    term :: Parser (MExpr Ps)
    term = choice [
          parens mExpr
        , MTerm <$> mTerm
        ]

    -- 'OperatorTable' expects the list in descending precedence
    ops = [
        -- Precedence 1 (all left-to-right)
        []

        -- Precedence 2 (all right-to-left)
      , [ Prefix (ap1 MUnaryPlus  <$ punctuation "+")
        , Prefix (ap1 MUnaryMinus <$ punctuation "-")
        , Prefix (ap1 MLogicalNot <$ punctuation "!")
        , Prefix (ap1 MBitwiseNot <$ punctuation "~")
        ]

        -- Precedence 3 (precedence 3 .. 12 are all left-to-right)
      , [ Infix (ap2 MMult <$ punctuation "*") AssocLeft
        , Infix (ap2 MDiv  <$ punctuation "/") AssocLeft
        , Infix (ap2 MRem  <$ punctuation "%") AssocLeft
        ]

        -- Precedence 4
      , [ Infix (ap2 MAdd <$ punctuation "+") AssocLeft
        , Infix (ap2 MSub <$ punctuation "-") AssocLeft
        ]

        -- Precedence 5
      , [ Infix (ap2 MShiftLeft  <$ punctuation "<<") AssocLeft
        , Infix (ap2 MShiftRight <$ punctuation ">>") AssocLeft
        ]

        -- Precedence 6
      , [ Infix (ap2 MRelLT <$ punctuation "<")  AssocLeft
        , Infix (ap2 MRelLE <$ punctuation "<=") AssocLeft
        , Infix (ap2 MRelGT <$ punctuation ">")  AssocLeft
        , Infix (ap2 MRelGE <$ punctuation ">=") AssocLeft
        ]

        -- Precedence 7
      , [ Infix (ap2 MRelEQ <$ punctuation "==") AssocLeft
        , Infix (ap2 MRelNE <$ punctuation "!=") AssocLeft
        ]

        -- Precedence 8 .. 12
      , [ Infix (ap2 MBitwiseAnd <$ punctuation "&")  AssocLeft ]
      , [ Infix (ap2 MBitwiseXor <$ punctuation "^")  AssocLeft ]
      , [ Infix (ap2 MBitwiseOr  <$ punctuation "|")  AssocLeft ]
      , [ Infix (ap2 MLogicalAnd <$ punctuation "&&") AssocLeft ]
      , [ Infix (ap2 MLogicalOr  <$ punctuation "||") AssocLeft ]
      ]

    ap1 :: MFun (S Z) -> MExpr Ps -> MExpr Ps
    ap1 op arg = MApp NoXApp op ( arg ::: VNil )

    ap2 :: MFun (S (S Z)) -> MExpr Ps -> MExpr Ps -> MExpr Ps
    ap2 op arg1 arg2 = MApp NoXApp op ( arg1 ::: arg2 ::: VNil )

sepBy2 :: ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m (a, a, [a])
{-# INLINEABLE sepBy2 #-}
sepBy2 p sep = do
  x1 <- p
  void sep
  x2 <- p
  xs <- many $ sep >> p
  return (x1, x2, xs)
