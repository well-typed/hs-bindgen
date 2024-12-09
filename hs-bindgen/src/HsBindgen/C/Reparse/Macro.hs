module HsBindgen.C.Reparse.Macro (
    reparseMacro
  ) where

import Data.Vec.Lazy
import Text.Parsec
import Text.Parsec.Expr

import HsBindgen.C.AST.Literal
import HsBindgen.C.AST.Macro
import HsBindgen.C.AST.Name
import HsBindgen.C.Reparse.Common
import HsBindgen.C.Reparse.Infra
import HsBindgen.C.Reparse.Literal
import HsBindgen.C.Reparse.Type
import HsBindgen.Clang.HighLevel.Types
import HsBindgen.Clang.LowLevel.Core


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

reparseMacro :: Reparse Macro
reparseMacro = do
    (macroLoc, macroName) <- reparseLocName
    choice [
        -- When we see an opening bracket it might be the start of an argument
        -- list, or it might be the start of the body, wrapped in parentheses.
        try $ functionLike macroLoc macroName
      , objectLike macroLoc macroName
      ]
  where
    functionLike, objectLike :: MultiLoc -> CName -> Reparse Macro
    functionLike loc name = Macro loc name <$> formalArgs <*> mExpr
    objectLike   loc name = Macro loc name [] <$> mExpr


formalArgs :: Reparse [CName]
formalArgs = parens $ formalArg `sepBy` comma

formalArg :: Reparse CName
formalArg = reparseName

{-------------------------------------------------------------------------------
  Simple expressions
-------------------------------------------------------------------------------}

mTerm :: Reparse MTerm
mTerm =
    buildExpressionParser ops term <?> "simple expression"
  where
    term :: Reparse MTerm
    term = choice [
        MEmpty       <$  eof
      , MInt         <$> literalInteger
      , MFloat       <$> literalFloat
      , MVar         <$> var <*> option [] actualArgs
      , MType        <$> reparsePrimType
      , MAttr        <$> reparseAttribute <*> mTerm
      , MStringize   <$  punctuation "#" <*> var
      ]

    ops = [[Infix (MConcat <$ punctuation "##") AssocLeft]]

var :: Reparse CName
var = reparseName

-- | Parse integer literal
literalInteger :: Reparse IntegerLiteral
literalInteger = do
  (txt, (val, ty)) <-
    parseTokenOfKind CXToken_Literal reparseLiteralInteger
  return $
    IntegerLiteral
      { integerLiteralText  = txt
      , integerLiteralType  = Just ty
      , integerLiteralValue = val }

-- | Parse floating point literal
literalFloat :: Reparse FloatingLiteral
literalFloat = do
  (txt, (fltVal, dblVal, ty)) <-
    parseTokenOfKind CXToken_Literal reparseLiteralFloating
  return $
    FloatingLiteral
      { floatingLiteralText = txt
      , floatingLiteralType = Just ty
      , floatingLiteralFloatValue = fltVal
      , floatingLiteralDoubleValue = dblVal }

actualArgs :: Reparse [MExpr]
actualArgs = parens $ mExpr `sepBy` comma

{-------------------------------------------------------------------------------
  Expressions

  This is currently only a subset of the operators described in
  <https://en.cppreference.com/w/c/language/operator_precedence>, but we do
  follow the same structure.
-------------------------------------------------------------------------------}

mExpr :: Reparse MExpr
mExpr =
    buildExpressionParser ops term <?> "expression"
  where
    term :: Reparse MExpr
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

    ap1 op arg = MApp op ( arg ::: VNil )
    ap2 op arg1 arg2 = MApp op ( arg1 ::: arg2 ::: VNil )
