{-# LANGUAGE OverloadedStrings #-}

module HsBindgen.C.Reparse.Macro (
    reparseMacro
  ) where

import Data.Text qualified as Text
import Text.Parsec
import Text.Parsec.Expr
import Text.Read (readMaybe)

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
        MEmpty     <$  eof
      , MInt       <$> literalInteger
      , MFloat     <$> literalFloat
      , MVar       <$> var <*> option [] actualArgs
      , MType      <$> reparsePrimType
      , MAttr      <$> reparseAttribute <*> mTerm
      , MStringize <$  punctuation "#" <*> var
      ]

    ops = [[Infix (MConcat <$ punctuation "##") AssocLeft]]

var :: Reparse CName
var = reparseName

-- | Parse integer literal
literalInteger :: Reparse (Literal Integer)
literalInteger = uncurry Literal <$>
    parseTokenOfKind CXToken_Literal reparseLiteralInteger

-- | Parse floating point literal
literalFloat :: Reparse Double
literalFloat = tokenOfKind CXToken_Literal (aux . Text.unpack)
  where
    aux :: String -> Maybe Double
    aux = readMaybe

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
