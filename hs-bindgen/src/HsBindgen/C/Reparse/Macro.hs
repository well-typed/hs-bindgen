module HsBindgen.C.Reparse.Macro (
    reparseMacro, mExpr
  ) where

import Control.Monad
import Data.Type.Nat
import Data.Vec.Lazy
import Text.Parsec
import Text.Parsec.Expr
import Data.Text qualified as Text

import HsBindgen.Imports
import HsBindgen.C.AST.Literal
import HsBindgen.C.AST.Macro
import HsBindgen.C.AST.Name
import HsBindgen.C.AST.Type
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
    macro <-
      choice [
          -- When we see an opening bracket it might be the start of an argument
          -- list, or it might be the start of the body, wrapped in parentheses.
          try $ functionLike macroLoc macroName
        , objectLike macroLoc macroName
        ]
    eof
    return macro
  where
    functionLike, objectLike :: MultiLoc -> CName -> Reparse Macro
    functionLike loc name = Macro loc name <$> formalArgs <*> mExprTuple
    objectLike   loc name = Macro loc name [] <$> mExprTuple


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
        MInt         <$> literalInteger
      , MFloat       <$> literalFloat
      , MChar        <$> literalChar
      , MString      <$> literalString
      , MVar         <$> var <*> option [] actualArgs
      , MType        <$> reparsePrimTypeWithArrs
      , MAttr        <$> reparseAttribute
                     <*> ( choice [ Just <$> mTerm, Nothing <$ eof ] )
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

-- | Parse character literal
literalChar :: Reparse CharLiteral
literalChar = do
  (txt, val) <-
    parseTokenOfKind CXToken_Literal reparseLiteralChar
  return $
    CharLiteral
      { charLiteralText  = txt
      , charLiteralValue = val
      }

-- | Parse string literal
literalString :: Reparse StringLiteral
literalString = do
  (txt, val) <-
    parseTokenOfKind CXToken_Literal reparseLiteralString
  return $
    StringLiteral
      { stringLiteralText  = txt
      , stringLiteralValue = val
      }

actualArgs :: Reparse [MExpr]
actualArgs = parens $ mExpr `sepBy` comma

{-------------------------------------------------------------------------------
  Expressions

  This is currently only a subset of the operators described in
  <https://en.cppreference.com/w/c/language/operator_precedence>, but we do
  follow the same structure.
-------------------------------------------------------------------------------}

mExprTuple :: Reparse MExpr
mExprTuple = try tuple <|> mExpr <|> ( MEmpty <$ eof )
  where
    tuple = do
      openParen <- optionMaybe $ punctuation "("
      (e1, e2, es) <- mExpr `sepBy2` comma
      case openParen of
        Nothing -> return ()
        Just {} -> punctuation ")"
      return $
        reifyList es $ \es' ->
           MApp MTuple ( e1 ::: e2 ::: es' )

mExpr :: Reparse MExpr
mExpr = buildExpressionParser ops term <?> "expression"
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

    ap1 :: MFun (S Z) -> MExpr -> MExpr
    ap1 op arg = MApp op ( arg ::: VNil )

    ap2 :: MFun (S (S Z)) -> MExpr -> MExpr -> MExpr
    ap2 op arg1 arg2 = MApp op ( arg1 ::: arg2 ::: VNil )


sepBy2 :: ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m (a, a, [a])
{-# INLINEABLE sepBy2 #-}
sepBy2 p sep = do
  x1 <- p
  void sep
  x2 <- p
  xs <- many $ sep >> p
  return (x1, x2, xs)

{-------------------------------------------------------------------------------
  Array types
-------------------------------------------------------------------------------}

-- | Like 'reparsePrimType', but including array syntax.
reparsePrimTypeWithArrs :: Reparse Type
-- TODO: duplication with HsBindgen.C.Reparse.Decl.withArrayOrFunctionSuffixes
reparsePrimTypeWithArrs = do
  baseTy <- reparsePrimType
  arrSizes <- many $ punctuation "[" *> reparseArraySize <* punctuation "]"
  let
    mkArrs :: [Maybe Natural] -> Type -> Type
    mkArrs [] ty = ty
    mkArrs (mbSz:szs) ty =
      mkArrs szs $
        case mbSz of
          Just s  -> TypeConstArray s ty
          Nothing -> TypeIncompleteArray ty
  return $ mkArrs arrSizes baseTy

reparseArraySize :: Reparse (Maybe Natural)
reparseArraySize =
  -- TODO: many other things can appear in array sizes,
  -- such as the 'static' keyword.
  --
  -- Moreover, the size can be an arbitrary integer constant expression,
  -- not necessarily a simple literal. We don't handle that for now.
  --
  -- See HsBindgen.C.Reparse.Decl.withArrayOrFunctionSuffixes.
  choice
    [ do { (txt, (i, _ty)) <- parseTokenOfKind CXToken_Literal reparseLiteralInteger
         ; if i < 0
           then unexpected $ "negative array size: " <> Text.unpack txt
           else return $ Just $ fromIntegral i }
    , Nothing <$ punctuation "*"
    -- TODO: handle empty size declaration?
    ]
  <?> "array size"
