{-# LANGUAGE MagicHash       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsBindgen.Backend.HsModule.Pretty.Expr (
    prettyExpr
  ) where

import Data.Char qualified
import Data.List qualified as List
import Data.Word
import DeBruijn (Add (..), EmptyCtx, Env (..), lookupEnv)
import GHC.Exts (Int (..), sizeofByteArray#)
import GHC.Exts qualified as IsList (IsList (..))
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Text.SimplePrettyPrint (CtxDoc, Pretty (..), (<+>), (><))
import Text.SimplePrettyPrint qualified as PP

import C.Char qualified as CExpr.Runtime

import C.Expr.Syntax qualified as CExpr

import HsBindgen.Backend.Global
import HsBindgen.Backend.HsModule.Names
import HsBindgen.Backend.HsModule.Pretty.Common
import HsBindgen.Backend.HsModule.Pretty.Type
import HsBindgen.Backend.Level
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.SHs.Translation.Common
import HsBindgen.Imports
import HsBindgen.NameHint

import Numeric (showHex)

{-------------------------------------------------------------------------------
  Expression pretty-printing
-------------------------------------------------------------------------------}

instance ctx ~ EmptyCtx => Pretty (SExpr ctx) where
  prettyPrec = prettyExpr EmptyEnv

prettyExpr :: Env ctx CtxDoc -> Int -> SExpr ctx -> CtxDoc
prettyExpr env prec expr = case asNaryEApp expr of
    (EBoxedTup n, args) ->
      let decls = prettyExpr env 0 <$> args
      in  prettyBoxedTuple n decls
    (EUnboxedTup n, args) ->
      let decls = prettyExpr env 0 <$> args
      in  prettyUnboxedTuple n decls
    (EApp{} , _) ->
      panicWith
        "Unexpected function application after unrolling function application"
        expr
    _otherwise -> prettyRolledExpr env prec expr

-- See 'prettyExpr' but do not unroll/recognize function application.
prettyRolledExpr :: Env ctx CtxDoc -> Int -> SExpr ctx -> CtxDoc
prettyRolledExpr env prec expr = case expr of
    EGlobal g -> pretty $ resolveGlobal g

    EBound x -> lookupEnv x env
    EFree x  -> pretty x
    ECon n   -> pretty n

    EIntegral i Nothing -> PP.parensWhen (prec > 0 && i < 0) (PP.show i)
    EUnboxedIntegral i ->
      PP.parens $ PP.hcat [PP.show i, "#"]
    EIntegral i (Just t) ->
      PP.parens $ PP.hcat [PP.show i, " :: ", prettyType EmptyEnv 0 t]
    EChar (CExpr.Runtime.CharValue { charValue = ba, unicodeCodePoint = mbUnicode }) ->
      prettyExpr env 0 (EGlobal $ cExprGlobalTerm CharValue_fromAddr)
        <+> PP.string str
        <+> PP.string (show len)
        <+> case mbUnicode of
            { Nothing -> pretty (resolveBindgenGlobalTerm Maybe_nothing)
            ; Just c -> PP.parens (pretty (resolveBindgenGlobalTerm Maybe_just) <+> PP.string (show c))
            }
      where
        (str, len) = addrLiteral ba
    EString s -> PP.show s
    ECString bs ->
      -- Use unboxed Addr# literals to turn a PP.string literal into a
      -- value of type CStringLen.
      let (str, len) = addrLiteral bs
      in PP.parens $ PP.hcat
        [ PP.parens $ prettyExpr env 0 (eBindgenGlobal Foreign_Ptr_constructor) <+> PP.string str >< ", " >< PP.string (show len)
        , " :: "
        , prettyTypeGlobal $ bindgenGlobalType CStringLen_type
        ]

    EFloat f t -> PP.parens $ PP.hcat [
        if CExpr.canBeRepresentedAsRational f then
          PP.show f
        else
          prettyExpr env prec $
            EApp (eBindgenGlobal CFloat_constructor) $
              EApp (eBindgenGlobal GHC_Float_castWord32ToFloat) $
                EIntegral (toInteger $ castFloatToWord32 f) (Just $ tBindgenGlobal CUInt_type)
      , " :: "
      , prettyType EmptyEnv 0 t
      ]
    EDouble f t -> PP.parens $ PP.hcat [
        if CExpr.canBeRepresentedAsRational f then
          PP.show f
        else
          prettyExpr env  prec $
            EApp (eBindgenGlobal CDouble_constructor) $
              EApp (eBindgenGlobal GHC_Float_castWord64ToDouble) $
                EIntegral (toInteger $ castDoubleToWord64 f) (Just $ tBindgenGlobal CULong_type)
      , " :: "
      , prettyType EmptyEnv 0 t
      ]

    EApp f x -> PP.parensWhen (prec > 3) $ prettyExpr env 3 f <+> prettyExpr env 4 x

    e@(EInfix op x y) -> case (prec, getInfixSpecialCase env e) of
      -- Handle special cases only at precedence 0.
      (0, Just ds) -> PP.vcat ds
      -- Sub-expressions are aggressively parenthesized so that we do not have
      -- to worry about operator fixity/precedence.
      _otherwise ->
        PP.parens $ PP.hsep
          [ prettyExpr env 1 x
          , prettyInfixResolvedName (resolveGlobal $ infixOpGlobal op)
          , prettyExpr env 1 y
          ]

    ELam (NameHint hint) body -> PP.withFreshName hint $ \x -> PP.parensWhen (prec > 1) $ PP.fsep
      [ PP.char '\\' >< x <+> "->"
      , PP.nest 2 $ prettyExpr (env :> x) 0 body
      ]

    EUnusedLam body -> PP.parensWhen (prec > 1) $ PP.fsep
      [ PP.char '\\' >< "_" <+> "->"
      , PP.nest 2 $ prettyExpr env 0 body
      ]

    ECase x alts -> PP.vparensWhen (prec > 1) $
      if null alts
        then PP.hsep ["case", prettyExpr env 0 x, "of", "{}"]
        else PP.hang (PP.hsep ["case", prettyExpr env 0 x, "of"]) 2 $ PP.vcat
            ([ withFreshNames env add hints $ \env' params ->

                let l = PP.hsep $ pretty cnst : params ++ ["->"]
                in  PP.ifFits l (PP.fsep [l, PP.nest 2 (prettyExpr env' 0 body)]) $
                    case unsnoc params of
                      Nothing -> PP.fsep [l, PP.nest 2 (prettyExpr env' 0 body)]
                      Just (lParams, rParam) -> PP.vcat $
                          pretty cnst
                        : [ PP.nest 2 param
                          | param <- lParams
                          ]
                        ++ [PP.nest 2 (rParam <+> "->")]
                        ++ [PP.nest 4 (prettyExpr env' 0 body)]

            | SAlt cnst add hints body <- alts
            ]
            ++
            [ withFreshNames env (AS AZ) hints $ \env' params ->
                let l = PP.hsep $ params ++ ["->"]
                in  PP.ifFits l (PP.fsep [l, PP.nest 2 (prettyExpr env' 0 body)]) $
                    case unsnoc params of
                      Nothing -> PP.fsep [l, PP.nest 2 (prettyExpr env' 0 body)]
                      Just (lParams, rParam) -> PP.vcat $
                          [ PP.nest 2 param
                          | param <- lParams
                          ]
                        ++ [PP.nest 2 (rParam <+> "->")]
                        ++ [PP.nest 4 (prettyExpr env' 0 body)]

            | SAltNoConstr hints body <- alts
            ]
            ++
            [ withFreshNames env add hints $ \env' params ->
                let l  = PP.hlist "(# " " #)" params <+> "->"
                in  PP.ifFits l (PP.fsep [l, PP.nest 2 (prettyExpr env' 0 body)]) $
                    case unsnoc params of
                      Nothing -> PP.fsep [l, PP.nest 2 (prettyExpr env' 0 body)]
                      Just (lParams, rParam) -> PP.vcat $
                          [ PP.nest 2 param
                          | param <- lParams
                          ]
                        ++ [PP.nest 2 (rParam <+> "->")]
                        ++ [PP.nest 4 (prettyExpr env' 0 body)]

            | SAltUnboxedTuple add hints body <- alts
            ]
            )

    EUnit -> PP.string "()"

    -- Handled in 'prettyExpr'.
    EBoxedTup{} ->
      panicWith
        "Unexpected boxed unsaturated tuple after unrolling function application"
        expr

    -- Handled in 'prettyExpr'.
    EUnboxedTup{} ->
      panicWith
        "Unexpected unboxed unsaturated tuple after unrolling function application"
        expr

    EList xs ->
      let ds = prettyExpr env 0 <$> xs
          l  = PP.hlist "[" "]" ds
      in  PP.ifFits l l $ PP.vlist "[" "]" ds

    -- NOTE: the precedence is copied from the @EApp@ case above
    ETypeApp f t ->
      PP.parensWhen (prec > 3) $
        prettyExpr env 3 f <+> "@" >< prettyType EmptyEnv 4 t

prettyTypeGlobal :: Global LvlType -> CtxDoc
prettyTypeGlobal = prettyType EmptyEnv 0 . TGlobal

-- | Returns the unboxed @Addr#@ literal for the given 'Data.Array.Byte.ByteArray', together
-- with its length.
addrLiteral :: ByteArray -> (String, Int)
addrLiteral ba@(ByteArray ba#) =
  let
    go :: Bool -> [Word8] -> String
    go _ [] = ""
    go prevHex (b:bs)
      | Just s <- escapeHsChar_maybe c
      = s ++ go False bs
      | b <= 0x7F
      , Data.Char.isPrint c
      = ( if prevHex then ( "\\&" ++ ) else id ) $
          c : go False bs
      | otherwise
      = "\\x" ++ map Data.Char.toUpper (showHex b "") ++ go True bs
      where
        c = Data.Char.chr $ fromIntegral b
    lit :: String
    lit = "\"" <> go False (IsList.toList ba) <> "\"#"
  in (lit, I# (sizeofByteArray# ba#))

escapeHsChar_maybe :: Char -> Maybe String
escapeHsChar_maybe c =
  case lookup c hsEscapes of
    Nothing -> Nothing
    Just e -> Just ['\\', e]

hsEscapes :: [(Char, Char)]
hsEscapes =
  [ ( '\''  , '\'' ) -- single quote
  , ( '\"'  , '\"' ) -- double quote
  , ( '\\'  , '\\' ) -- backslash
  , ( '\f'  , 'f'  ) -- form feed - new page
  , ( '\t'  , 't'  ) -- horizontal tab
  , ( '\v'  , 'v'  ) -- vertical tab
  , ( '\a'  , 'a'  ) -- audible bell
  , ( '\b'  , 'b'  ) -- backspace
  , ( '\n'  , 'n'  ) -- line feed - new line
  , ( '\r'  , 'r'  ) -- carriage return
  , ( '\NUL', '0'  ) -- null character
  ]

getInfixSpecialCase :: forall ctx. Env ctx CtxDoc -> SExpr ctx -> Maybe [CtxDoc]
getInfixSpecialCase env = \case
    EInfix op x y ->
      let opGlo = infixOpGlobal op
          opDoc = prettyInfixResolvedName $ resolveGlobal opGlo
      in  case op of
            InfixApplicative_seq -> auxl op opDoc [opDoc <+> prettyExpr env 1 y] x
            InfixMonad_seq       -> auxr op opDoc [sp opDoc <+> prettyExpr env 1 x] y
            _otherwise      -> Nothing
    _otherwise -> Nothing
  where
    -- | Handle left-associative special cases
    auxl ::
         InfixOp  -- ^ operator
      -> CtxDoc   -- ^ operator document
      -> [CtxDoc] -- ^ accumulated lines
      -> SExpr ctx -- ^ left expression
      -> Maybe [CtxDoc]
    auxl op opDoc acc = \case
      EInfix op' x y
        | op' == op -> auxl op opDoc (opDoc <+> prettyExpr env 1 y : acc) x
        | otherwise -> Nothing
      e -> Just $ sp opDoc <+> prettyExpr env 1 e : acc

    -- | Handle right-associative special cases
    auxr ::
         InfixOp   -- ^ operator
      -> CtxDoc   -- ^ operator document
      -> [CtxDoc] -- ^ accumulated lines in reverse order
      -> SExpr ctx -- ^ right expression
      -> Maybe [CtxDoc]
    auxr op opDoc acc = \case
      EInfix op' x y
        | op' == op -> auxr op opDoc (opDoc <+> prettyExpr env 1 x : acc) y
        | otherwise -> Nothing
      e -> Just . reverse $ opDoc <+> prettyExpr env 1 e : acc

    -- | Create document of spaces that has same width as passed document
    sp :: CtxDoc -> CtxDoc
    sp =
      -- TODO compute column width, do not just count chars with length
      PP.string . flip List.replicate ' ' . length . show

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | In "Data.List" from @base-4.19.0.0@
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
{-# INLINABLE unsnoc #-}

resolveBindgenGlobalTerm :: BindgenGlobalTerm -> ResolvedName
resolveBindgenGlobalTerm = resolveGlobal . bindgenGlobalTerm

-- | Pretty-print a 'HsBindgen.Backend.HsModule.Names.ResolvedName' in infix notation
--
-- Identifiers are surrounded by backticks.
prettyInfixResolvedName :: ResolvedName -> CtxDoc
prettyInfixResolvedName resolved =
    bticksWhen (resolved.typ == IdentifierName) $ prettyResolvedNamePlain resolved
  where
    bticksWhen :: Bool -> CtxDoc -> CtxDoc
    bticksWhen False d = d
    bticksWhen True  d = PP.hcat [PP.char '`', d, PP.char '`']
