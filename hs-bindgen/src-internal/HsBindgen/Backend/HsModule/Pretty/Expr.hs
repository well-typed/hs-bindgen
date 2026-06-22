{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsBindgen.Backend.HsModule.Pretty.Expr (
    prettyExpr
  ) where

import Data.ByteString qualified as BS
import Data.Char qualified
import Data.List qualified as List
import Data.Word
import DeBruijn (Add (..), EmptyCtx, Env (..), lookupEnv)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Text.SimplePrettyPrint (CtxDoc, Pretty (..), (<+>), (><))
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Backend.Global
import HsBindgen.Backend.HsModule.Names
import HsBindgen.Backend.HsModule.Pretty.Common
import HsBindgen.Backend.HsModule.Pretty.Type
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.SHs.AST.Expr (FBind (FBind))
import HsBindgen.Backend.SHs.Translation.Common
import HsBindgen.NameHint
import HsBindgen.Util.Rational (canBeRepresentedAsRational)

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
    ECChar c -> PP.show c
    EString s -> PP.show s
    ECString bs ->
      let
        bytes   = BS.unpack bs
        bsList  = PP.string $ "[" ++ List.intercalate ", " (map showHexByte bytes) ++ "]"
        packApp = prettyExpr env 3 (eBindgenGlobal ByteString_pack) <+> bsList
      in PP.parensWhen (prec > 3) packApp

    EFloat f t -> PP.parens $ PP.hcat [
        if canBeRepresentedAsRational f then
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
        if canBeRepresentedAsRational f then
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

    -- NOTE: the precedence is copied from the @EApp@ case above
    ERecCon con fs ->
      let fsDocs = fmap (prettyFBind env) fs
          hl = PP.hlist "{" "}" fsDocs
          vl = PP.vlist "{" "}" fsDocs
      in  PP.parensWhen (prec > 3) $
            pretty con <+> PP.ifFits hl hl vl

-- | Pretty-print a field binding
--
-- Field bindings do not have to be parenthesised in the context where they are
-- used: record construction and record update. As such, this function does not
-- get a precedence argument.
prettyFBind :: Env ctx CtxDoc -> FBind ctx -> CtxDoc
prettyFBind env (FBind label expr) = PP.string label <+> "=" <+> prettyExpr env 0 expr

-- | Format a byte as a two-digit uppercase hex literal, e.g. @0x00@, @0xE3@.
showHexByte :: Word8 -> String
showHexByte w = "0x" ++ map Data.Char.toUpper (pad2 (showHex w ""))
  where
    pad2 s = if length s == 1 then '0' : s else s

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
