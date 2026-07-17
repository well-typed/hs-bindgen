{-# LANGUAGE TemplateHaskellQuotes #-}
module HsBindgen.Internal.Macro.Global (
    cExprGlobalType
  , CExprGlobalType(..)
  , cExprGlobalTerm
  , CExprGlobalTerm(..)
  ) where

import Language.Haskell.TH qualified as TH

import C.Expr.HostPlatform qualified

import HsBindgen.Backend.Global
import HsBindgen.Backend.Level
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Globals specific to C expressions
-------------------------------------------------------------------------------}

-- We avoid full Template Haskell name resolution, because we want to depend on
-- the intermediate runtime modules.
cExprToHsImport :: Hs.Import
cExprToHsImport = Hs.QualifiedImport "C.Expr.HostPlatform" Nothing

data CExprGlobalType =
    Not_class
  | Logical_class
  | RelEq_class
  | RelOrd_class
  | Plus_class
  | Plus_resTyCon
  | Minus_class
  | Minus_resTyCon
  | Add_class
  | Add_resTyCon
  | Sub_class
  | Sub_resTyCon
  | Mult_class
  | Mult_resTyCon
  | Div_class
  | Div_resTyCon
  | Rem_class
  | Rem_resTyCon
  | Complement_class
  | Complement_resTyCon
  | Bitwise_class
  | Bitwise_resTyCon
  | Shift_class
  | Shift_resTyCon

data CExprGlobalTerm =
    Not_not
  | Logical_and
  | Logical_or
  | RelEq_eq
  | RelEq_uneq
  | RelOrd_lt
  | RelOrd_le
  | RelOrd_gt
  | RelOrd_ge
  | Plus_plus
  | Minus_negate
  | Add_add
  | Sub_minus
  | Mult_mult
  | Div_div
  | Rem_rem
  | Complement_complement
  | Bitwise_and
  | Bitwise_or
  | Bitwise_xor
  | Shift_shiftL
  | Shift_shiftR

cExprGlobalType :: CExprGlobalType -> Global LvlType
cExprGlobalType = aux . \case
    Not_class           -> (GTyp, ''C.Expr.HostPlatform.Not)
    Logical_class       -> (GTyp, ''C.Expr.HostPlatform.Logical)
    RelEq_class         -> (GTyp, ''C.Expr.HostPlatform.RelEq)
    RelOrd_class        -> (GTyp, ''C.Expr.HostPlatform.RelOrd)
    Plus_class          -> (GTyp, ''C.Expr.HostPlatform.Plus)
    Plus_resTyCon       -> (GTyp, ''C.Expr.HostPlatform.PlusRes)
    Minus_class         -> (GTyp, ''C.Expr.HostPlatform.Minus)
    Minus_resTyCon      -> (GTyp, ''C.Expr.HostPlatform.MinusRes)
    Add_class           -> (GTyp, ''C.Expr.HostPlatform.Add)
    Add_resTyCon        -> (GTyp, ''C.Expr.HostPlatform.AddRes)
    Sub_class           -> (GTyp, ''C.Expr.HostPlatform.Sub)
    Sub_resTyCon        -> (GTyp, ''C.Expr.HostPlatform.SubRes)
    Mult_class          -> (GTyp, ''C.Expr.HostPlatform.Mult)
    Mult_resTyCon       -> (GTyp, ''C.Expr.HostPlatform.MultRes)
    Div_class           -> (GTyp, ''C.Expr.HostPlatform.Div)
    Div_resTyCon        -> (GTyp, ''C.Expr.HostPlatform.DivRes)
    Rem_class           -> (GTyp, ''C.Expr.HostPlatform.Rem)
    Rem_resTyCon        -> (GTyp, ''C.Expr.HostPlatform.RemRes)
    Complement_class    -> (GTyp, ''C.Expr.HostPlatform.Complement)
    Complement_resTyCon -> (GTyp, ''C.Expr.HostPlatform.ComplementRes)
    Bitwise_class       -> (GTyp, ''C.Expr.HostPlatform.Bitwise)
    Bitwise_resTyCon    -> (GTyp, ''C.Expr.HostPlatform.BitsRes)
    Shift_class         -> (GTyp, ''C.Expr.HostPlatform.Shift)
    Shift_resTyCon      -> (GTyp, ''C.Expr.HostPlatform.ShiftRes)
  where
    aux :: (GlobalCat LvlType, TH.Name) -> Global LvlType
    aux (c, n) = CustomGlobal n c cExprToHsImport

cExprGlobalTerm :: CExprGlobalTerm -> Global LvlTerm
cExprGlobalTerm = aux . \case
    Not_not               -> (GVar,  'C.Expr.HostPlatform.not)
    Logical_and           -> (GVar, '(C.Expr.HostPlatform.&&))
    Logical_or            -> (GVar, '(C.Expr.HostPlatform.||))
    RelEq_eq              -> (GVar, '(C.Expr.HostPlatform.==))
    RelEq_uneq            -> (GVar, '(C.Expr.HostPlatform.!=))
    RelOrd_lt             -> (GVar, '(C.Expr.HostPlatform.<))
    RelOrd_le             -> (GVar, '(C.Expr.HostPlatform.<=))
    RelOrd_gt             -> (GVar, '(C.Expr.HostPlatform.>))
    RelOrd_ge             -> (GVar, '(C.Expr.HostPlatform.>=))
    Plus_plus             -> (GVar,  'C.Expr.HostPlatform.plus)
    Minus_negate          -> (GVar,  'C.Expr.HostPlatform.negate)
    Add_add               -> (GVar, '(C.Expr.HostPlatform.+))
    Sub_minus             -> (GVar, '(C.Expr.HostPlatform.-))
    Mult_mult             -> (GVar, '(C.Expr.HostPlatform.*))
    Div_div               -> (GVar, '(C.Expr.HostPlatform./))
    Rem_rem               -> (GVar, '(C.Expr.HostPlatform.%))
    Complement_complement -> (GVar, '(C.Expr.HostPlatform..~))
    Bitwise_and           -> (GVar, '(C.Expr.HostPlatform..&.))
    Bitwise_or            -> (GVar, '(C.Expr.HostPlatform..|.))
    Bitwise_xor           -> (GVar, '(C.Expr.HostPlatform..^.))
    Shift_shiftL          -> (GVar, '(C.Expr.HostPlatform.<<))
    Shift_shiftR          -> (GVar, '(C.Expr.HostPlatform.>>))
  where
    aux :: (GlobalCat LvlTerm, TH.Name) -> Global LvlTerm
    aux (c, n) =
      CustomGlobal n c cExprToHsImport
