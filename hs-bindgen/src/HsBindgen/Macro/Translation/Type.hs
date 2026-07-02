module HsBindgen.Macro.Translation.Type (
    translateMacroType
  ) where

import C.Expr.Syntax qualified as CExpr
import C.Expr.Typecheck
import C.Expr.Typecheck.Interface.Type qualified as T

import HsBindgen.Backend.Hs.Translation.Type qualified as Type
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.Language.C qualified as C
import HsBindgen.Macro.CExpr (CExpr)
import HsBindgen.Macro.CExpr qualified as Macro

translateMacroType ::
     Macro.TypecheckedType CExpr Hs.Type
  -> Hs.Type
translateMacroType (Macro.TypecheckedTypeCExpr tcExpr) = go tcExpr.macroTypeBody
  where
    go :: T.Expr Hs.Type -> Hs.Type
    go = \case
      T.TypeLit lit     -> Hs.PrimType (convertLiteral lit)
      T.Var v           -> v
      T.App T.Pointer t -> ptrOf t
      T.App T.Const   t -> go t

    ptrOf :: T.Expr Hs.Type -> Hs.Type
    ptrOf (T.App T.Const t) = Hs.PtrConst (go t)
    ptrOf t                 = Hs.Ptr (go t)

    convertLiteral :: CExpr.TypeLit -> Hs.PrimType
    convertLiteral = \case
      CExpr.TypeInt  sign size -> Type.primType $ C.PrimIntegral (convertIntSize size) (convertSign sign)
      CExpr.TypeChar sign      -> Type.primType $ C.PrimChar (convertCharSign sign)
      CExpr.TypeFloat size     -> Type.primType $ C.PrimFloating (convertFloatSize size)
      CExpr.TypeBool           -> Type.primType C.PrimBool
      CExpr.TypeVoid           -> Hs.PrimVoid

    convertSign :: Maybe CExpr.Sign -> C.PrimSign
    convertSign = \case
      Nothing             -> C.Signed
      Just CExpr.Signed   -> C.Signed
      Just CExpr.Unsigned -> C.Unsigned

    convertCharSign :: Maybe CExpr.Sign -> C.PrimSignChar
    convertCharSign = \case
      Nothing             -> C.PrimSignImplicit Nothing
      Just CExpr.Signed   -> C.PrimSignExplicit C.Signed
      Just CExpr.Unsigned -> C.PrimSignExplicit C.Unsigned

    convertIntSize :: Maybe CExpr.IntSize -> C.PrimIntType
    convertIntSize = \case
      Nothing                 -> C.PrimInt
      Just CExpr.SizeShort    -> C.PrimShort
      Just CExpr.SizeInt      -> C.PrimInt
      Just CExpr.SizeLong     -> C.PrimLong
      Just CExpr.SizeLongLong -> C.PrimLongLong

    convertFloatSize :: CExpr.FloatSize -> C.PrimFloatType
    convertFloatSize = \case
      CExpr.SizeFloat  -> C.PrimFloat
      CExpr.SizeDouble -> C.PrimDouble
