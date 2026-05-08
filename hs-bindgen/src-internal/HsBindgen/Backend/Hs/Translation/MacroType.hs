-- | Translate typechecked macro type expressions to Haskell types
--
-- Intended for qualified import
--
-- > import HsBindgen.Backend.Hs.Translation.MacroType qualified as MacroType
--
-- TODO <https://github.com/well-typed/hs-bindgen/issues/1953>
--
-- == Temporary: Two conversions from @T.Expr@
--
-- There are two independent conversions from 'T.Expr' (the typechecked
-- macro-type tree):
--
-- * __Macro → C type__
--   ('HsBindgen.Frontend.Pass.TypecheckMacros.convertTExpr'): converts a
--   'T.Expr' to a 'C.Type'. Used during the 'TypecheckMacros' pass to populate
--   the language-c reparse environment so that machinery relying on the
--   **underlying C type** (e.g., FFI type) keeps working.
--
-- * __Macro → Haskell type__ ('macroTypeToHsType', this module): converts a
--   'T.Expr' to an 'Hs.HsType'. Used in the backend when generating the newtype
--   wrapper for a type-like macro. This is the desired end state, allowing
--   users to employ different macro frameworks.
module HsBindgen.Backend.Hs.Translation.MacroType (
    macroTypeToHsType
  ) where

import Data.Proxy (Proxy (..))

import C.Expr.Syntax qualified as CExpr
import C.Expr.Typecheck qualified as CExpr
import C.Expr.Typecheck.Interface.Type qualified as T

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Translation.Type qualified as Type
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Convert a typechecked macro type expression to a Haskell type.
--
-- Top-level incomplete types (@void@, @const void@) are rejected by
-- 'CExpr.tcMacro' before this function is reached; 'CExpr.TypeVoid' can
-- therefore only appear as the pointee of a pointer (e.g. @void *@).
macroTypeToHsType :: CExpr.CheckedMacroTypeExpr (MacroTypeBodyVar Final) -> Hs.HsType
macroTypeToHsType expr = go expr.macroTypeBody
  where
    go :: T.Expr (MacroTypeBodyVar Final) -> Hs.HsType
    go = \case
      T.TypeLit lit ->
        Hs.HsPrimType (convertLiteral lit)
      T.Var (MacroTypeExtBinding ext) ->
        Hs.HsExtBinding ext.hsName ext.cSpec ext.hsSpec $
          Hs.HsTypRef
            (Hs.assertNs (Proxy @Hs.NsTypeConstr)
            (extDeclIdPair ext).hsName)
            Nothing
      T.Var (MacroTypeBodyVar pair) ->
        Hs.HsTypRef (Hs.assertNs (Proxy @Hs.NsTypeConstr) pair.hsName) Nothing
      T.App T.Pointer t ->
        ptrOf t
      T.App T.Const t ->
        go t

    -- Mirrors the const-pointer distinction in 'Type.topLevel'
    ptrOf :: T.Expr (MacroTypeBodyVar Final) -> Hs.HsType
    ptrOf (T.App T.Const t) = Hs.HsPtrConst (go t)
    ptrOf t                 = Hs.HsPtr (go t)

{-------------------------------------------------------------------------------
  Literals
-------------------------------------------------------------------------------}

convertLiteral :: CExpr.TypeLit -> Hs.HsPrimType
convertLiteral = \case
    CExpr.TypeInt  sign size ->
      Type.primType $ C.PrimIntegral (convertIntSize size) (convertSign sign)
    CExpr.TypeChar sign ->
      Type.primType $ C.PrimChar (convertCharSign sign)
    CExpr.TypeFloat size ->
      Type.primType $ C.PrimFloating (convertFloatSize size)
    CExpr.TypeBool ->
      Type.primType C.PrimBool
    -- Only reachable as the pointee of a pointer (e.g. @void *@, @const void *@);
    -- bare 'void' and 'const void' macro types are rejected by 'tcMacro'.
    CExpr.TypeVoid ->
      Hs.HsPrimVoid

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
