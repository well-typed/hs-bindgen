{-# OPTIONS_GHC -Wno-orphans #-}
module Orphans where

import Data.Foldable (toList)
import Data.Text qualified as Text
import Data.TreeDiff.Class (ToExpr(..))
import Data.TreeDiff.Expr qualified as Expr
import Foreign.C
import System.FilePath (splitDirectories)

import HsBindgen.C.AST qualified as C
import HsBindgen.Lib
import HsBindgen.Patterns

{-------------------------------------------------------------------------------
  base
-------------------------------------------------------------------------------}

instance ToExpr CInt where
  toExpr = toExpr . (fromIntegral :: CInt -> Int)

{-------------------------------------------------------------------------------
  hs-bindgen
-------------------------------------------------------------------------------}

instance ToExpr CHeader

instance ToExpr C.Attribute
instance ToExpr C.CName
instance ToExpr C.Decl
instance ToExpr C.Enu
instance ToExpr C.EnumValue
instance ToExpr C.Header
instance ToExpr C.Macro
instance ToExpr C.MacroDecl
instance ToExpr C.MTerm
instance ToExpr C.MultiLoc
instance ToExpr C.PrimSign
instance ToExpr C.PrimIntType
instance ToExpr C.PrimFloatType
instance ToExpr C.PrimType
instance ToExpr C.ReparseError
instance ToExpr C.SingleLoc
instance ToExpr C.Struct
instance ToExpr C.StructField
instance ToExpr C.TokenSpelling
instance ToExpr C.Typ
instance ToExpr C.Typedef

instance ToExpr C.IntegerLiteral
instance ToExpr C.FloatingLiteral
instance ToExpr a => ToExpr (C.Range a)
instance ToExpr a => ToExpr (C.Token a)

-- Construct platform-independent expression
instance ToExpr C.SourcePath where
  toExpr = toExpr . splitDirectories . Text.unpack . C.getSourcePath

instance ToExpr C.TcMacroError where
  toExpr err = toExpr $ C.pprTcMacroError err

instance ToExpr C.MExpr where
  toExpr = \case
    C.MTerm tm ->
      Expr.App "MTerm" [toExpr tm]
    C.MApp fun args ->
      Expr.App "MApp" [toExpr fun, toExpr (toList args)]

instance ToExpr ( C.MFun arity ) where
  toExpr f = Expr.App (show f) []

instance ToExpr C.QuantTy where
  toExpr quantTy = toExpr $ show quantTy

{-------------------------------------------------------------------------------
  hs-bindgen-patterns
-------------------------------------------------------------------------------}

instance ToExpr (SimpleEnum hs)
