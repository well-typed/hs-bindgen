{-# OPTIONS_GHC -Wno-orphans #-}
module Orphans where

import Data.TreeDiff.Class (ToExpr)

import HsBindgen.Lib
import HsBindgen.C.AST qualified as C

instance ToExpr CHeader
instance ToExpr C.Header
instance ToExpr C.Decl
instance ToExpr C.Struct
instance ToExpr C.Typedef
instance ToExpr C.Typ
instance ToExpr C.PrimType
instance ToExpr C.StructField
