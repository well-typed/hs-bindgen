{-# OPTIONS_GHC -Wno-orphans #-}
module Orphans where

import Data.Text qualified as Text
import Data.TreeDiff.Class (ToExpr(..))
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

instance ToExpr C.Decl
instance ToExpr C.Enu
instance ToExpr C.EnumValue
instance ToExpr C.Header
instance ToExpr C.Macro
instance ToExpr C.PrimSign
instance ToExpr C.PrimType
instance ToExpr C.SourceLoc
instance ToExpr C.SourceRange
instance ToExpr C.Struct
instance ToExpr C.StructField
instance ToExpr C.TokenSpelling
instance ToExpr C.Typ
instance ToExpr C.Typedef

instance ToExpr a => ToExpr (C.Token a)

-- Construct platform-independent expression
instance ToExpr C.SourcePath where
  toExpr = toExpr . splitDirectories . Text.unpack . C.getSourcePath

{-------------------------------------------------------------------------------
  hs-bindgen-patterns
-------------------------------------------------------------------------------}

instance ToExpr (SimpleEnum hs)
