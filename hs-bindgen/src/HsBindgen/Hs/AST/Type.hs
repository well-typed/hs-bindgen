module HsBindgen.Hs.AST.Type (
  HsPrimType (..),
  HsType (..),  
) where

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

data HsPrimType
    = HsPrimCInt
    | HsPrimCChar
    | HsPrimCFloat
  deriving stock (Show)

data HsType =
    HsType String
  | HsPrimType HsPrimType
  deriving stock (Show)