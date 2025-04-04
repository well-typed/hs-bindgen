{-# LANGUAGE LambdaCase #-}

module HsBindgen.Runtime.CEnum.Exception (
    -- * Type
    CEnumException(..)
  ) where

import Control.Exception (Exception(displayException))

{-------------------------------------------------------------------------------
  Type
-------------------------------------------------------------------------------}

data CEnumException
  = CEnumInvalid Integer
  | CEnumNoSuccessor Integer
  | CEnumNoPredecessor Integer
  | CEnumEmpty
  | CEnumFromEqThen Integer
  deriving (Eq, Show)

instance Exception CEnumException where
  displayException = \case
    CEnumInvalid       i -> "C enumeration value invalid: "            ++ show i
    CEnumNoSuccessor   i -> "C enumeration value has no successor: "   ++ show i
    CEnumNoPredecessor i -> "C enumeration value has no predecessor: " ++ show i
    CEnumEmpty           -> "C enumeration has no values"
    CEnumFromEqThen    i -> "enumeration from and then values equal: " ++ show i
