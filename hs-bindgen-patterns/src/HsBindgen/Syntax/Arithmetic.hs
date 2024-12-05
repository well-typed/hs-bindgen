{-# LANGUAGE TypeFamilies #-}

module HsBindgen.Syntax.Arithmetic where

import Prelude hiding
  ( Eq(..), Ord(..), Num(..), Fractional(..) )
import Prelude qualified

import Data.Kind ( Type, Constraint )

--------------------------------------------------------------------------------

type IntLike   a = Prelude.Integral   a => a
type FloatLike a = Prelude.Fractional a => a

--------------------------------------------------------------------------------
-- Logical operators

type NotRes :: Type -> Type
type family NotRes a
type CNot :: Type -> Constraint
class CNot a where
  not :: a -> NotRes a

infixr 3 &&
infixr 2 ||

type LogicalRes :: Type -> Type -> Type
type family LogicalRes a b

type CLogical :: Type -> Type -> Constraint
class CLogical a b where
  (&&), (||) :: a -> b -> LogicalRes a b

--------------------------------------------------------------------------------
-- Comparison operators

infixr 4 ==
infixr 4 /=

type CEq :: Type -> Type -> Constraint
class CEq a b where
  (==), (/=) :: a -> b -> Bool

type COrd :: Type -> Type -> Constraint
class COrd a b where
  (<), (>), (<=), (>=) :: a -> b -> Bool

--------------------------------------------------------------------------------
-- Arithmetic operators

-- Unary
type PlusRes :: Type -> Type
type family PlusRes a

type CPlus :: Type -> Constraint
class CPlus a where
  plus :: a -> PlusRes a

type MinusRes :: Type -> Type
type family MinusRes a

type CMinus :: Type -> Constraint
class CMinus a where
  negate :: a -> MinusRes a

-- Binary additive
type AddRes :: Type -> Type -> Type
type family AddRes a b

infixl 6 +
type CAdd :: Type -> Type -> Constraint
class CAdd a b where
  (+) :: a -> b -> AddRes a b

type SubRes :: Type -> Type -> Type
type family SubRes a b

infixl 6 -
type CSub :: Type -> Type -> Constraint
class CSub a b where
  (-) :: a -> b -> SubRes a b

-- Binary multiplicative

type MultRes :: Type -> Type -> Type
type family MultRes a b

infixl 7 *
type CMult :: Type -> Type -> Constraint
class CMult a b where
  (*) :: a -> b -> MultRes a b

type DivRes :: Type -> Type -> Type
type family DivRes a b

infixl 7 /
type CDiv :: Type -> Type -> Constraint
class CDiv a b where
  (/) :: a -> b -> DivRes a b

type RemRes :: Type -> Type -> Type
type family RemRes a b

infixl 7 `rem`
type CRem :: Type -> Type -> Constraint
class CRem a b where
  rem :: a -> b -> RemRes a b

--------------------------------------------------------------------------------
-- Bit operators

type ComplementRes :: Type -> Type
type family ComplementRes a

type CComplement :: Type -> Constraint
class CComplement a where
  complement :: a -> ComplementRes a

type BitsRes :: Type -> Type -> Type
type family BitsRes a b

infixl 7 .&.
infixl 6 `xor`
infixl 5 .|.
type CBits :: Type -> Type -> Constraint
class CBits a b where
  (.&.), (.|.), xor :: a -> b -> BitsRes a b

-- Bit shift operators

type ShiftRes :: Type -> Type
type family ShiftRes a

type CShift :: Type -> Type -> Constraint
class CShift a i where
  shiftL, shiftR :: a -> i -> ShiftRes a

--------------------------------------------------------------------------------
