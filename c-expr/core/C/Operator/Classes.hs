{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module C.Operator.Classes
  ( -- * Logical operators
    Not(..)
  , Logical(..)
    -- * Equality and comparison
  , RelEq(..), RelOrd(..)
  , NotNull(..)
    -- * Arithmetic
    -- ** Unary
  , Plus(..)
  , Minus(..)
    -- ** Binary
  , Add(..)
  , Sub(..)
  , Mult(..)
  , Div(..)
  , Rem(..)
    -- * Bitwise
    -- ** Unary
  , Complement(..)
    -- ** Binary
  , Bitwise(..)
  , Shift(..)
  ) where

-- base
import Prelude
  ( Bool(..), Eq(..), Num(..) )

import Data.Kind
  ( Type, Constraint )
import Foreign
  ( Ptr, nullPtr )
import Foreign.C

--------------------------------------------------------------------------------

-- | Class to compare whether a value is zero/null.
type NotNull :: Type -> Constraint
class NotNull a where
  notNull :: a -> Bool
instance ( Eq a, Num a ) => NotNull a where
  notNull = ( /= 0 )
instance {-# OVERLAPPING #-} NotNull ( Ptr a ) where
  notNull = ( /= nullPtr )

--------------------------------------------------------------------------------

infixr 0 `not`
-- | Class for the C logical negation operator.
type Not :: Type -> Constraint
class Not a where
  -- | C logical negation operator.
  not :: a -> CInt

infixl 7 &&
infixl 8 ||
-- | Class for C boolean logical operators (conjunction and disjunction).
type Logical :: Type -> Type -> Constraint
class Logical a b where
  (&&), (||) :: a -> b -> CInt

--------------------------------------------------------------------------------

infixl 5 ==
infixl 5 !=
-- | Class for C equality and inequality operators.
type RelEq :: Type -> Type -> Constraint
class RelEq a b where
  (==), (!=) :: a -> b -> CInt

infixl 4 >=
infixl 4 <
infixl 4 <=
infixl 4 >
-- | Class for C relative comparison operators (less than, greater than or equal, etc).
type RelOrd :: Type -> Type -> Constraint
class RelOrd a b where
  (<=), (<), (>=), (>) :: a -> b -> CInt

--------------------------------------------------------------------------------

infixr 0 `plus`
-- | Class for the C unary plus operator.
type Plus :: Type -> Constraint
class Plus a where
  -- | Result type family of the C unary plus operator.
  type family PlusRes a :: Type
  -- | C unary plus operator.
  plus :: a -> PlusRes a

infixr 0 `negate`
-- | Class for the C unary minus operator.
type Minus :: Type -> Constraint
class Minus a where
  -- | Result type family of the C unary minus operator.
  type family MinusRes a :: Type
  -- | C unary plus minus.
  negate :: a -> MinusRes a

infixl 2 +
-- | Class for the C binary addition operator.
type Add :: Type -> Type -> Constraint
class Add a b where
  -- | Result type family of the C binary addition operator.
  type family AddRes a b :: Type
  -- | C binary addition operator.
  (+) :: a -> b -> AddRes a b

infixl 2 -
-- | Class for the C binary subtraction operator.
type Sub :: Type -> Type -> Constraint
class Sub a b where
  -- | Result type family of the C binary subtraction operator.
  type family SubRes a b :: Type
  -- | C binary subtraction operator.
  (-) :: a -> b -> SubRes a b

infixl 1 *
-- | Class for the C binary multiplication operator..
type Mult :: Type -> Type -> Constraint
class Mult a b where
  -- | Result type family of the C binary multiplication operator.
  type family MultRes a b :: Type
  -- | C binary multiplication operator.
  (*) :: a -> b -> MultRes a b

infixl 1 /
-- | Class for the C binary division operator.
type Div :: Type -> Type -> Constraint
class Div a b where
  -- | Result type family of the C binary division operator.
  type family DivRes a b :: Type
  -- | C binary division operator.
  (/) :: a -> b -> DivRes a b

infixl 1 %
-- | Class for the C binary remainder operator..
type Rem :: Type -> Type -> Constraint
class Rem a b where
  -- | Result type family of the C binary remainder operator.
  type family RemRes a b :: Type
  -- | C binary remainder operator.
  (%) :: a -> b -> RemRes a b

--------------------------------------------------------------------------------

infixr 0 .~
-- | Class for the C unary bitwise complement operator.
type Complement :: Type -> Constraint
class Complement a where
  -- | Result type family of the C unary bitwise complement operator.
  type family ComplementRes a :: Type
  -- | C unary bitwise complement operator.
  (.~) :: a -> ComplementRes a

infixl 7 .&.
infixl 8 .|.
infixl 6 .^.
-- | Class for C binary bitwise logical operators.
type Bitwise :: Type -> Type -> Constraint
class Bitwise a b where
  -- | Result type family of C binary bitwise logical operators.
  type family BitsRes a b :: Type
  -- | C binary bitwise *and* operator.
  (.&.) :: a -> b -> BitsRes a b
  -- | C binary bitwise *or* operator.
  (.|.) :: a -> b -> BitsRes a b
  -- | C binary bitwise *xor* operator.
  (.^.) :: a -> b -> BitsRes a b

infixl 3 <<
infixl 3 >>
-- | Class for the C binary bit-shift operators.
type Shift :: Type -> Type -> Constraint
class Shift a i where
  -- | Result type family of C binary bit-shift operators.
  type family ShiftRes a :: Type
  -- | C binary left-shift operator.
  (<<) :: a -> i -> ShiftRes a
  -- | C binary right-shift operator.
  (>>) :: a -> i -> ShiftRes a

--------------------------------------------------------------------------------
