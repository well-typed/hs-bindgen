-- | Type expressions for C type macros.
--
-- Intended import:
--
-- > import HsBindgen.Macro.Type qualified as Type
module HsBindgen.Macro.Type (
    -- * Type expressions
    Expr(..)
    -- * Primitives
  , TypeLit(..)
  , Sign(..)
  , IntSize(..)
  , FloatSize(..)
  ) where

import HsBindgen.Macro.Ref

{-------------------------------------------------------------------------------
  Type expressions
-------------------------------------------------------------------------------}

-- | A type expression as it appears in a C type macro body.
--
-- Resolution to concrete @C.Type@ happens on the @hs-bindgen@ side.
data Expr =
    Lit     TypeLit
  | Ref     Ref
    -- | Pointer type (@T *@).
  | Pointer Expr
    -- | Const-qualified type (@const T@).
  | Const   Expr
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Primitive type literals
-------------------------------------------------------------------------------}

-- | Primitive C type literals as they appear in macro definitions.
--
-- Both sign and size can be omitted in C source:
--
--   * @signed@ alone means @signed int@
--   * @unsigned@ alone means @unsigned int@
--   * @char@ without sign specifier has implementation-defined signedness
--
-- Resolution to concrete types happens on the @hs-bindgen@ side.
data TypeLit =
    Int  (Maybe Sign) (Maybe IntSize)
  | Char (Maybe Sign)
  | Float FloatSize
  | Void
  | Bool
  deriving stock (Show, Eq)

data Sign = Signed | Unsigned
  deriving stock (Show, Eq)

data IntSize = SizeShort | SizeInt | SizeLong | SizeLongLong
  deriving stock (Show, Eq)

data FloatSize = SizeFloat | SizeDouble
  deriving stock (Show, Eq)
