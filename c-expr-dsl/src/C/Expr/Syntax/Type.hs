-- | AST for C types as they appear in macro definitions
--
-- This covers the minimum amount of C type syntax needed for hs-bindgen:
-- primitive types with sign/size specifiers and references to named types
-- (typedefs / other macro types). Const qualifiers and pointer indirection
-- are represented as 'TyApp' nodes in the expression tree using 'Const'
-- and 'Pointer'.
module C.Expr.Syntax.Type (
    TypeLit(..)
  , TagKind(..)
  , Sign(..)
  , IntSize(..)
  , FloatSize(..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | A C type literal as it appears in a macro definition body.
--
-- This is the base type, without const qualifiers or pointer indirections.
-- Those are represented by 'TyApp' nodes wrapping this term in the
-- expression tree.
--
-- Examples:
--
-- > int           => TypeInt Nothing (Just SizeInt)
-- > unsigned long => TypeInt (Just Unsigned) (Just SizeLong)
-- > struct Foo    => TypeTagged TagStruct "Foo"
--
-- Named types (typedefs, type macros) are not represented here; a bare
-- identifier is parsed as 'Var' in the expression layer and the
-- typechecker decides whether it denotes a type or a value.
data TypeLit =
    -- | An integral type: @[signed|unsigned] [short|int|long|long long]@
    --
    -- Both sign and size can be omitted:
    --
    --   * @signed@ alone means @signed int@
    --   * @unsigned@ alone means @unsigned int@
    --   * @short@ alone means @signed short int@
    --   * etc.
    TypeInt !(Maybe Sign) !(Maybe IntSize)

    -- | @[signed|unsigned] char@
  | TypeChar !(Maybe Sign)

    -- | A floating-point type: @float@ or @double@
  | TypeFloat !FloatSize

    -- | @void@
  | TypeVoid

    -- | @_Bool@ or @bool@ (C23)
  | TypeBool

    -- NB: We do not accept untagged identifiers here, since they may correspond
    -- to a value expression, not a type expression.

    -- | An elaborated type literal: @struct tag@, @union tag@, @enum tag@
  | TypeTagged !TagKind !Text
  deriving stock (Eq, Ord, Show, Generic)

-- | Tag kind for elaborated types
data TagKind = TagStruct | TagUnion | TagEnum
  deriving stock (Eq, Ord, Show, Generic)

data Sign = Signed | Unsigned
  deriving stock (Eq, Ord, Show, Generic)

data IntSize =
    SizeShort      -- ^ @short [int]@
  | SizeInt        -- ^ @int@
  | SizeLong       -- ^ @long [int]@
  | SizeLongLong   -- ^ @long long [int]@
  deriving stock (Eq, Ord, Show, Generic)

data FloatSize =
    SizeFloat    -- ^ @float@
  | SizeDouble   -- ^ @double@
  deriving stock (Eq, Ord, Show, Generic)
