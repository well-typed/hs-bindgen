-- | Value expressions and quantified types for C value macros.
--
-- Intended import:
--
-- > import HsBindgen.Macro.Value qualified as Value
module HsBindgen.Macro.Value (
    -- * Value expressions
    Expr(..)
    -- * Literals
  , Literal(..)
  , IntegerLit(..)
  , FloatingLit(..)
    -- * Operators
  , Op(..)
    -- * Quantified types
  , Type(..)
  , Constraint(..)
  , HsType(..)
  , DataCon(..)
  , ClassCon(..)
  , FamilyCon(..)
    -- * Typecheck result
  , Result(..)
    -- * Utilities
  , canBeRepresentedAsRational
  ) where

import C.Char (CharValue)
import C.Type (FloatingType, IntLikeType, IntegralType)

import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Value expressions
-------------------------------------------------------------------------------}

-- | A typechecked value expression from a C macro body.
data Expr var =
    Lit Literal
  | Var var [Expr var]
    -- | Application of a built-in C operator.
    --
    -- The length of the argument list must match the operator's arity
    -- (see 'Op').
  | OpApp Op [Expr var]
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

{-------------------------------------------------------------------------------
  Literals
-------------------------------------------------------------------------------}

data Literal =
    IntLit    IntegerLit
  | FloatLit  FloatingLit
  | CharLit   CharValue
  | StringLit [CharValue]
  deriving stock (Show, Eq)

data IntegerLit = IntegerLit {
    value :: Integer
    -- | From @c-expr-runtime@.
  , typ   :: IntLikeType
  }
  deriving stock (Show, Eq)

data FloatingLit =
    -- | C @float@ (e.g. @1.0f@).
    FloatValue  Float
  | -- | C @double@ (e.g. @1.0@).
    DoubleValue Double
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Operators
-------------------------------------------------------------------------------}

-- | Built-in C operators. Each maps 1:1 to a @c-expr-runtime@ type class
-- method.
--
-- Arity: unary operators take 1 argument, binary operators take 2,
-- @Tuple n@ takes @n@ arguments.
data Op =
    -- Unary (arity 1)
    UnaryPlus | UnaryMinus | LogicalNot | BitwiseNot
    -- Binary (arity 2)
  | Mult | Div | Rem | Add | Sub
  | ShiftLeft | ShiftRight
  | CmpLT | CmpLE | CmpGT | CmpGE | CmpEQ | CmpNE
  | BitwiseAnd | BitwiseXor | BitwiseOr
  | LogicalAnd | LogicalOr
    -- | N-ary tuple (arity @n@, @n >= 2@).
  | Tuple Natural
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Quantified types
-------------------------------------------------------------------------------}

-- | Quantified type for a macro value expression.
--
-- Example: @forall a b. (Add a b, IntLike a, IntLike b) => a -> b -> AddRes a b@
data Type = Type {
    vars        :: [Text]
  , constraints :: [Constraint]
  , body        :: HsType
  }
  deriving stock (Show, Eq)

data Constraint =
    ClassCt ClassCon [HsType]
    -- | Nominal equality (@~@).
  | EqCt    HsType   HsType
  deriving stock (Show, Eq)

data HsType =
    -- | Type variable.
    TyVar  Text
    -- | Function type.
  | Fun    (NonEmpty HsType) HsType
    -- | Data type application.
  | App    DataCon   [HsType]
    -- | Type family application.
  | FamApp FamilyCon [HsType]
  deriving stock (Show, Eq)

-- | Data type constructors appearing in macro types.
--
-- Note: The \"type of types\" kind is intentionally absent. Type macros produce
-- 'Type.Expr', not 'Value.Result', so types can never appear in a value macro's
-- quantified type.
--
-- Prefixed with @Dc@ to avoid constructor clashes with 'Op' (@Tuple@) and
-- @Data.Void@ (@Void@).
data DataCon =
    DcVoid
  | DcPtr
  | DcCharLit
  | DcIntLike
  | DcFloatLike
    -- | Boxed tuple (@n >= 2@).
  | DcTuple     Natural
    -- | From @c-expr-runtime@.
  | DcPrimInt   IntegralType
    -- | From @c-expr-runtime@.
  | DcPrimFloat FloatingType
  deriving stock (Show, Eq)

-- | Type class constructors for macro constraints.
--
-- Prefixed with @Cls@ to avoid constructor clashes with 'Op'.
data ClassCon =
    ClsNot | ClsLogical | ClsRelEq | ClsRelOrd
  | ClsPlus | ClsMinus | ClsAdd | ClsSub | ClsMult | ClsDiv | ClsRem
  | ClsComplement | ClsBitwise | ClsShift
  deriving stock (Show, Eq)

-- | Type family constructors (result types of C operations).
data FamilyCon =
    PlusRes | MinusRes
  | AddRes | SubRes | MultRes | DivRes | RemRes
  | ComplementRes | BitsRes | ShiftRes
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Typecheck result
-------------------------------------------------------------------------------}

-- | Typecheck result for a value macro.
data Result = Result {
    -- | Macro argument names (in order).
    args :: [Text]
    -- | Typechecked expression body.
  , body :: Expr Text
    -- | Quantified Haskell type.
  , typ  :: Type
  }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

{-# SPECIALISE canBeRepresentedAsRational :: Float  -> Bool #-}
{-# SPECIALISE canBeRepresentedAsRational :: Double -> Bool #-}

-- | Can this floating-point value be represented as a 'Rational' without
-- loss of information?
canBeRepresentedAsRational :: RealFloat a => a -> Bool
canBeRepresentedAsRational f = not $ or [
      isNaN f
    , isInfinite f
    , isNegativeZero f
      -- Not strictly necessary, but let's be conservative
    , isDenormalized f
    ]
