-- | C macros of a certain shape
--
-- This is re-exported in "HsBindgen.C.AST".
module HsBindgen.C.AST.Macro (
    -- * Definition
    Macro(..)
  , MacroBody(..)
    -- ** Expressions
  , MExpr(..)
  , MFun(..)
  , MTerm(..)
    -- * Classification
  , isIncludeGuard
  ) where

import Data.Char (toUpper)
import Data.GADT.Compare (GEq(geq))
import Data.Nat (Nat(..))
import Data.Vec.Lazy (Vec(..))
import Data.String
import Data.Type.Equality
  ( type (:~:)(..) )
import Data.Type.Nat (SNatI)
import GHC.Generics (Generic)
import System.FilePath (takeBaseName)

import Clang.HighLevel.Types
import Clang.Paths
import HsBindgen.C.AST.Name
import HsBindgen.C.AST.Literal
import HsBindgen.Util.TestEquality
  ( equals1 )

import {-# SOURCE #-} HsBindgen.C.Reparse.Decl
import Data.Type.Nat qualified as Nat

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

data Macro = Macro {
      macroLoc  :: MultiLoc
    , macroName :: CName
    , macroArgs :: [CName]
    , macroBody :: MacroBody
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Expressions
-------------------------------------------------------------------------------}

-- | Body of a function-like macro
data MacroBody
  -- | Empty macro body
  = EmptyMacro
  -- | A term-level (expression) macro
  --
  -- NB: this may be an integer expression, which
  -- we can use at the type level as well (e.g. in the size of an array)
  | ExpressionMacro MExpr
  -- | A macro that defines a type.
  --
  -- See Note [Macros defining types]
  | TypeMacro TypeName
  -- | A macro that defines attributes
  | AttributeMacro [AttributeSpecifier]
  deriving stock ( Eq, Show )

{- Note [Macros defining types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to support macros that define types (see e.g. #401), such as:

  #define Ty1 int
  #define Ty2 int*
  #define Ty3 int[8]

But... what precisely **are** 'int', 'int*' and 'int[8]'? According to the C23
specification, these are "type names", which consist of:

  1. a type specifier (here 'int' in all three cases)
  2. some attributes (none in these examples)
  3. an abstract declarator

These can appear in several places in the C grammar, such as:

  - in casts, e.g. (ty)(expr)
  - as the argument to sizeof, e.g. sizeof(ty)

We thus reparse these three examples as "type names", which has a low
implementation cost as we already have a (re)parser for such things for the
purpose of reparsing function parameters and struct field declarations.

As a consequence, we also reparse the following:

  #define Ty4 int(float)     // a function that takes a float and returns an int
  #define Ty5 int (*)(float) // a function pointer to a function taking a float and returning an int

One downside is that we can't straightforwardly re-use these type names in
other places, such as in function parameter types:

  void foo(int x1, int* x2, int x3[8], int (*x5)(float))

Function parameters are non-abstract declarators, and for more complicated
types the declarator name appears "in the middle" of the type, e.g.

  int x3[8]         // 'x3' appears in the middle of 'int[8]'
  int (*x5)(float)  // 'x5' appears in the middle of 'int (*)(float)'

For the time being, we accept macros defining such type names.
-}

-- | Macro expression
data MExpr
  -- | A term that is not a function application.
  = MTerm MTerm
  -- | Exactly saturated non-nullary function application.
  | forall n. MApp ( MFun ( S n ) ) ( Vec ( S n ) MExpr )
deriving stock instance Show MExpr

instance Eq MExpr where
  MTerm m1 == MTerm m2 = m1 == m2
  MApp f1 args1 == MApp f2 args2
    | Just Refl <- f1 `equals1` f2
    = args1 == args2
    | otherwise
    = False
  _ == _ = False

data MFun arity where
  -- | @+@
  MUnaryPlus  :: MFun ( S Z )
  -- | @-@
  MUnaryMinus :: MFun ( S Z )
  -- | @!@
  MLogicalNot :: MFun ( S Z )
  -- | @~@
  MBitwiseNot :: MFun ( S Z )
  -- | @*@
  MMult       :: MFun ( S ( S Z ) )
  -- | @/@
  MDiv        :: MFun ( S ( S Z ) )
  -- | @%@
  MRem        :: MFun ( S ( S Z ) )
  -- | @+@
  MAdd        :: MFun ( S ( S Z ) )
  -- | @-@
  MSub        :: MFun ( S ( S Z ) )
  -- | @<<@
  MShiftLeft  :: MFun ( S ( S Z ) )
  -- | @>>@
  MShiftRight :: MFun ( S ( S Z ) )
  -- | @<@
  MRelLT      :: MFun ( S ( S Z ) )
  -- | @<=@
  MRelLE      :: MFun ( S ( S Z ) )
  -- | @>@
  MRelGT      :: MFun ( S ( S Z ) )
  -- | @>=@
  MRelGE      :: MFun ( S ( S Z ) )
  -- | @==@
  MRelEQ      :: MFun ( S ( S Z ) )
  -- | @!=@
  MRelNE      :: MFun ( S ( S Z ) )
  -- | @&@
  MBitwiseAnd :: MFun ( S ( S Z ) )
  -- | @^@
  MBitwiseXor :: MFun ( S ( S Z ) )
  -- | @|@
  MBitwiseOr  :: MFun ( S ( S Z ) )
  -- | @&&@
  MLogicalAnd :: MFun ( S ( S Z ) )
  -- | @||@
  MLogicalOr  :: MFun ( S ( S Z ) )
  -- | Tuples
  MTuple      :: SNatI n => MFun ( S ( S n ) )

  -- NB: make sure to update 'instance GEq MFun'
  -- when adding a new constructor.

deriving stock instance Show ( MFun arity )
deriving stock instance Eq   ( MFun arity )

instance GEq MFun where
  geq MUnaryPlus  MUnaryPlus  = Just Refl
  geq MUnaryMinus MUnaryMinus = Just Refl
  geq MLogicalNot MLogicalNot = Just Refl
  geq MBitwiseNot MBitwiseNot = Just Refl
  geq MMult       MMult       = Just Refl
  geq MDiv        MDiv        = Just Refl
  geq MRem        MRem        = Just Refl
  geq MAdd        MAdd        = Just Refl
  geq MSub        MSub        = Just Refl
  geq MShiftLeft  MShiftLeft  = Just Refl
  geq MShiftRight MShiftRight = Just Refl
  geq MRelLT      MRelLT      = Just Refl
  geq MRelLE      MRelLE      = Just Refl
  geq MRelGT      MRelGT      = Just Refl
  geq MRelGE      MRelGE      = Just Refl
  geq MRelEQ      MRelEQ      = Just Refl
  geq MRelNE      MRelNE      = Just Refl
  geq MBitwiseAnd MBitwiseAnd = Just Refl
  geq MBitwiseXor MBitwiseXor = Just Refl
  geq MBitwiseOr  MBitwiseOr  = Just Refl
  geq MLogicalAnd MLogicalAnd = Just Refl
  geq MLogicalOr  MLogicalOr  = Just Refl
  geq (MTuple @i) (MTuple @j)
    | Just Refl <- Nat.eqNat @i @j
    = Just Refl
  geq _           _           = Nothing

data MTerm =

    -- | Integer literal
    MInt IntegerLiteral

    -- | Floating-point literal
  | MFloat FloatingLiteral

    -- | Character literal
  | MChar CharLiteral

    -- | String literal
  | MString StringLiteral

    -- | Variable or function/macro call
    --
    -- This might be a macro argument, or another marco.
  | MVar CName [MExpr]

    -- | Stringizing
    --
    -- See
    --
    -- * Section 6.10.3.2, "The # operator" of the spec
    -- * <https://gcc.gnu.org/onlinedocs/cpp/Stringizing.html>
  | MStringize CName

    -- | Concatenation
    --
    -- See
    --
    -- * Section 6.10.3.3, "The ## operator" of the spec
    -- * <https://gcc.gnu.org/onlinedocs/cpp/Concatenation.html>
  | MConcat MTerm MTerm
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Classification
-------------------------------------------------------------------------------}

isIncludeGuard :: Macro -> Bool
isIncludeGuard Macro{macroLoc, macroName, macroArgs, macroBody} =
    and [
        macroName `elem` includeGuards
      , null macroArgs
      , case macroBody of
          EmptyMacro
            -> True
          ExpressionMacro
            (MTerm (MInt IntegerLiteral { integerLiteralValue = 1 }))
            -> True
          _otherwise
            -> False
      ]
  where
    sourcePath :: FilePath
    sourcePath = getSourcePath . singleLocPath $ multiLocExpansion macroLoc

    includeGuards :: [CName]
    includeGuards = possibleIncludeGuards (takeBaseName sourcePath)

    -- | Possible names for include guards, given the file (base) name
    possibleIncludeGuards :: String -> [CName]
    possibleIncludeGuards baseName = map fromString $ [
                 map toUpper baseName ++ "_H"
        , "_" ++ map toUpper baseName ++ "_H" -- this would be a reserved name
        ,        map toUpper baseName ++ "_INCLUDED"
        ]

