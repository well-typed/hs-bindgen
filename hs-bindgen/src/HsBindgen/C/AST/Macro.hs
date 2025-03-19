-- | C macros of a certain shape
--
-- This is re-exported in "HsBindgen.C.AST".
module HsBindgen.C.AST.Macro (
    -- * Definition
    Macro(..)
    -- ** Expressions
  , MExpr(..)
  , MFun(..)
  , MTerm(..)
    -- ** Attributes
  , Attribute(..)
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

import HsBindgen.C.AST.Name
import HsBindgen.C.AST.Literal
import HsBindgen.C.AST.Type
import HsBindgen.Clang.HighLevel.Types
import HsBindgen.Clang.Paths
import HsBindgen.Util.TestEquality
  ( equals1 )

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

data Macro = Macro {
      macroLoc  :: MultiLoc
    , macroName :: CName
    , macroArgs :: [CName]
    , macroBody :: MExpr
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Expressions
-------------------------------------------------------------------------------}

-- | Body of a function-like macro
data MExpr =
    MTerm MTerm
  | -- | Empty
    MEmpty
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

    -- | Type declaration
  | MType Type

    -- | Attribute
  | MAttr Attribute (Maybe MTerm)

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
  Attributes
-------------------------------------------------------------------------------}

-- | Attribute
--
-- See Section 5.25, "Attribute syntax" of the gcc manual.
-- <https://gcc.gnu.org/onlinedocs/gcc-4.1.2/gcc/Attribute-Syntax.html#Attribute-Syntax>.
--
-- For now we make no attempt to parse what's actually inside the attribute.
data Attribute = Attribute [Token TokenSpelling]
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
          MEmpty
            -> True
          MTerm ( MInt IntegerLiteral { integerLiteralValue = 1 } )
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

