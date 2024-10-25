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
import Data.Nat (Nat(..))
import Data.Vec.Lazy (Vec(..))
import Data.String
import Data.Text qualified as Text
import Data.Type.Equality
  ( type (:~:)(..) )
import GHC.Generics (Generic)
import System.FilePath (takeBaseName)
import Text.Show.Pretty (PrettyVal(..))
import Text.Show.Pretty qualified as Pretty

import HsBindgen.C.AST.Name
import HsBindgen.C.AST.Literal
import HsBindgen.C.AST.Type
import HsBindgen.Clang.HighLevel.Types
import HsBindgen.Pretty.Orphans
  ()
import HsBindgen.Util.TestEquality
  ( equals )

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
  deriving anyclass (PrettyVal)

{-------------------------------------------------------------------------------
  Expressions
-------------------------------------------------------------------------------}

-- | Body of a function-like macro
data MExpr =
    MTerm MTerm
  -- | Exactly saturated non-nullary function application.
  | forall n. MApp ( MFun ( S n ) ) ( Vec ( S n ) MExpr )
deriving stock instance Show MExpr
instance PrettyVal MExpr where
  prettyVal = \case
    MTerm tm    -> Pretty.Con "MTerm" [prettyVal tm]
    MApp f args -> Pretty.Con "MApp" [prettyVal f, prettyVal args]

instance Eq MExpr where
  MTerm m1 == MTerm m2 = m1 == m2
  MApp f1 args1 == MApp f2 args2
    | Just Refl <- f1 `equals` f2
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

deriving stock instance Show ( MFun arity )
deriving stock instance Eq   ( MFun arity )

instance PrettyVal ( MFun arity ) where
  prettyVal f = Pretty.Con (show f) []

data MTerm =
    -- | Empty
    MEmpty

    -- | Integer literal
  | MInt IntegerLiteral

    -- | Floating-point literal
  | MFloat FloatingLiteral

    -- | Variable or function/macro call
    --
    -- This might be a macro argument, or another marco.
  | MVar CName [MExpr]

    -- | Type declaration
    --
    -- For now we only support primitive types.
  | MType PrimType

    -- | Attribute
  | MAttr Attribute MTerm

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
  deriving anyclass (PrettyVal)

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
  deriving anyclass (PrettyVal)

{-------------------------------------------------------------------------------
  Classification
-------------------------------------------------------------------------------}

isIncludeGuard :: Macro -> Bool
isIncludeGuard Macro{macroLoc, macroName, macroArgs, macroBody} =
    and [
        macroName `elem` includeGuards
      , null macroArgs
      , case macroBody of
          MTerm MEmpty
            -> True
          MTerm ( MInt IntegerLiteral { integerLiteralValue = 1 } )
            -> True
          _otherwise
            -> False
      ]
  where
    sourcePath :: FilePath
    sourcePath = Text.unpack . getSourcePath . singleLocPath $
                   multiLocExpansion macroLoc

    includeGuards :: [CName]
    includeGuards = possibleIncludeGuards (takeBaseName sourcePath)

    -- | Possible names for include guards, given the file (base) name
    possibleIncludeGuards :: String -> [CName]
    possibleIncludeGuards baseName = map fromString $ [
                 map toUpper baseName ++ "_H"
        , "_" ++ map toUpper baseName ++ "_H" -- this would be a reserved name
        ,        map toUpper baseName ++ "_INCLUDED"
        ]

