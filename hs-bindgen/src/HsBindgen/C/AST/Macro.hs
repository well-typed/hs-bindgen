-- | C macros of a certain shape
--
-- This is re-exported in "HsBindgen.C.AST".
module HsBindgen.C.AST.Macro (
    -- * Definition
    Macro(..)
    -- ** Expressions
  , MExpr(..)
  , MTerm(..)
    -- ** Attributes
  , Attribute(..)
    -- * Classification
  , isIncludeGuard
  ) where

import Data.Char (toUpper)
import Data.String
import Data.Text qualified as Text
import GHC.Generics (Generic)
import System.FilePath (takeBaseName)
import Text.Show.Pretty (PrettyVal)

import HsBindgen.C.AST.Literal
import HsBindgen.C.AST.Name
import HsBindgen.C.AST.Type
import HsBindgen.Clang.HighLevel.Types

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
  | MUnaryPlus MExpr        -- ^ @+@
  | MUnaryMinus MExpr       -- ^ @-@
  | MLogicalNot MExpr       -- ^ @!@
  | MBitwiseNot MExpr       -- ^ @~@
  | MMult MExpr MExpr        -- ^ @*@
  | MDiv MExpr MExpr         -- ^ @/@
  | MRem MExpr MExpr         -- ^ @%@
  | MAdd MExpr MExpr         -- ^ @+@
  | MSub MExpr MExpr         -- ^ @-@
  | MShiftLeft MExpr MExpr   -- ^ @<<@
  | MShiftRight MExpr MExpr  -- ^ @>>@
  | MRelLT MExpr MExpr       -- ^ @<@
  | MRelLE MExpr MExpr       -- ^ @<=@
  | MRelGT MExpr MExpr       -- ^ @>@
  | MRelGE MExpr MExpr       -- ^ @>=@
  | MRelEQ MExpr MExpr       -- ^ @==@
  | MRelNE MExpr MExpr       -- ^ @!=@
  | MBitwiseAnd MExpr MExpr  -- ^ @&@
  | MBitwiseXor MExpr MExpr  -- ^ @^@
  | MBitwiseOr MExpr MExpr   -- ^ @|@
  | MLogicalAnd MExpr MExpr  -- ^ @&&@
  | MLogicalOr MExpr MExpr   -- ^ @||@
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

data MTerm =
    -- | Empty
    MEmpty

    -- | Integer literal
  | MInt (Literal Integer)

    -- | Floating point literal
  | MFloat Double

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
          MTerm MEmpty                         -> True
          MTerm (MInt i) | literalValue i == 1 -> True
          _otherwise                           -> False
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

