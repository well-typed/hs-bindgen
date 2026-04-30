-- | The syntax for macros recognized by hs-bindgen
--
-- Intended for unqualified import.
module C.Expr.Syntax (
    -- * Definition
    Macro(..)
    -- ** Type syntax
  , TypeLit(..)
  , TagKind(..)
  , Sign(..)
  , IntSize(..)
  , FloatSize(..)
    -- ** Expressions
  , Expr(..)
  , TyFun(..)
  , VaFun(..)
  , ValueLit(..)
  , Literal(..)
  , Term(..)
  , Name(..)
    -- ** Literals
  , IntegerLiteral(..)
  , FloatingLiteral(..)
  , CharLiteral(..)
  , StringLiteral(..)
  , canBeRepresentedAsRational
    -- ** Annotations
  , Pass
  , Ps
  , XVar(..)
  , XApp(..)
  ) where

import Data.Kind qualified as Hs
import GHC.Generics (Generic)

import C.Expr.Syntax.Expr
import C.Expr.Syntax.Literals
import C.Expr.Syntax.Name
import C.Expr.Syntax.TTG
import C.Expr.Syntax.TTG.Parse
import C.Expr.Syntax.Type

import Clang.HighLevel.Types

type Macro :: Hs.Type
data Macro = Macro {
      macroLoc  :: MultiLoc
    , macroName :: Name
      -- TODO-D: macroParams
    , macroArgs :: [Name]
    , macroExpr :: Expr Ps
    }
  deriving stock (Eq, Show, Generic)
