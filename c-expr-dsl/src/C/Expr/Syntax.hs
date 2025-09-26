-- | The syntax for macros recognized by hs-bindgen
--
-- Intended for unqualified import.
module C.Expr.Syntax (
    -- * Definition
    Macro(..)
    -- ** Expressions
  , MExpr(..)
  , MFun(..)
  , MTerm(..)
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

import Clang.HighLevel.Types

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

type Macro :: Pass -> Hs.Type
data Macro p = Macro {
      macroLoc  :: MultiLoc
    , macroName :: Name      -- TODO: This should be polymorphic in ID
    , macroArgs :: [Name]
    , macroBody :: MExpr p
    }
  deriving stock Generic
deriving stock instance ( Eq ( XApp p ), Eq ( XVar p ) ) => Eq ( Macro p )
deriving stock instance ( Show ( XApp p ), Show ( XVar p ) ) => Show ( Macro p )
