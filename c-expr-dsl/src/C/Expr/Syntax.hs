{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >=908
{-# LANGUAGE TypeAbstractions #-}
#endif

-- | The syntax for macros recognized by hs-bindgen
--
-- Intended for unqualified import.
module C.Expr.Syntax (
    -- * Definition
    Macro(..)
  , sameMacro
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
import Data.Type.Equality ((:~:)(..))
import Data.Vec.Lazy (Vec, withDict)
import DeBruijn (Ctx)

import C.Expr.Syntax.Expr
import C.Expr.Syntax.Literals
import C.Expr.Syntax.Name
import C.Expr.Syntax.TTG
import C.Expr.Syntax.TTG.Parse
import C.Expr.Syntax.Type

import Clang.HighLevel.Types
import Data.Type.Nat qualified as Nat

type Macro :: Hs.Type
data Macro = forall (ctx :: Ctx). Macro {
      macroLoc    :: MultiLoc
    , macroName   :: Name
    , macroParams :: Vec ctx Name
    , macroExpr   :: Expr ctx Ps
    }

instance Eq Macro where
  m1 == m2 = macroLoc m1 == macroLoc m2 && sameMacro m1 m2

deriving stock instance Show Macro

-- | Are two macros referring to the same macro.
--
-- The location and parameter names do not need to match. Everything else must
-- be equal.
sameMacro :: Macro -> Macro -> Bool
sameMacro (Macro @c1 _ n1 p1 e1) (Macro @c2 _ n2 p2 e2) =
         n1 == n2
      && ( withDict p1 $ withDict p2 $
           case Nat.eqNat @c1 @c2 of
             Just Refl -> p1 == p2 && e1 == e2
             Nothing   -> False
         )
