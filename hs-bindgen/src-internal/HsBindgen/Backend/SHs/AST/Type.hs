{-# LANGUAGE MagicHash #-}

-- | Simplified HS abstract syntax tree for types
--
-- TODO <https://github.com/well-typed/hs-bindgen/issues/1761>
-- We should mark this module as intended for qualified import and avoid
-- unnecessarily prefixes.
module HsBindgen.Backend.SHs.AST.Type (
    ClosedType
  , SType (..)
    -- * Plus2
  , Plus2 (..)
  , applyPlus2
  ) where

import DeBruijn (Add, Ctx, EmptyCtx, Idx)

import HsBindgen.Backend.Global
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Level
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.NameHint

{-------------------------------------------------------------------------------
  Type
-------------------------------------------------------------------------------}

type ClosedType = SType EmptyCtx

-- | Simple types
type SType :: Ctx -> Star
data SType ctx =
    TGlobal (Global LvlType)
  | TClass Inst.TypeClass
  | TCon (Hs.Name Hs.NsTypeConstr)
  | TFun (SType ctx) (SType ctx)
  | TLit Natural
  | TStrLit String
  | TExt Hs.ExtRef BindingSpec.CTypeSpec BindingSpec.HsTypeSpec
  | TBound (Idx ctx)
  | TFree (Hs.Name Hs.NsVar)
  | TApp (SType ctx) (SType ctx)
  | TUnit
  | TBoxedTup Plus2
  | TEq
  | forall n ctx'. TForall (Vec n NameHint) (Add n ctx ctx') [SType ctx'] (SType ctx')

infixl 9 `TApp`

deriving stock instance Show (SType ctx)

{-------------------------------------------------------------------------------
  Plus2
-------------------------------------------------------------------------------}

-- | Natural number larger equal 2.
newtype Plus2 = Plus2 Natural
  deriving stock (Show, Eq, Ord)

applyPlus2 :: Plus2 -> Natural
applyPlus2 (Plus2 n) = n+2
