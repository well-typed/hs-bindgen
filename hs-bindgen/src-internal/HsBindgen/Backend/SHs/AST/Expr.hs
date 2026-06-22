{-# LANGUAGE MagicHash #-}

-- | Expression and binding types for the simplified Haskell AST
--
-- These types have no dependency on 'HsBindgen.Backend.Hs.Origin' and can
-- therefore be imported from 'HsBindgen.Macro.Interface' without creating a
-- module cycle.
--
-- Intended for qualified import.
module HsBindgen.Backend.SHs.AST.Expr (
    -- * Expressions
    ClosedExpr
  , SExpr(..)
  , eBindgenGlobal
  , eInt
  , InfixOp(..)
  , infixOpGlobal
  , SAlt(..)
  , PatExpr(..)
    -- * Records
  , FBind (..)
    -- * Bindings
  , Binding(..)
  , Parameter(..)
  , Result(..)
  , Pragma(..)
  ) where

import Data.ByteString (ByteString)
import Data.Type.Nat (Nat1)
import DeBruijn (Add, Ctx, EmptyCtx, Idx)
import Foreign.C (CChar)

import HsBindgen.Backend.Global
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Level
import HsBindgen.Backend.SHs.AST.Type
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.NameHint

{-------------------------------------------------------------------------------
  Expressions
-------------------------------------------------------------------------------}

type ClosedExpr = SExpr EmptyCtx

-- | Simple expressions
type SExpr :: Ctx -> Star
data SExpr ctx =
    EGlobal (Global LvlTerm)
  | EBound (Idx ctx)
  | EFree Hs.TermName
  | ECon (Hs.Name Hs.NsConstr)
  | EUnboxedIntegral Integer
  | EIntegral Integer (Maybe ClosedType)
  | EFloat Float ClosedType
  | EDouble Double ClosedType
  | ECChar CChar
  | EString String
  | ECString ByteString
    -- ^ C string literal, represented as its execution-encoding bytes.
    --
    -- The bytes are bit-for-bit identical to what a C compiler would embed for
    -- this literal. This is required because the generated binding may be
    -- passed directly to a C function that expects the same byte sequence.
  | EApp (SExpr ctx) (SExpr ctx)
  | EInfix InfixOp (SExpr ctx) (SExpr ctx)
  | ELam NameHint (SExpr (S ctx))
  | EUnusedLam (SExpr ctx)
  | ECase (SExpr ctx) [SAlt ctx]
  | EUnit
  | EBoxedTup Plus2
  | EUnboxedTup Plus2
  | EList [SExpr ctx]
  | ETypeApp (SExpr ctx) ClosedType
    -- | Record construction
  | ERecCon (Hs.Name Hs.NsConstr) [FBind ctx]
  deriving stock (Show)

eBindgenGlobal :: BindgenGlobalTerm -> SExpr ctx
eBindgenGlobal = EGlobal . bindgenGlobalTerm

-- | Pattern&Expressions
type PatExpr :: Star
data PatExpr
  = PEApps (Hs.Name Hs.NsConstr) [PatExpr]
  | PELit Integer
  deriving stock (Show)

eInt :: Int -> SExpr be
eInt i = EIntegral (fromIntegral i) (Just $ tBindgenGlobal Int_type)

-- | Supported infix operators.
data InfixOp =
    InfixApplicative_seq
  | InfixMonad_seq
  | InfixNonEmpty_constructor
  deriving stock (Show, Eq)

infixOpGlobal :: InfixOp -> Global LvlTerm
infixOpGlobal = bindgenGlobalTerm . \case
  InfixApplicative_seq      -> Applicative_seq
  InfixMonad_seq            -> Monad_seq
  InfixNonEmpty_constructor -> NonEmpty_constructor

-- | Case alternatives
data SAlt ctx where
    SAlt
      :: Hs.Name Hs.NsConstr -> Add n ctx ctx' -> Vec n NameHint -> SExpr ctx' -> SAlt ctx
    SAltNoConstr
      :: Vec Nat1 NameHint -> SExpr (S ctx) -> SAlt ctx
    SAltUnboxedTuple
      :: Add n ctx ctx' -> Vec n NameHint -> SExpr ctx' -> SAlt ctx

deriving stock instance Show (SAlt ctx)

{-------------------------------------------------------------------------------
  Records
-------------------------------------------------------------------------------}

-- | Field binding
--
-- Field bindings are used to for record construction and record updates
--
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-540003.15.3>
data FBind ctx = FBind {
    label :: String
  , expr :: SExpr ctx
  }
  deriving stock Show

{-------------------------------------------------------------------------------
  Bindings
-------------------------------------------------------------------------------}

data Pragma = NOINLINE
  deriving stock Show

data Binding = Binding{
      name       :: Hs.TermName
    , parameters :: [Parameter]
    , result     :: Result
    , body       :: ClosedExpr
    , pragmas    :: [Pragma]
    , comment    :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data Parameter = Parameter{
      typ     :: ClosedType
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data Result = Result{
      typ     :: ClosedType
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)
