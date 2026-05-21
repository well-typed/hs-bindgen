{-# LANGUAGE MagicHash #-}

-- | Simplified HS abstract syntax tree
--
-- TODO <https://github.com/well-typed/hs-bindgen/issues/1761>
-- We should mark this module as intended for qualified import and avoid
-- unnecessarily prefixes.
module HsBindgen.Backend.SHs.AST (
    ClosedExpr
  , Plus2(..)
  , applyPlus2
  , SExpr(..)
  , eBindgenGlobal
  , eInt
  , InfixOp(..)
  , infixOpGlobal
  , SAlt(..)
  , PatExpr(..)
  , SDecl(..)
  , TypeSynonym(..)
  , Pragma(..)
  , ClosedType
  , SType(..)
  , tBindgenGlobal
  , Binding(..)
  , Instance(..)
  , Field(..)
  , Record(..)
  , EmptyData(..)
  , DerivingInstance(..)
  , Newtype(..)
  , ForeignImport(..)
  , Safety(..)
  , Parameter(..)
  , Result(..)
  , PatternSynonym(..)
  ) where

import HsBindgen.Backend.Global
import HsBindgen.Backend.Hs.AST.Strategy qualified as Hs
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Level
import HsBindgen.Backend.SHs.AST.Expr
import HsBindgen.Backend.SHs.AST.Type
import HsBindgen.Frontend.Naming
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Re-exports from SHs.AST.Expr and SHs.AST.Type
-------------------------------------------------------------------------------}

-- SExpr, ClosedExpr, SAlt, PatExpr, InfixOp, Binding, Parameter, Result,
-- Pragma are re-exported from SHs.AST.Expr (imported above).
-- SType, ClosedType, Plus2, applyPlus2, tBindgenGlobal are re-exported from
-- SHs.AST.Type (imported above).

{-------------------------------------------------------------------------------
  Backend representation
-------------------------------------------------------------------------------}

-- | Simple declarations
data SDecl =
    DTypSyn TypeSynonym
  | DInst Instance
  | DRecord Record
  | DNewtype Newtype
  | DEmptyData EmptyData
  | DDerivingInstance DerivingInstance
  | DForeignImport ForeignImport
  | DBinding Binding
  | DPatternSynonym PatternSynonym
  deriving stock (Show)

data TypeSynonym = TypeSynonym{
      name    :: Hs.Name Hs.NsTypeConstr
    , typ     :: ClosedType
    , origin  :: Origin.Decl Origin.EmptyData
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data Instance = Instance{
      clss    :: Inst.TypeClass
    , args    :: [ClosedType]
    , super   :: [ClosedType]
    , types   :: [(Global LvlType, [ClosedType], ClosedType)]
    , decs    :: [(Global LvlTerm, ClosedExpr)]
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data Field = Field{
      name   :: Hs.Name Hs.NsVar
    , typ    :: ClosedType
    , origin :: Origin.Field
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data Record = Record{
      typ     :: Hs.Name Hs.NsTypeConstr
    , con     :: Hs.Name Hs.NsConstr
    , fields  :: [Field]
    , origin  :: Origin.Decl Origin.Struct
    , deriv   :: [(Hs.Strategy ClosedType, [Inst.TypeClass])]
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data EmptyData = EmptyData{
      name    :: Hs.Name Hs.NsTypeConstr
    , origin  :: Origin.Decl Origin.EmptyData
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data DerivingInstance = DerivingInstance{
      strategy :: Hs.Strategy ClosedType
    , typ      :: ClosedType
    , comment  :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data Newtype = Newtype{
      name    :: Hs.Name Hs.NsTypeConstr
    , con     :: Hs.Name Hs.NsConstr
    , field   :: Field
    , origin  :: Origin.Decl Origin.Newtype
    , deriv   :: [(Hs.Strategy ClosedType, [Inst.TypeClass])]
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Generic, Show)

data ForeignImport = ForeignImport{
      name       :: Hs.TermName
    , parameters :: [Parameter]
    , result     :: Result
    , origName   :: CDeclName
    , callConv   :: CallConv
    , origin     :: Origin.ForeignImport
    , comment    :: Maybe HsDoc.Comment
    , safety     :: Safety
    }
  deriving stock (Show, Generic)

-- | Safety of foreign import declarations
data Safety = Safe | Unsafe
  deriving stock (Show, Eq, Generic)

instance Default Safety where
  def = Safe

data PatternSynonym = PatternSynonym{
      name    :: Hs.Name Hs.NsConstr
    , typ     :: ClosedType
    , rhs     :: PatExpr
    , origin  :: Origin.PatSyn
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)
