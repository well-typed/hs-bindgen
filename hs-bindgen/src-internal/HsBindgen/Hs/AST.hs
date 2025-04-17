{-# LANGUAGE MagicHash #-}

-- | Haskell AST
--
-- Abstract Haskell syntax for the specific purposes of hs-bindgen: we only
-- cover the parts of the Haskell syntax that we need. We attempt to do this in
-- such a way that the generated Haskell code is type correct by construction.
--
-- Intended for qualified import:
--
-- > import HsBindgen.Hs.AST qualified as Hs
module HsBindgen.Hs.AST (
    -- * Information about generated code
    Field(..)
  , FieldOrigin(..)
  , Struct(..)
  , StructOrigin(..)
  , EmptyData(..)
  , EmptyDataOrigin(..)
  , Newtype(..)
  , NewtypeOrigin(..)
    -- * Types
  , HsType(..)
    -- * Variable binding
  , Lambda(..)
  , Ap(..)
    -- * Declarations
  , Decl(..)
  , InstanceDecl(..)
    -- ** Variable declarations
  , VarDecl(..)
  , SigmaType(..)
  , PhiType(..)
  , TauType(..)
  , PredType(..)
  , ATyCon(..)
  , AClass(..)
  , VarDeclRHS(..)
  , VarDeclRHSAppHead(..)
    -- ** Deriving instances
  , Strategy(..)
  , TypeClass(..)
    -- ** Foreign imports
  , ForeignImportDecl(..)
  , ForeignImportDeclOrigin(..)
    -- ** 'Storable'
  , StorableInstance(..)
  , PeekByteOff(..)
  , PokeByteOff(..)
    -- ** Statements
  , Seq(..)
    -- ** Structs
  , StructCon (..)
  , ElimStruct(..)
  , makeElimStruct
    -- ** Pattern Synonyms
  , PatSyn(..)
  , PatSynOrigin(..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Type.Nat (SNat, SNatI, snat)
import Data.Type.Nat qualified as Nat

import HsBindgen.C.AST qualified as C
import HsBindgen.C.Tc.Macro qualified as C

import HsBindgen.Imports
import HsBindgen.NameHint
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type
import HsBindgen.Orphans ()
import HsBindgen.Util.TestEquality

import DeBruijn

import C.Char qualified as CExpr

{-------------------------------------------------------------------------------
  Information about generated code
-------------------------------------------------------------------------------}

data Field = Field {
      fieldName   :: HsName NsVar
    , fieldType   :: HsType
    , fieldOrigin :: FieldOrigin
    }
  deriving stock (Generic, Show)

data FieldOrigin =
      FieldOriginNone
    | FieldOriginStructField C.StructField
  deriving stock (Generic, Show)

data Struct (n :: Nat) = Struct {
      structName   :: HsName NsTypeConstr
    , structConstr :: HsName NsConstr
    , structFields :: Vec n Field
    , structOrigin :: StructOrigin
    }
  deriving stock (Generic, Show)

data StructOrigin =
      StructOriginStruct C.Struct
    | StructOriginEnum C.Enu
  deriving stock (Generic, Show)

data EmptyData = EmptyData {
      emptyDataName   :: HsName NsTypeConstr
    , emptyDataOrigin :: EmptyDataOrigin
    }
  deriving stock (Generic, Show)

data EmptyDataOrigin =
      EmptyDataOriginOpaqueStruct C.OpaqueStruct
    | EmptyDataOriginOpaqueEnum C.OpaqueEnum
  deriving stock (Generic, Show)

data Newtype = Newtype {
      newtypeName   :: HsName NsTypeConstr
    , newtypeConstr :: HsName NsConstr
    , newtypeField  :: Field
    , newtypeOrigin :: NewtypeOrigin
    }
  deriving stock (Generic, Show)

data NewtypeOrigin =
      NewtypeOriginEnum C.Enu
    | NewtypeOriginTypedef C.Typedef
    | NewtypeOriginUnion C.Union
    | NewtypeOriginMacro C.Macro
  deriving stock (Generic, Show)

data ForeignImportDecl = ForeignImportDecl
    { foreignImportName       :: HsName NsVar
    , foreignImportType       :: HsType
    , foreignImportOrigName   :: Text
    , foreignImportHeader     :: FilePath -- TODO: https://github.com/well-typed/hs-bindgen/issues/333
    , foreignImportDeclOrigin :: ForeignImportDeclOrigin
    }
  deriving stock (Generic, Show)

newtype ForeignImportDeclOrigin =
      ForeignImportDeclOriginFunction C.Function
  deriving stock (Generic, Show)

{-------------------------------------------------------------------------------
  Variable binding
-------------------------------------------------------------------------------}

-- | Lambda abstraction
type Lambda :: (Ctx -> Star) -> (Ctx -> Star)
data Lambda t ctx = Lambda
    NameHint  -- ^ name suggestion
    (t (S ctx)) -- ^ body

deriving instance Show (t (S ctx)) => Show (Lambda t ctx)

-- | Applicative structure

data Ap pure xs ctx = Ap (pure ctx) [xs ctx]
  deriving stock (Generic, Show)

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

-- | Top-level declaration
type Decl :: Star
data Decl where
    DeclData            :: SNatI n => Struct n -> Decl
    DeclEmpty           :: EmptyData -> Decl
    DeclNewtype         :: Newtype -> Decl
    DeclPatSyn          :: PatSyn -> Decl
    DeclDefineInstance  :: InstanceDecl -> Decl
    DeclDeriveInstance  :: Strategy HsType -> TypeClass -> HsName NsTypeConstr -> Decl
    DeclForeignImport   :: ForeignImportDecl -> Decl
    DeclVar             :: VarDecl -> Decl
    DeclUnionGetter     :: HsName NsTypeConstr -> HsType -> HsName NsVar -> Decl
    DeclUnionSetter     :: HsName NsTypeConstr -> HsType -> HsName NsVar -> Decl

deriving instance Show Decl

-- | Deriving strategy
data Strategy ty =
    DeriveNewtype
  | DeriveStock
  | DeriveVia ty
  deriving stock (Generic, Show, Functor, Foldable, Traversable)

-- | Class instance names (for instances that /ghc/ generates)
data TypeClass =
    Storable

    -- Haskell98 derivable classes
    -- <https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/deriving.html>
  | Eq
  | Ord
  | Enum
  | Ix
  | Bounded
  | Read
  | Show

    -- Classes we can only derive through newtype deriving
  | Bits
  | FiniteBits
  | Floating
  | Fractional
  | Integral
  | Num
  | Real
  | RealFloat
  | RealFrac
  deriving stock (Generic, Show, Eq, Ord)

-- | Class instance declaration (with code that /we/ generate)
type InstanceDecl :: Star
data InstanceDecl where
    InstanceStorable :: Struct n -> StorableInstance -> InstanceDecl
    InstanceHasFLAM :: Struct n -> HsType -> Int -> InstanceDecl
    InstanceCEnum ::
         Struct (S Z)
      -> HsType
      -> Map Integer (NonEmpty String)
      -> InstanceDecl
    InstanceSequentialCEnum ::
         Struct (S Z)
      -> HsName NsConstr
      -> HsName NsConstr
      -> InstanceDecl
    InstanceCEnumShow :: Struct (S Z) -> InstanceDecl

deriving instance Show InstanceDecl

-- | Variable or function declaration.
type VarDecl :: Star
data VarDecl =
  VarDecl
    -- | Name of variable/function.
    { varDeclName :: HsName NsVar
    -- | Type of variable/function.
    , varDeclType :: SigmaType
    -- | RHS of variable/function.
    , varDeclBody :: VarDeclRHS EmptyCtx
    }
  deriving stock (Generic, Show)

-- | A σ-type, of the form @forall tvs. ctxt => body@.
type SigmaType :: Star
data SigmaType where
  ForallTy ::
    { forallTyBinders :: Vec n NameHint
    , forallTy        :: PhiType n
    }
    -> SigmaType

deriving stock instance Show SigmaType

-- | A φ-type, of the form @ctxt => body@.
type PhiType :: Ctx -> Star
data PhiType ctx
  = QuantTy
  { quantTyCts  :: [PredType ctx]
  , quantTyBody :: TauType ctx
  }
  deriving stock (Generic, Show)

-- | A τ-type: no quantification or contexts (i.e. no @forall@, no @=>@ arrows).
type TauType :: Ctx -> Star
data TauType ctx
  = FunTy (TauType ctx) (TauType ctx)
  | TyVarTy (Idx ctx)
  | TyConAppTy ATyCon [TauType ctx]
  deriving stock (Eq, Generic, Show)

-- | A predicate/constraint τ-type.
type PredType :: Ctx -> Star
data PredType ctx
  = DictTy AClass [TauType ctx]
  | NomEqTy (TauType ctx) (TauType ctx)
  deriving stock Show

instance Eq (PredType ctx) where
  DictTy cls1 tys1 == DictTy cls2 tys2 =
    cls1 == cls2 && tys1 == tys2
  NomEqTy l1 r1 == NomEqTy l2 r2 =
    l1 == l2 && r1 == r2
  _ == _ = False

data ATyCon where
  ATyCon :: C.TyCon args C.Ty -> ATyCon
instance Show ATyCon where
  show ( ATyCon tc ) = show tc
instance Eq ATyCon where
  ATyCon tc1 == ATyCon tc2 =
    isJust $ equals2 tc1 tc2

data AClass where
  AClass :: C.TyCon args C.Ct -> AClass
instance Show AClass where
  show ( AClass tc ) = show tc
instance Eq AClass where
  AClass ( C.GenerativeTyCon ( C.ClassTyCon cls1 ) )
    ==
    AClass ( C.GenerativeTyCon ( C.ClassTyCon cls2 ) ) =
      isJust $ equals1 cls1 cls2

-- | RHS of a variable or function declaration.
type VarDeclRHS :: Ctx -> Star
data VarDeclRHS ctx
  = VarDeclIntegral Integer HsPrimType
  | VarDeclFloat Float
  | VarDeclDouble Double
  | VarDeclChar   CExpr.CharValue
  | VarDeclString ByteArray
  | VarDeclLambda (Lambda VarDeclRHS ctx)
  | VarDeclApp VarDeclRHSAppHead [VarDeclRHS ctx]
  | VarDeclVar (Idx ctx)
  deriving stock (Generic, Show)

-- | The function at the head of an application in the Haskell translation
-- of a C macro.
data VarDeclRHSAppHead
  -- | The translation of a built-in C infix function such as @*@ or @&&@.
  = forall arity. InfixAppHead (C.MFun arity)
  -- | A function name, or the name of a function-like macro.
  | VarAppHead (HsName NsVar)

deriving stock instance Show VarDeclRHSAppHead

{-------------------------------------------------------------------------------
  Pattern Synonyms
-------------------------------------------------------------------------------}

-- | Pattern synonyms
--
-- For now only pattern synonyms of form
--
-- @
-- pattern P :: T
-- pattern P = C e
-- @
--
data PatSyn = PatSyn
    { patSynName   :: HsName NsConstr
    , patSynType   :: HsName NsTypeConstr
    , patSynConstr :: HsName NsConstr
    , patSynValue  :: Integer
    , patSynOrigin :: PatSynOrigin
    }
  deriving stock (Generic, Show)

newtype PatSynOrigin =
      PatSynOriginEnumValue C.EnumValue
  deriving stock (Generic, Show)

{-------------------------------------------------------------------------------
  'Storable'
-------------------------------------------------------------------------------}

-- | 'Storable' instance
--
-- Currently this models storable instances for structs /only/.
--
-- <https://hackage.haskell.org/package/base/docs/Foreign-Storable.html#t:Storable>
type StorableInstance :: Star
data StorableInstance = StorableInstance
    { storableSizeOf    :: Int
    , storableAlignment :: Int
    , storablePeek      :: Lambda (Ap StructCon PeekByteOff) EmptyCtx
    , storablePoke      :: Lambda (Lambda (ElimStruct (Seq PokeByteOff))) EmptyCtx
    }
  deriving stock (Generic, Show)

-- | Call to 'peekByteOff'
--
-- <https://hackage.haskell.org/package/base/docs/Foreign-Storable.html#v:peekByteOff>
type PeekByteOff :: Ctx -> Star
data PeekByteOff ctx
  = PeekByteOff (Idx ctx) Int
  | PeekBitOffWidth (Idx ctx) Int Int
  deriving stock (Generic, Show)

-- | Call to 'pokeByteOff'
--
-- <https://hackage.haskell.org/package/base/docs/Foreign-Storable.html#v:pokeByteOff>
type PokeByteOff :: Ctx -> Star
data PokeByteOff ctx
  = PokeByteOff (Idx ctx) Int (Idx ctx)
  | PokeBitOffWidth (Idx ctx) Int Int (Idx ctx)
  deriving stock (Generic, Show)

{-------------------------------------------------------------------------------
  Statements
-------------------------------------------------------------------------------}

-- | Simple sequential composition (no bindings)
newtype Seq t ctx = Seq [t ctx]
  deriving stock (Generic, Show)

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

type StructCon :: Ctx -> Star
data StructCon ctx where
    StructCon :: Struct n -> StructCon ctx

deriving instance Show (StructCon ctx)

-- | Case split for a struct
type ElimStruct :: (Ctx -> Star) -> (Ctx -> Star)
data ElimStruct t ctx where
    ElimStruct ::
         Idx ctx
      -> Struct n
      -> Add n ctx ctx'
      -> t ctx'
      -> ElimStruct t ctx

deriving instance (forall ctx'. Show (t ctx')) => Show (ElimStruct t ctx)

-- | Create 'ElimStruct' using kind-of HOAS interface.
makeElimStruct :: forall n ctx t.
     SNatI n
  => Idx ctx
  -> Struct n
  -> (forall ctx'. Wk ctx ctx' -> Vec n (Idx ctx') -> t ctx')
  -> ElimStruct t ctx
makeElimStruct s struct kont = makeElimStruct' (snat :: SNat n) $ \add wk xs ->
    ElimStruct s struct add (kont wk xs)

-- TODO: use Data.Type.Nat.induction instead of explicit recursion.
-- TODO: verify that we bind fields in right order.
makeElimStruct' :: forall m ctx t.
     SNat m
  -> ( forall ctx'.
            Add m ctx ctx'
         -> Wk ctx ctx'
         -> Vec m (Idx ctx')
         -> ElimStruct t ctx
     )
  -> ElimStruct t ctx
makeElimStruct' Nat.SZ      kont = kont AZ IdWk VNil
makeElimStruct' (Nat.SS' n) kont = makeElimStruct' n $ \add wk xs ->
    kont (AS add) (SkipWk wk) (IZ ::: fmap IS xs)
