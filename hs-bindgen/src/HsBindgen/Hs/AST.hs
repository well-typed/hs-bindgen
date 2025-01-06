-- | Haskell AST
--
-- Abstract Haskell syntax for the specific purposes of hs-bindgen: we only
-- cover the parts of the Haskell syntax that we need. We attempt to do this in
-- such a way that the generated Haskell code is type correct by construction.
-- We use PHOAS for bound variables: the individual backends (TH, standalone)
-- are responsible for generating fresh variable names.
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/23>
-- We should annotate the AST with to explain tool decisions (when generating
-- high-level API).
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/74>
-- We should annotate the AST with the relevant part of the C header here
-- (including line numbers).
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
  , TyConAppTy(..)
  , ClassTy(..)
  , VarDeclRHS(..)
  , VarDeclRHSAppHead(..)
    -- ** Newtype instances
  , TypeClass (..)
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

import HsBindgen.C.AST qualified as C
import HsBindgen.C.Tc.Macro qualified as C
import Data.GADT.Compare (GEq(geq), defaultEq)
import Data.Type.Equality ((:~:)(Refl))
import Data.Type.Nat as Nat
import Data.Vec.Lazy qualified as Vec
import Unsafe.Coerce (unsafeCoerce)

import HsBindgen.Imports
import HsBindgen.NameHint
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type
import HsBindgen.Orphans ()
import HsBindgen.Util.TestEquality

import DeBruijn
import DeBruijn.Internal.Add (Add(UnsafeAdd))

{-------------------------------------------------------------------------------
  Information about generated code
-------------------------------------------------------------------------------}

data Field = Field {
      fieldName   :: HsName NsVar
    , fieldType   :: HsType
    , fieldOrigin :: FieldOrigin
    }
  deriving stock (Eq, Generic, Show)

data FieldOrigin =
      FieldOriginNone
    | FieldOriginStructField C.StructField
  deriving stock (Eq, Generic, Show)

data Struct (n :: Nat) = Struct {
      structName   :: HsName NsTypeConstr
    , structConstr :: HsName NsConstr
    , structFields :: Vec n Field
    , structOrigin :: StructOrigin
    }
  deriving stock (Eq, Generic, Show)
  deriving GEq via ApEq Struct

data StructOrigin =
      StructOriginStruct C.Struct
    | StructOriginEnum C.Enu
  deriving stock (Eq, Generic, Show)

data Newtype = Newtype {
      newtypeName   :: HsName NsTypeConstr
    , newtypeConstr :: HsName NsConstr
    , newtypeField  :: Field
    , newtypeOrigin :: NewtypeOrigin
    }
  deriving stock (Eq, Generic, Show)

data NewtypeOrigin =
      NewtypeOriginEnum C.Enu
    | NewtypeOriginTypedef C.Typedef
    | NewtypeOriginMacro C.Macro
  deriving stock (Eq, Generic, Show)

data ForeignImportDecl = ForeignImportDecl
    { foreignImportName       :: HsName NsVar
    , foreignImportType       :: HsType
    , foreignImportOrigName   :: Text
    , foreignImportHeader     :: FilePath -- TODO: https://github.com/well-typed/hs-bindgen/issues/333
    , foreignImportDeclOrigin :: ForeignImportDeclOrigin
    }
  deriving stock (Eq, Generic, Show)

newtype ForeignImportDeclOrigin =
      ForeignImportDeclOriginFunction C.Function
  deriving stock (Eq, Generic, Show)

{-------------------------------------------------------------------------------
  Variable binding
-------------------------------------------------------------------------------}

-- | Lambda abstraction
type Lambda :: (Ctx -> Star) -> (Ctx -> Star)
data Lambda t ctx = Lambda
    NameHint  -- ^ name suggestion
    (t (S ctx)) -- ^ body

deriving instance Eq   (t (S ctx)) => Eq   (Lambda t ctx)
deriving instance Show (t (S ctx)) => Show (Lambda t ctx)

-- | Applicative structure

data Ap pure xs ctx = Ap (pure ctx) [xs ctx]
  deriving stock (Eq, Generic, Show)

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

-- | Top-level declaration
type Decl :: Star
data Decl where
    DeclData            :: SNatI n => Struct n -> Decl
    DeclEmpty           :: HsName NsTypeConstr -> Decl
    DeclNewtype         :: Newtype -> Decl
    DeclPatSyn          :: PatSyn -> Decl
    DeclInstance        :: InstanceDecl -> Decl
    DeclNewtypeInstance :: TypeClass -> HsName NsTypeConstr -> Decl
    DeclForeignImport   :: ForeignImportDecl -> Decl
    DeclVar             :: VarDecl -> Decl

deriving instance Show Decl

instance Eq Decl where
  DeclData l == DeclData r = l `defaultEq` r
  DeclEmpty l == DeclEmpty r = l == r
  DeclNewtype l == DeclNewtype r = l == r
  DeclPatSyn l == DeclPatSyn r = l == r
  DeclInstance l == DeclInstance r = l == r
  DeclNewtypeInstance tcL nameL == DeclNewtypeInstance tcR nameR =
    tcL == tcR && nameL == nameR
  DeclForeignImport l == DeclForeignImport r = l == r
  DeclVar l == DeclVar r = l == r
  _l == _r = False

-- | Class instance names
data TypeClass =
    Storable
  deriving stock (Eq, Generic, Show)

-- | Class instance declaration
type InstanceDecl :: Star
data InstanceDecl where
    InstanceStorable :: Struct n -> StorableInstance -> InstanceDecl

deriving instance Show InstanceDecl

instance Eq InstanceDecl where
  InstanceStorable sL iL == InstanceStorable sR iR =
    sL `defaultEq` sR && iL == iR

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
  deriving stock (Eq, Generic, Show)

-- | A σ-type, of the form @forall tvs. ctxt => body@.
type SigmaType :: Star
data SigmaType where
  ForallTy ::
       { forallTySize    :: Size n
       , forallTyBinders :: Vec n NameHint
       , forallTy        :: PhiType n
       }
    -> SigmaType

deriving stock instance Show SigmaType

instance Eq SigmaType where
  ForallTy sL vL pL == ForallTy sR vR pR =
    fromMaybe False $ do
      Refl <- geqVec vL vR
      return $ sL == sR && pL == pR

-- | A φ-type, of the form @ctxt => body@.
type PhiType :: Ctx -> Star
data PhiType ctx
  = QuantTy
  { quantTyCts  :: [ClassTy ctx]
  , quantTyBody :: TauType ctx
  }
  deriving stock (Eq, Generic, Show)

-- | A τ-type: no quantification or contexts (i.e. no @forall@, no @=>@ arrows).
type TauType :: Ctx -> Star
data TauType ctx
  = FunTy (TauType ctx) (TauType ctx)
  | TyVarTy (Idx ctx)
  | TyConAppTy (TyConAppTy ctx)
  deriving stock (Eq, Generic, Show)

data TyConAppTy ctx where
  TyConApp :: C.DataTyCon arity -> Vec arity (TauType ctx) -> TyConAppTy ctx

deriving stock instance Show (TyConAppTy ctx)

instance Eq (TauType ctx) => Eq (TyConAppTy ctx) where
  TyConApp dL vL == TyConApp dR vR =
    fromMaybe False $ do
      Refl <- geqVec vL vR
      return $ dL == dR

data ClassTy ctx where
  ClassTy :: C.ClassTyCon arity -> Vec arity (TauType ctx) -> ClassTy ctx

deriving stock instance Show (ClassTy ctx)

instance Eq (TauType ctx) => Eq (ClassTy ctx) where
  ClassTy cL vL == ClassTy cR vR =
    fromMaybe False $ do
      Refl <- geqVec vL vR
      return $ cL == cR

-- | RHS of a variable or function declaration.
type VarDeclRHS :: Ctx -> Star
data VarDeclRHS ctx
  = VarDeclIntegral Integer HsPrimType
  | VarDeclFloat Float
  | VarDeclDouble Double
  | VarDeclLambda (Lambda VarDeclRHS ctx)
  | VarDeclApp VarDeclRHSAppHead [VarDeclRHS ctx]
  | VarDeclVar (Idx ctx)
  deriving stock (Eq, Generic, Show)

-- | The function at the head of an application in the Haskell translation
-- of a C macro.
data VarDeclRHSAppHead
  -- | The translation of a built-in C infix function such as @*@ or @&&@.
  = forall arity. InfixAppHead (C.MFun arity)
  -- | A function name, or the name of a function-like macro.
  | VarAppHead (HsName NsVar)

deriving stock instance Show VarDeclRHSAppHead

instance Eq VarDeclRHSAppHead where
  InfixAppHead l == InfixAppHead r = l `defaultEq` r
  VarAppHead l == VarAppHead r = l == r
  _l == _r = False

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
  deriving stock (Eq, Generic, Show)

newtype PatSynOrigin =
      PatSynOriginEnumValue C.EnumValue
  deriving stock (Eq, Generic, Show)

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
  deriving stock (Eq, Generic, Show)

-- | Call to 'peekByteOff'
--
-- <https://hackage.haskell.org/package/base/docs/Foreign-Storable.html#v:peekByteOff>
type PeekByteOff :: Ctx -> Star
data PeekByteOff ctx = PeekByteOff
    (Idx ctx)
    Int
  deriving stock (Eq, Generic, Show)

-- | Call to 'pokeByteOff'
--
-- <https://hackage.haskell.org/package/base/docs/Foreign-Storable.html#v:pokeByteOff>
type PokeByteOff :: Ctx -> Star
data PokeByteOff ctx = PokeByteOff (Idx ctx) Int (Idx ctx)
  deriving stock (Eq, Generic, Show)

{-------------------------------------------------------------------------------
  Statements
-------------------------------------------------------------------------------}

-- | Simple sequential composition (no bindings)
newtype Seq t ctx = Seq [t ctx]
  deriving stock (Eq, Generic, Show)

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

type StructCon :: Ctx -> Star
data StructCon ctx where
    StructCon :: Struct n -> StructCon ctx

deriving instance Show (StructCon ctx)

instance Eq (StructCon ctx) where
  StructCon sL == StructCon sR = sL `defaultEq` sR

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

instance (forall ctx'. (Eq (t ctx'))) => Eq (ElimStruct t ctx) where
  ElimStruct idxL sL addL tL == ElimStruct idxR sR addR tR =
      fromMaybe False $ do
        Refl <- geq (ApEq sL) (ApEq sR)
        Refl <- geq idxL idxR
        case eqAdds addL addR of
          Refl -> return $ tL == tR
    where
      eqAdds :: Add n m pL -> Add n m pR -> pL :~: pR
      eqAdds (UnsafeAdd !_) (UnsafeAdd !_) = unsafeCoerce Refl

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

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

geqVec :: forall n m a. Eq a => Vec n a -> Vec m a -> Maybe (n :~: m)
geqVec l r = Vec.withDict l $ Vec.withDict r $
    case Nat.eqNat @n @m of
      Just Refl
        | l == r    -> Just Refl
        | otherwise -> Nothing
      Nothing       -> Nothing
