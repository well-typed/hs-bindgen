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
    -- * Passes and Annotations
    Pass(..)
  , Ann
    -- * Information about generated code
  , Struct(..)
  , Newtype(..)
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
  ) where

import Data.Type.Nat as Nat
import GHC.Base (Symbol)

import HsBindgen.C.AST qualified as C (MFun(..))
import HsBindgen.C.Tc.Macro qualified as C
import HsBindgen.Imports
import HsBindgen.NameHint
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type

import DeBruijn

{-------------------------------------------------------------------------------
  Passes and annotations
-------------------------------------------------------------------------------}

-- | Passes for the Haskell AST phase
data Pass = Placeholder

-- | Symbol-indexed annotations for a given pass
type family Ann (pass :: Pass) (s :: Symbol) where
  Ann Placeholder s = AnnPlaceholder s

-- | Symbol-indexed annotations for the 'Placeholder' pass
type family AnnPlaceholder (s :: Symbol) where
  AnnPlaceholder s = ()

-- Class alias to work around GHC limitation that type family synonym
-- applications cannot be used in quantified constraints
class    Show (Ann pass s) => ShowAnn pass s
instance Show (Ann pass s) => ShowAnn pass s

-- All annotations must have a 'Show' instance (quantified constraint)
class    (forall s. ShowAnn pass s) => AllAnnShow pass
instance (forall s. ShowAnn pass s) => AllAnnShow pass

{-------------------------------------------------------------------------------
  Information about generated code
-------------------------------------------------------------------------------}

data Struct (pass :: Pass) (n :: Nat) = Struct {
      structAnn    :: Ann pass "Struct"
    , structName   :: HsName NsTypeConstr
    , structConstr :: HsName NsConstr
    , structFields :: Vec n (Ann pass "StructField", (HsName NsVar, HsType))
    }

deriving stock instance AllAnnShow pass => Show (Struct pass n)

data Newtype (pass :: Pass) = Newtype {
      newtypeAnn      :: Ann pass "Newtype"
    , newtypeName     :: HsName NsTypeConstr
    , newtypeConstr   :: HsName NsConstr
    , newtypeFieldAnn :: Ann pass "NewtypeField"
    , newtypeField    :: HsName NsVar
    , newtypeType     :: HsType
    }

deriving stock instance AllAnnShow pass => Show (Newtype pass)

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
  deriving Show

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

-- | Top-level declaration
type Decl :: Pass -> Star
data Decl pass where
    DeclData            :: SNatI n => Struct pass n -> Decl pass
    DeclEmpty           :: HsName NsTypeConstr -> Decl pass
    DeclNewtype         :: Newtype pass -> Decl pass
    DeclInstance        :: InstanceDecl pass -> Decl pass
    DeclNewtypeInstance :: TypeClass -> HsName NsTypeConstr -> Decl pass
    DeclVar             :: VarDecl -> Decl pass

deriving instance AllAnnShow pass => Show (Decl pass)

-- | Class instance names
data TypeClass =
    Storable
  deriving stock (Show)

-- | Class instance declaration
type InstanceDecl :: Pass -> Star
data InstanceDecl pass where
    InstanceStorable ::
         Struct pass n
      -> StorableInstance pass
      -> InstanceDecl pass

deriving instance AllAnnShow pass => Show (InstanceDecl pass)

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
  deriving Show

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

-- | A φ-type, of the form @ctxt => body@.
type PhiType :: Ctx -> Star
data PhiType ctx
  = QuantTy
  { quantTyCts  :: [ClassTy ctx]
  , quantTyBody :: TauType ctx
  }

deriving stock instance Show (PhiType ctx)

-- | A τ-type: no quantification or contexts (i.e. no @forall@, no @=>@ arrows).
type TauType :: Ctx -> Star
data TauType ctx
  = FunTy (TauType ctx) (TauType ctx)
  | TyVarTy (Idx ctx)
  | TyConAppTy (TyConAppTy ctx)

deriving stock instance Show (TauType ctx)

data TyConAppTy ctx where
  TyConApp :: C.DataTyCon arity -> Vec arity (TauType ctx) -> TyConAppTy ctx

deriving stock instance Show (TyConAppTy ctx)

data ClassTy ctx where
  ClassTy :: C.ClassTyCon arity -> Vec arity (TauType ctx) -> ClassTy ctx

deriving stock instance Show (ClassTy ctx)

-- | RHS of a variable or function declaration.
type VarDeclRHS :: Ctx -> Star
data VarDeclRHS ctx
  = VarDeclIntegral Integer HsPrimType
  | VarDeclFloat Float
  | VarDeclDouble Double
  | VarDeclLambda (Lambda VarDeclRHS ctx)
  | VarDeclApp VarDeclRHSAppHead [VarDeclRHS ctx]
  | VarDeclVar (Idx ctx)

deriving stock instance Show (VarDeclRHS ctx)

-- | The function at the head of an application in the Haskell translation
-- of a C macro.
data VarDeclRHSAppHead
  -- | The translation of a built-in C infix function such as @*@ or @&&@.
  = forall arity. InfixAppHead (C.MFun arity)
  -- | A function name, or the name of a function-like macro.
  | VarAppHead (HsName NsVar)

deriving stock instance Show VarDeclRHSAppHead

{-------------------------------------------------------------------------------
  'Storable'
-------------------------------------------------------------------------------}

-- | 'Storable' instance
--
-- Currently this models storable instances for structs /only/.
--
-- <https://hackage.haskell.org/package/base/docs/Foreign-Storable.html#t:Storable>
type StorableInstance :: Pass -> Star
data StorableInstance pass = StorableInstance
    { storableSizeOf    :: Int
    , storableAlignment :: Int
    , storablePeek      :: Lambda (Ap (StructCon pass) PeekByteOff) EmptyCtx
    , storablePoke      ::
        Lambda (Lambda (ElimStruct pass (Seq PokeByteOff))) EmptyCtx
    }

deriving instance AllAnnShow pass => Show (StorableInstance pass)

-- | Call to 'peekByteOff'
--
-- <https://hackage.haskell.org/package/base/docs/Foreign-Storable.html#v:peekByteOff>
type PeekByteOff :: Ctx -> Star
data PeekByteOff ctx = PeekByteOff
    (Idx ctx)
    Int
  deriving Show

-- | Call to 'pokeByteOff'
--
-- <https://hackage.haskell.org/package/base/docs/Foreign-Storable.html#v:pokeByteOff>
type PokeByteOff :: Ctx -> Star
data PokeByteOff ctx = PokeByteOff (Idx ctx) Int (Idx ctx)
  deriving Show

{-------------------------------------------------------------------------------
  Statements
-------------------------------------------------------------------------------}

-- | Simple sequential composition (no bindings)
newtype Seq t ctx = Seq [t ctx]
  deriving Show

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

type StructCon :: Pass -> Ctx -> Star
data StructCon pass ctx where
    StructCon :: Struct pass n -> StructCon pass ctx

deriving instance AllAnnShow pass => Show (StructCon pass ctx)

-- | Case split for a struct
type ElimStruct :: Pass -> (Ctx -> Star) -> (Ctx -> Star)
data ElimStruct pass t ctx where
    ElimStruct ::
         Idx ctx
      -> Struct pass n
      -> Add n ctx ctx'
      -> t ctx'
      -> ElimStruct pass t ctx

deriving instance
     (AllAnnShow pass, forall ctx'. Show (t ctx'))
  => Show (ElimStruct pass t ctx)

-- | Create 'ElimStruct' using kind-of HOAS interface.
--
makeElimStruct :: forall n ctx t pass.
     SNatI n
  => Idx ctx
  -> Struct pass n
  -> (forall ctx'. Wk ctx ctx' -> Vec n (Idx ctx') -> t ctx')
  -> ElimStruct pass t ctx
makeElimStruct s struct kont = makeElimStruct' (snat :: SNat n) $ \add wk xs ->
    ElimStruct s struct add (kont wk xs)

--
-- TODO: use Data.Type.Nat.induction instead of explicit recursion.
-- TODO: verify that we bind fields in right order.
makeElimStruct' :: forall m ctx t pass.
     SNat m
  -> ( forall ctx'.
            Add m ctx ctx'
         -> Wk ctx ctx'
         -> Vec m (Idx ctx')
         -> ElimStruct pass t ctx
     )
  -> ElimStruct pass t ctx
makeElimStruct' Nat.SZ      kont = kont AZ IdWk VNil
makeElimStruct' (Nat.SS' n) kont = makeElimStruct' n $ \add wk xs -> kont (AS add) (SkipWk wk) (IZ ::: fmap IS xs)
