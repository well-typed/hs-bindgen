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
    Struct(..)
  , Newtype(..)
    -- * Types
  , HsType(..)
    -- * Variable binding
  , Lambda(..)
  , Ap(..)
  , Forall(..)
    -- * Declarations
  , Decl(..)
  , InstanceDecl(..)
  , DataDecl(..)
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
  , WithStruct(..)
  , IntroStruct(..)
  , ElimStruct(..)
  ) where

import Data.Nat
import Data.Type.Nat
import Data.Vec.Lazy (Vec(..), toList)
import Generics.SOP qualified as SOP
import GHC.Generics qualified as GHC
import GHC.Show (appPrec1)

import HsBindgen.C.AST qualified as C (MFun(..))
import HsBindgen.C.Tc.Macro qualified as C
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type
import HsBindgen.Util.PHOAS

{-------------------------------------------------------------------------------
  Information about generated code
-------------------------------------------------------------------------------}

data Struct (n :: Nat) = Struct {
      structName   :: HsName NsTypeConstr
    , structConstr :: HsName NsConstr
    , structFields :: Vec n (HsName NsVar, HsType)
    }

deriving stock instance Show (Struct n)

data Newtype = Newtype {
      newtypeName   :: HsName NsTypeConstr
    , newtypeConstr :: HsName NsConstr
    , newtypeField  :: HsName NsVar
    , newtypeType   :: HsType
    }

deriving stock instance Show Newtype

{-------------------------------------------------------------------------------
  Variable binding
-------------------------------------------------------------------------------}

-- | Lambda abstraction
type Lambda :: PHOAS -> PHOAS
data Lambda a f = Lambda (f Bound -> a f)
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Forall
type Forall :: Nat -> PHOAS -> PHOAS
data Forall n a f = Forall (Vec n (f Bound) -> a f)
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Applicative structure
type Ap :: PHOAS -> PHOAS -> PHOAS
data Ap a b f = Ap (b f) [a f]
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

-- | Top-level declaration
type Decl :: PHOAS
data Decl f =
    DeclData (WithStruct DataDecl f)
  | DeclNewtype Newtype
  | DeclInstance (InstanceDecl f)
  | DeclNewtypeInstance TypeClass (HsName NsTypeConstr)
  | DeclVar (VarDecl f)
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Class instance names
data TypeClass =
    Storable
  deriving stock (Show)

-- | Class instance declaration
type InstanceDecl :: PHOAS
data InstanceDecl f =
    InstanceStorable (WithStruct StorableInstance f)
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

type DataDecl :: Nat -> PHOAS
data DataDecl n f = MkDataDecl
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Variable or function declaration.
type VarDecl :: PHOAS
data VarDecl f =
  VarDecl
    -- | Name of variable/function.
    { varDeclName :: HsName NsVar
    -- | Type of variable/function.
    , varDeclType :: SigmaType f
    -- | RHS of variable/function.
    , varDeclBody :: VarDeclRHS f
    }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | A σ-type, of the form @forall tvs. ctxt => body@.
type SigmaType :: PHOAS
data SigmaType f
  = forall n. SNatI n
  => ForallTy
      { forallTyBinders :: Vec n (HsName NsTypeVar)
      , forallTy :: Forall n PhiType f
      }

-- | A φ-type, of the form @ctxt => body@.
type PhiType :: PHOAS
data PhiType f
  = QuantTy
  { quantTyCts  :: [ClassTy f]
  , quantTyBody :: TauType f
  }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | A τ-type: no quantification or contexts (i.e. no @forall@, no @=>@ arrows).
type TauType :: PHOAS
data TauType f
  = FunTy (TauType f) (TauType f)
  | TyVarTy (f Bound)
  | TyConAppTy (TyConAppTy f)
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

data TyConAppTy f where
  TyConApp :: C.DataTyCon n -> Vec n (TauType f) -> TyConAppTy f

data ClassTy f where
  ClassTy :: C.ClassTyCon n -> Vec n (TauType f) -> ClassTy f

-- | RHS of a variable or function declaration.
type VarDeclRHS :: PHOAS
data VarDeclRHS f
  = VarDeclIntegral Integer HsPrimType
  | VarDeclFloat Float
  | VarDeclDouble Double
  | VarDeclLambda (HsName NsVar) (Lambda VarDeclRHS f)
  | VarDeclApp VarDeclRHSAppHead [VarDeclRHS f]
  | VarDeclVar (f Bound)
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | The function at the head of an application in the Haskell translation
-- of a C macro.
data VarDeclRHSAppHead
  -- | The translation of a built-in C infix function such as @*@ or @&&@.
  = forall arity. InfixAppHead (C.MFun arity)
  -- | A function name, or the name of a function-like macro.
  | VarAppHead (HsName NsVar)

{-------------------------------------------------------------------------------
  'Storable'
-------------------------------------------------------------------------------}

-- | 'Storable' instance
--
-- Currently this models storable instances for structs /only/.
--
-- <https://hackage.haskell.org/package/base/docs/Foreign-Storable.html#t:Storable>
type StorableInstance :: Nat -> PHOAS
data StorableInstance n f where
    StorableInstance ::
         { storableSizeOf    :: Int
         , storableAlignment :: Int
         , storablePeek      :: Lambda (Ap PeekByteOff (IntroStruct n)) f
         , storablePoke      :: Lambda (ElimStruct n (Seq PokeByteOff)) f
         }
      -> StorableInstance n f
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Call to 'peekByteOff'
--
-- <https://hackage.haskell.org/package/base/docs/Foreign-Storable.html#v:peekByteOff>
type PeekByteOff :: PHOAS
data PeekByteOff f = PeekByteOff (f Bound) Int
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Call to 'pokeByteOff'
--
-- <https://hackage.haskell.org/package/base/docs/Foreign-Storable.html#v:pokeByteOff>
type PokeByteOff :: PHOAS
data PokeByteOff f = PokeByteOff (f Bound) Int (f Bound)
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

{-------------------------------------------------------------------------------
  Statements
-------------------------------------------------------------------------------}

-- | Simple sequential composition (no bindings)
type Seq :: PHOAS -> PHOAS
newtype Seq a f = Seq (List a f)
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

type WithStruct :: (Nat -> PHOAS) -> PHOAS
data WithStruct a f where
  WithStruct :: SNatI n => Struct n -> a n f -> WithStruct a f

-- | Construct value of a struct
type IntroStruct :: Nat -> PHOAS
data IntroStruct n f = IntroStruct (Struct n)
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Lambda-case for a struct
type ElimStruct :: Nat -> PHOAS -> PHOAS
data ElimStruct n a f = ElimStruct (Struct n) (Vec n (f Bound) -> a f)
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

{-------------------------------------------------------------------------------
  Show instances

  These generate valid Haskell code.
-------------------------------------------------------------------------------}

deriving anyclass instance ShowOpen (Decl Unique)
deriving anyclass instance SNatI n => ShowOpen (DataDecl n Unique)
deriving anyclass instance ShowOpen (InstanceDecl Unique)
deriving anyclass instance ShowOpen (PeekByteOff Unique)
deriving anyclass instance ShowOpen (PokeByteOff Unique)

deriving anyclass instance SNatI n => ShowOpen (IntroStruct n Unique)
deriving anyclass instance SNatI n => ShowOpen (StorableInstance n Unique)

deriving anyclass instance ShowOpen (a Unique) => ShowOpen (Lambda a Unique)
deriving anyclass instance (SNatI n, ShowOpen (a Unique)) => ShowOpen (Forall n a Unique)
deriving anyclass instance ShowOpen (a Unique) => ShowOpen (Seq a Unique)

deriving anyclass instance
     (ShowOpen (a Unique), ShowOpen (b Unique))
  => ShowOpen (Ap a b Unique)

deriving anyclass instance
     (ShowOpen (a Unique), SNatI n)
  => ShowOpen (ElimStruct n a Unique)

deriving via Degenerate (Struct n) instance ShowOpen (Struct n)

-- Handwritten instance (generics don't play nice with existentials)
instance
       (forall n. SNatI n => ShowOpen (a n Unique))
    => ShowOpen (WithStruct a Unique) where
  showOpen u p (WithStruct struct a) = showParen (p >= appPrec1) $
        showString "WithStruct "
      . showOpen u appPrec1 struct
      . showString " "
      . showOpen u appPrec1 a

instance ShowOpen Newtype where
    showOpen _ = showsPrec

instance ShowOpen TypeClass where
    showOpen _ = showsPrec

instance ShowOpen (VarDecl Unique) where
    showOpen u p (VarDecl nm ty rhs) = showParen (p >= appPrec1) $
        showString "VarDecl "
      . showsPrec appPrec1 nm
      . showString " "
      . showOpen u appPrec1 ty
      . showString " "
      . showOpen u appPrec1 rhs

instance ShowOpen (SigmaType Unique) where
    showOpen u p = showParen (p >= appPrec1) . \case
      ForallTy tvs f ->
          showString "ForallTy "
        . showsPrec appPrec1 tvs
        . showString " "
        . showOpen u appPrec1 f
deriving anyclass instance ShowOpen (PhiType Unique)


instance ShowOpen (TyConAppTy Unique) where
    showOpen u p = showParen (p >= appPrec1) . \case
      TyConApp tc args ->
          showString "TyConApp "
        . showsPrec appPrec1 tc
        . showString " "
        . showOpen u appPrec1 (toList args)
instance ShowOpen (ClassTy Unique) where
    showOpen u p = showParen (p >= appPrec1) . \case
      ClassTy tc args ->
          showString "ClassTy "
        . showsPrec appPrec1 tc
        . showString " "
        . showOpen u appPrec1 (toList args)

instance ShowOpen (TauType Unique) where
    showOpen u p = showParen (p >= appPrec1) . \case
      FunTy arg res ->
          showString "FunTy "
        . showOpen u appPrec1 arg
        . showString " "
        . showOpen u appPrec1 res
      TyVarTy tv ->
          showString "TyVarTy "
        . showOpen u appPrec1 tv
      TyConAppTy tcApp ->
          showString "TyConAppTy "
        . showOpen u appPrec1 tcApp

instance ShowOpen (VarDeclRHS Unique) where
    showOpen u p = showParen (p >= appPrec1) . \case
      VarDeclIntegral i ty ->
          showString "VarDeclIntegral "
        . showsPrec appPrec1 i
        . showString " "
        . showsPrec appPrec1 ty
      VarDeclFloat f ->
          showString "VarDeclFloat "
        . showsPrec appPrec1 f
      VarDeclDouble d ->
          showString "VarDeclDouble "
        . showsPrec appPrec1 d
      VarDeclLambda nm k ->
          showString "VarDeclLambda "
        . showsPrec appPrec1 nm
        . showString " "
        . showOpen u appPrec1 k
      VarDeclApp f as ->
          showString "VarDeclApp "
        . showsPrec appPrec1 f
        . showString " "
        . showOpen u appPrec1 as
      VarDeclVar var ->
          showString "VarDeclVar "
        . showOpen u appPrec1 var

instance Show VarDeclRHSAppHead where
    showsPrec p = showParen (p >= appPrec1) . \case
      InfixAppHead mfun ->
          showString "InfixAppHead "
        . showsPrec appPrec1 mfun
      VarAppHead macroNm ->
          showString "VarApphead "
        . showsPrec appPrec1 macroNm
