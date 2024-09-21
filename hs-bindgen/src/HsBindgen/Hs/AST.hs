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
    -- * Variable binding
  , Lambda(..)
  , Ap(..)
    -- * Declarations
  , Decl(..)
  , InstanceDecl(..)
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
import Data.Vec.Lazy (Vec(..))
import Generics.SOP qualified as SOP
import GHC.Generics qualified as GHC
import GHC.Show (appPrec1)

import HsBindgen.Hs.AST.Name
import HsBindgen.Util.PHOAS

{-------------------------------------------------------------------------------
  Information about generated code
-------------------------------------------------------------------------------}

data Struct (n :: Nat) = Struct {
      structName   :: HsName NsTypeConstr
    , structConstr :: HsName NsConstr
    , structFields :: Vec n (HsName NsVar)
    }

deriving stock instance Show (Struct n)

{-------------------------------------------------------------------------------
  Variable binding
-------------------------------------------------------------------------------}

-- | Lambda abstraction
type Lambda :: PHOAS -> PHOAS
data Lambda a f = Lambda (f Bound -> a f)
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
    DeclInstance (InstanceDecl f)
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Class instance declaration
type InstanceDecl :: PHOAS
data InstanceDecl f =
    InstanceStorable (WithStruct StorableInstance f)
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

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
deriving anyclass instance ShowOpen (InstanceDecl Unique)
deriving anyclass instance ShowOpen (PeekByteOff Unique)
deriving anyclass instance ShowOpen (PokeByteOff Unique)

deriving anyclass instance SNatI n => ShowOpen (IntroStruct n Unique)
deriving anyclass instance SNatI n => ShowOpen (StorableInstance n Unique)

deriving anyclass instance ShowOpen (a Unique) => ShowOpen (Lambda a Unique)
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
