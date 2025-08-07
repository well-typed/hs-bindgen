{-# LANGUAGE MagicHash #-}

-- | Haskell AST
--
-- Abstract Haskell syntax for the specific purposes of hs-bindgen: we only
-- cover the parts of the Haskell syntax that we need. We attempt to do this in
-- such a way that the generated Haskell code is type correct by construction.
--
-- Intended for qualified import:
--
-- > import HsBindgen.Backend.Hs.AST qualified as Hs
module HsBindgen.Backend.Hs.AST (
    -- * Generated Haskell datatypes
    Field(..)
  , Struct(..)
  , EmptyData(..)
  , Newtype(..)
    -- * Types
  , HsType(..)
    -- * Variable binding
  , Lambda(..)
  , Ap(..)
    -- * Declarations
  , Decl(..)
  , InstanceDecl(..)
  , UnionGetter(..)
  , UnionSetter(..)
  , DefineInstance(..)
  , DeriveInstance(..)
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
  , HsTypeClass(..)
    -- ** Foreign imports
  , ForeignImportDecl(..)
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
  ) where

import Data.Type.Nat (SNat, SNatI, snat)
import Data.Type.Nat qualified as Nat
import DeBruijn (Ctx, EmptyCtx, Wk (..), Add (..), Idx (..))

import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Backend.Hs.AST.SigmaType
import HsBindgen.Backend.Hs.AST.Strategy
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation (Comment)
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Imports
import HsBindgen.Language.Haskell
import HsBindgen.NameHint
import HsBindgen.Orphans ()
import HsBindgen.SHs.AST qualified as SHs

import C.Char qualified

{-------------------------------------------------------------------------------
  Information about generated code
-------------------------------------------------------------------------------}

data Field = Field {
      fieldName    :: HsName NsVar
    , fieldType    :: HsType
    , fieldOrigin  :: Origin.Field
    , fieldComment :: Maybe Comment
    }
  deriving stock (Generic, Show)

data Struct (n :: Nat) = Struct {
      structName      :: HsName NsTypeConstr
    , structConstr    :: HsName NsConstr
    , structFields    :: Vec n Field
      -- TODO: This is a temporary work-around: for enums we generate /both/
      -- a newtype /and/ a struct, and then define instances only for the
      -- struct. This is a nasty hack that we should get rid of.
    , structOrigin    :: Maybe (Origin.Decl Origin.Struct)
    , structInstances :: Set HsTypeClass
    , structComment   :: Maybe Comment
    }
  deriving stock (Generic, Show)

data EmptyData = EmptyData {
      emptyDataName    :: HsName NsTypeConstr
    , emptyDataOrigin  :: Origin.Decl Origin.EmptyData
    , emptyDataComment :: Maybe Comment
    }
  deriving stock (Generic, Show)

data Newtype = Newtype {
      newtypeName      :: HsName NsTypeConstr
    , newtypeConstr    :: HsName NsConstr
    , newtypeField     :: Field
    , newtypeOrigin    :: Origin.Decl Origin.Newtype
    , newtypeInstances :: Set HsTypeClass
    , newtypeComment   :: Maybe Comment
    }
  deriving stock (Generic, Show)

data ForeignImportDecl = ForeignImportDecl
    { foreignImportName     :: HsName NsVar
    , foreignImportType     :: HsType
    , foreignImportOrigName :: Text
    , foreignImportCallConv :: CallConv
    , foreignImportOrigin   :: Origin.ForeignImport
    , foreignImportComment  :: Maybe Comment
    }
  deriving stock (Generic, Show)

data UnionGetter = UnionGetter
  { unionGetterName    :: HsName NsVar
  , unionGetterType    :: HsType
  , unionGetterConstr  :: HsName NsTypeConstr
  , unionGetterComment :: Maybe Comment
  }
  deriving stock (Generic, Show)

data UnionSetter = UnionSetter
  { unionSetterName    :: HsName NsVar
  , unionSetterType    :: HsType
  , unionSetterConstr  :: HsName NsTypeConstr
  , unionSetterComment :: Maybe Comment
  }
  deriving stock (Generic, Show)

data DeriveInstance = DeriveInstance
  { deriveInstanceStrategy :: Strategy HsType
  , deriveInstanceClass    :: HsTypeClass
  , deriveInstanceName     :: HsName NsTypeConstr
  , deriveInstanceComment  :: Maybe Comment
  }
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
    DeclDefineInstance  :: DefineInstance -> Decl
    DeclDeriveInstance  :: DeriveInstance -> Decl
    DeclInlineCInclude  :: String -> Decl
    DeclInlineC         :: String -> Decl
    DeclForeignImport   :: ForeignImportDecl -> Decl
    DeclVar             :: VarDecl -> Decl
    DeclUnionGetter     :: UnionGetter -> Decl
    DeclUnionSetter     :: UnionSetter -> Decl
    DeclSimple          :: SHs.SDecl -> Decl
deriving instance Show Decl

data DefineInstance =
  DefineInstance
    { defineInstanceDeclarations :: InstanceDecl
    , defineInstanceComment      :: Maybe Comment
    }
  deriving stock (Generic, Show)

-- | Class instance declaration (with code that /we/ generate)
type InstanceDecl :: Star
data InstanceDecl where
    InstanceStorable :: Struct n -> StorableInstance -> InstanceDecl
    InstanceHasFLAM :: Struct n -> HsType -> Int -> InstanceDecl
    InstanceCEnum ::
         Struct (S Z)
      -> HsType
      -> Map Integer (NonEmpty String)
      -> Bool  -- is sequential?
      -> InstanceDecl
    InstanceSequentialCEnum ::
         Struct (S Z)
      -> HsName NsConstr
      -> HsName NsConstr
      -> InstanceDecl
    InstanceCEnumShow :: Struct (S Z) -> InstanceDecl
    InstanceCEnumRead :: Struct (S Z) -> InstanceDecl

deriving instance Show InstanceDecl

-- | Variable or function declaration.
type VarDecl :: Star
data VarDecl =
  VarDecl
    -- | Name of variable/function.
    { varDeclName    :: HsName NsVar
    -- | Type of variable/function.
    , varDeclType    :: SigmaType
    -- | RHS of variable/function.
    , varDeclBody    :: VarDeclRHS EmptyCtx
    , varDeclComment :: Maybe Comment
    }
  deriving stock (Generic, Show)

-- | RHS of a variable or function declaration.
type VarDeclRHS :: Ctx -> Star
data VarDeclRHS ctx
  = VarDeclIntegral Integer HsPrimType
  | VarDeclFloat Float
  | VarDeclDouble Double
  | VarDeclChar   C.Char.CharValue
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
    { patSynName    :: HsName NsConstr
    , patSynType    :: HsName NsTypeConstr
    , patSynConstr  :: HsName NsConstr
    , patSynValue   :: Integer
    , patSynOrigin  :: Origin.PatSyn
    , patSynComment :: Maybe Comment
    }
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
