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
  , Apply(..)
  , Ap(..)
    -- * Declarations
  , Decl(..)
  , InstanceDecl(..)
  , UnionGetter(..)
  , UnionSetter(..)
  , DefineInstance(..)
  , DeriveInstance(..)
  , Var(..)
    -- ** Variable declarations
  , MacroExpr(..)
  , VarDeclRHS(..)
  , VarDeclRHSAppHead(..)
    -- ** Deriving instances
  , Strategy(..)
  , Hs.TypeClass(..)
    -- ** Foreign imports
  , ForeignImportDecl(..)
  , FunctionParameter(..)
    -- ** Function declarations
  , FunctionDecl(..)
    -- ** 'ToFunPtr'
  , ToFunPtrInstance(..)
  , ForeignImportWrapper(..)
    -- ** 'FromFunPtr'
  , FromFunPtrInstance(..)
  , ForeignImportDynamic(..)
    -- ** 'Storable'
  , StorableInstance(..)
  , PeekCField(..)
  , PokeCField(..)
    -- ** 'Prim'
  , PrimInstance(..)
  , IndexPrimFieldData(..)
  , IndexByteArrayField(..)
  , IndexOffAddrField(..)
  , ReadPrimFieldsData(..)
  , ReadByteArrayFields(..)
  , ReadOffAddrFields(..)
  , WritePrimFieldsData(..)
  , WriteByteArrayFields(..)
  , WriteOffAddrFields(..)
    -- ** 'HasCField'
  , HasCFieldInstance(..)
    -- ** 'HasCBitfield'
  , HasCBitfieldInstance(..)
    -- ** 'HasField'
  , HasFieldInstance(..)
  , HasFieldInstanceVia(..)
    -- ** Statements
  , Seq(..)
    -- ** Type synonyms
  , TypSyn (..)
    -- ** Structs
  , StructCon (..)
  , ElimStruct(..)
  , makeElimStruct
    -- ** Pattern Synonyms
  , PatSyn(..)
  ) where

import Data.Type.Nat (SNat, SNatI, snat)
import Data.Type.Nat qualified as Nat
import DeBruijn (Add (..), Ctx, EmptyCtx, Idx (..), Wk (..))

import C.Char qualified as CExpr.Runtime

import C.Expr.Syntax qualified as CExpr.DSL

import HsBindgen.Backend.Hs.AST.Strategy
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.UniqueSymbol (UniqueSymbol)
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.NameHint
import HsBindgen.Orphans ()

{-------------------------------------------------------------------------------
  Information about generated code
-------------------------------------------------------------------------------}

data Field = Field{
      name    :: Hs.Name Hs.NsVar
    , typ     :: HsType
    , origin  :: Origin.Field
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Generic, Show)

-- | Struct
--
-- TODO: for enums we generate /both/ a newtype /and/ a struct, and then define
-- instances only for the struct. We should get rid of this nasty hack.
data Struct (n :: Nat) = Struct{
      name      :: Hs.Name Hs.NsTypeConstr
    , constr    :: Hs.Name Hs.NsConstr
    , fields    :: Vec n Field
    , origin    :: Maybe (Origin.Decl Origin.Struct)
    , instances :: Set Hs.TypeClass
    , comment   :: Maybe HsDoc.Comment
    }
  deriving stock (Generic, Show)

data EmptyData = EmptyData{
      name    :: Hs.Name Hs.NsTypeConstr
    , origin  :: Origin.Decl Origin.EmptyData
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Generic, Show)

data Newtype = Newtype{
      name      :: Hs.Name Hs.NsTypeConstr
    , constr    :: Hs.Name Hs.NsConstr
    , field     :: Field
    , origin    :: Origin.Decl Origin.Newtype
    , instances :: Set Hs.TypeClass
    , ffiType   :: Maybe BindingSpec.FFIType
    , comment   :: Maybe HsDoc.Comment
    }
  deriving stock (Generic, Show)

data ForeignImportDecl = ForeignImportDecl{
      name       :: Hs.Name Hs.NsVar
    , parameters :: [FunctionParameter]
    , result     :: HsType
    , origName   :: C.DeclName
    , callConv   :: CallConv
    , origin     :: Origin.ForeignImport
    , comment    :: Maybe HsDoc.Comment
    , safety     :: SHs.Safety
    }
  deriving stock (Generic, Show)

data FunctionParameter = FunctionParameter{
      name    :: Maybe (Hs.Name Hs.NsVar)
    , typ     :: HsType
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Generic, Show)

data FunctionDecl = FunctionDecl{
      name       :: Hs.Name Hs.NsVar
    , parameters :: [FunctionParameter]
    , result     :: HsType
    , body       :: SHs.ClosedExpr
    , origin     :: Origin.ForeignImport
    , pragmas    :: [SHs.Pragma]
    , comment    :: Maybe HsDoc.Comment
    }
  deriving stock (Generic, Show)

data UnionGetter = UnionGetter{
      name    :: Hs.Name Hs.NsVar
    , typ     :: HsType
    , constr  :: Hs.Name Hs.NsTypeConstr
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Generic, Show)

data UnionSetter = UnionSetter{
      name    :: Hs.Name Hs.NsVar
    , typ     :: HsType
    , constr  :: Hs.Name Hs.NsTypeConstr
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Generic, Show)

data DeriveInstance = DeriveInstance{
      strategy :: Strategy HsType
    , clss     :: Hs.TypeClass
    , name     :: Hs.Name Hs.NsTypeConstr
    , comment  :: Maybe HsDoc.Comment
    }
  deriving stock (Generic, Show)

data Var = Var{
      name    :: Hs.Name Hs.NsVar
    , typ     :: SHs.ClosedType
    , expr    :: SHs.ClosedExpr
    , pragmas :: [SHs.Pragma]
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

{-------------------------------------------------------------------------------
  Variable binding
-------------------------------------------------------------------------------}

-- | Lambda abstraction
type Lambda :: (Ctx -> Star) -> (Ctx -> Star)
data Lambda t ctx = Lambda
    NameHint  -- ^ name suggestion
    (t (S ctx)) -- ^ body

deriving instance Show (t (S ctx)) => Show (Lambda t ctx)

-- | Direct application (non-applicative)
--
-- Unlike 'Ap' which uses applicative composition (<*>), DirectApply directly
-- applies a constructor to a list of expressions.
--
data Apply pure xs ctx = Apply (pure ctx) [xs ctx]
  deriving stock (Generic, Show)

-- | Applicative structure
data Ap pure xs ctx = Ap (pure ctx) [xs ctx]
  deriving stock (Generic, Show)

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

-- | Top-level declaration
type Decl :: Star
data Decl where
    DeclTypSyn               :: TypSyn              -> Decl
    DeclData                 :: SNatI n => Struct n -> Decl
    DeclEmpty                :: EmptyData           -> Decl
    DeclNewtype              :: Newtype             -> Decl
    DeclPatSyn               :: PatSyn              -> Decl
    DeclDefineInstance       :: DefineInstance      -> Decl
    DeclDeriveInstance       :: DeriveInstance      -> Decl
    DeclForeignImport        :: ForeignImportDecl   -> Decl
    DeclForeignImportDynamic :: ForeignImportDynamic -> Decl
    DeclForeignImportWrapper :: ForeignImportWrapper -> Decl
    DeclFunction             :: FunctionDecl        -> Decl
    DeclMacroExpr            :: MacroExpr           -> Decl
    DeclUnionGetter          :: UnionGetter         -> Decl
    DeclUnionSetter          :: UnionSetter         -> Decl
    DeclVar                  :: Var                 -> Decl
deriving instance Show Decl

data DefineInstance = DefineInstance{
      instanceDecl :: InstanceDecl
    , comment      :: Maybe HsDoc.Comment
    }
  deriving stock (Generic, Show)

-- | Class instance declaration (with code that /we/ generate)
type InstanceDecl :: Star
data InstanceDecl where
    InstanceStorable :: Struct n -> StorableInstance -> InstanceDecl
    InstancePrim :: Struct n -> PrimInstance -> InstanceDecl
    InstanceHasCField :: HasCFieldInstance -> InstanceDecl
    InstanceHasCBitfield :: HasCBitfieldInstance -> InstanceDecl
    InstanceHasField :: HasFieldInstance -> InstanceDecl
    InstanceHasFLAM :: Struct n -> HsType -> Int -> InstanceDecl
    InstanceCEnum ::
         Struct (S Z)
      -> HsType
      -> Map Integer (NonEmpty String)
      -> Bool  -- is sequential?
      -> InstanceDecl
    InstanceSequentialCEnum ::
         Struct (S Z)
      -> Hs.Name Hs.NsConstr
      -> Hs.Name Hs.NsConstr
      -> InstanceDecl
    InstanceCEnumShow :: Struct (S Z) -> InstanceDecl
    InstanceCEnumRead :: Struct (S Z) -> InstanceDecl
    InstanceToFunPtr   :: ToFunPtrInstance -> InstanceDecl
    InstanceFromFunPtr :: FromFunPtrInstance -> InstanceDecl

deriving instance Show InstanceDecl

-- | Macro expresson
type MacroExpr :: Star
data MacroExpr = MacroExpr {
    -- | Name of variable/function.
      name    :: Hs.Name Hs.NsVar
    -- | Type of variable/function.
    , expr    :: CheckedMacroExpr
    -- | RHS of variable/function.
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Generic, Show)

-- | RHS of a variable or function declaration.
--
-- TODO: Do we need this, or could we just use SExpr instead?
type VarDeclRHS :: Ctx -> Star
data VarDeclRHS ctx
  = VarDeclIntegral Integer HsPrimType
  | VarDeclFloat Float
  | VarDeclDouble Double
  | VarDeclChar   CExpr.Runtime.CharValue
  | VarDeclString ByteArray
  | VarDeclLambda (Lambda VarDeclRHS ctx)
  | VarDeclApp VarDeclRHSAppHead [VarDeclRHS ctx]
  | VarDeclVar (Idx ctx)
  deriving stock (Generic, Show)

-- | The function at the head of an application in the Haskell translation
-- of a C macro.
data VarDeclRHSAppHead
  -- | The translation of a built-in C infix function such as @*@ or @&&@.
  = forall arity. InfixAppHead (CExpr.DSL.MFun arity)
  -- | A function name, or the name of a function-like macro.
  | VarAppHead (Hs.Name Hs.NsVar)

deriving stock instance Show VarDeclRHSAppHead

{-------------------------------------------------------------------------------
  Pattern Synonyms
-------------------------------------------------------------------------------}

-- | Pattern synonyms
--
-- Supports pattern synonyms of the following forms:
--
-- @
-- pattern P :: T
-- pattern P = C e
-- @
--
-- and
--
-- @
-- pattern P :: T
-- pattern P = e
-- @
--
data PatSyn = PatSyn{
      name    :: Hs.Name Hs.NsConstr
    , typ     :: HsType
    , constr  :: Maybe (Hs.Name Hs.NsConstr)
    , value   :: Integer
    , origin  :: Origin.PatSyn
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Generic, Show)

{-------------------------------------------------------------------------------
  'ToFunPtr'
-------------------------------------------------------------------------------}

-- | 'ToFunPtr' instance
data ToFunPtrInstance = ToFunPtrInstance{
      typ  :: HsType
    , body :: UniqueSymbol
    }
  deriving stock (Generic, Show)

-- | A dynamic wrapper
--
-- For example, assuming @name = "foo"@ and @funType = ft@
--
-- > foreign import ccall "wrapper" foo :: ft -> IO (FunPtr ft)
--
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1620008.5.1>
data ForeignImportWrapper = ForeignImportWrapper {
      name    :: Hs.Name Hs.NsVar
    , funType :: HsType
    , origin  :: Origin.ForeignImport
    , comment :: Maybe HsDoc.Comment
    }
    deriving stock (Generic, Show)

{-------------------------------------------------------------------------------
  'FromFunPtr'
-------------------------------------------------------------------------------}

-- | 'FromFunPtr' instance
data FromFunPtrInstance = FromFunPtrInstance{
      typ  :: HsType
    , body :: UniqueSymbol
    }
  deriving stock (Generic, Show)

-- | A dynamic import
--
-- For example, assuming @name = "foo"@ and @funType = ft@
--
-- > foreign import ccall "dynamic" foo :: FunPtr ft -> ft
--
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1620008.5.1>
data ForeignImportDynamic = ForeignImportDynamic {
      name    :: Hs.Name Hs.NsVar
    , funType :: HsType
    , origin  :: Origin.ForeignImport
    , comment :: Maybe HsDoc.Comment
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
data StorableInstance = StorableInstance{
      sizeOf    :: Int
    , alignment :: Int
    , peek      :: Lambda (Ap StructCon PeekCField) EmptyCtx
    , poke      :: Lambda (Lambda (ElimStruct (Seq PokeCField))) EmptyCtx
    }
  deriving stock (Generic, Show)

-- | A call to 'peekCField', 'peekCBitfield', or 'peekByteOff'.
type PeekCField :: Ctx -> Star
data PeekCField ctx =
    PeekCField HsType (Idx ctx)
  | PeekCBitfield HsType (Idx ctx)
  | PeekByteOff (Idx ctx) Int
  deriving stock (Generic, Show)

-- | A call to 'pokeCField', 'pokeCBitfield', or 'pokeByteOff'.
type PokeCField :: Ctx -> Star
data PokeCField ctx =
    PokeCField HsType (Idx ctx) (Idx ctx)
  | PokeCBitfield HsType (Idx ctx) (Idx ctx)
  | PokeByteOff (Idx ctx) Int (Idx ctx)
  deriving stock (Generic, Show)

{-------------------------------------------------------------------------------
  'Prim'
-------------------------------------------------------------------------------}

-- | Prim instance for a struct
--
-- Generates explicit implementations of all Prim methods for structs
-- where all fields are Prim instances.
--
-- Unlike Storable which uses byte offsets and pointer operations, Prim uses
-- element indices and direct primitive operations. The key difference:
--
-- * Storable: @peekByteOff ptr byteOffset@
-- * Prim: @indexByteArray# arr# (numFields# *# i# +# fieldPos#)@
--
-- <https://hackage.haskell.org/package/primitive/docs/Data-Primitive-Types.html#t:Prim>
data PrimInstance = PrimInstance{
      -- | Total size in bytes
      sizeOf :: Int

      -- | Alignment requirement
    , alignment :: Int

      -- | @indexByteArray# :: ByteArray# -> Int# -> a@
      --
      -- Takes array and element index, returns struct by indexing each field
      -- Body: StructCon with direct field calls (no applicative)
    , indexByteArray :: Lambda (Lambda (Apply StructCon IndexByteArrayField)) EmptyCtx

      -- | @readByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)@
      --
      -- Takes array, element index, and state; returns new state and struct
    , readByteArray :: Lambda (Lambda (Lambda ReadByteArrayFields)) EmptyCtx

      -- | @writeByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s@
      --
      -- Takes array, element index, struct value, and state; returns new state
    , writeByteArray :: Lambda (Lambda (Lambda (Lambda (ElimStruct WriteByteArrayFields)))) EmptyCtx

      -- | @indexOffAddr# :: Addr# -> Int# -> a@
      --
      -- Takes address and element index, returns struct by indexing each field
      -- Body: StructCon with direct field calls (no applicative)
    , indexOffAddr :: Lambda (Lambda (Apply StructCon IndexOffAddrField)) EmptyCtx

      -- | readOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, a #)
      --
      -- Takes address, element index, and state; returns new state and struct

    , readOffAddr :: Lambda (Lambda (Lambda ReadOffAddrFields)) EmptyCtx

      -- | writeOffAddr# :: Addr# -> Int# -> a -> State# s -> State# s
      --
      -- Takes address, element index, struct value, and state; returns new state
    , writeOffAddr :: Lambda (Lambda (Lambda (Lambda (ElimStruct WriteOffAddrFields)))) EmptyCtx
    }
  deriving stock (Generic, Show)

-- | Common field metadata for indexing operations
type IndexPrimFieldData :: Ctx -> Star
data IndexPrimFieldData ctx = IndexPrimFieldData{
      typ       :: HsType  -- ^ Field type
    , arg1      :: Idx ctx -- ^ First argument variable
    , arg2      :: Idx ctx -- ^ Second argument variable
    , pos       :: Int     -- ^ Field position (0-based)
    , numFields :: Int     -- ^ Total number of fields
    }
  deriving stock (Generic, Show)

-- | Index a field from ByteArray# using indexByteArray#
--
-- For a struct with n fields at element index i, field f is at position: n*i + f
-- Example: @indexByteArray# arr# (3# *# i# +# 1#)@ for field 1 of a 3-field struct
--
type IndexByteArrayField :: Ctx -> Star
data IndexByteArrayField ctx = IndexByteArrayField{
      metadata :: IndexPrimFieldData ctx
    }
  deriving stock (Generic, Show)

-- | Index a field from Addr# using indexOffAddr#
--
-- For a struct with n fields at element index i, field f is at position: n*i + f
-- Example: @indexOffAddr# addr# (3# *# i# +# 1#)@ for field 1 of a 3-field struct
type IndexOffAddrField :: Ctx -> Star
data IndexOffAddrField ctx = IndexOffAddrField{
      metadata :: IndexPrimFieldData ctx
    }
  deriving stock (Generic, Show)

-- | Common field metadata for read operations
type ReadPrimFieldsData :: Ctx -> Star
data ReadPrimFieldsData ctx = ReadPrimFieldsData{
      fields    :: [(HsType, Int)] -- ^ Fields: (type, position)
    , arg1      :: Idx ctx         -- ^ First argument variable
    , arg2      :: Idx ctx         -- ^ Second argument variable
    , arg3      :: Idx ctx         -- ^ Third argument variable
    , numFields :: Int             -- ^ Total number of fields
    }
  deriving stock (Generic, Show)

-- | Read fields from MutableByteArray# with state threading
--
-- Generates nested case expressions to thread State# through multiple reads:
--
-- > case readByteArray# arr# (n# *# i# +# 0#) s0 of
-- >   (# s1, x #) -> case readByteArray# arr# (n# *# i# +# 1#) s1 of
-- >     (# s2, y #) -> (# s2, Struct x y #)
--
type ReadByteArrayFields :: Ctx -> Star
data ReadByteArrayFields ctx = ReadByteArrayFields{
      metadata :: ReadPrimFieldsData ctx
    }
  deriving stock (Generic, Show)

-- | Read fields from Addr# with state threading
--
-- Generates nested case expressions to thread State# through multiple reads:
--
-- > case readOffAddr# addr# (n# *# i# +# 0#) s0 of
-- >   (# s1, x #) -> case readOffAddr# addr# (n# *# i# +# 1#) s1 of
-- >     (# s2, y #) -> (# s2, Struct x y #)
--
type ReadOffAddrFields :: Ctx -> Star
data ReadOffAddrFields ctx = ReadOffAddrFields{
      metadata :: ReadPrimFieldsData ctx
    }
  deriving stock (Generic, Show)

-- | Common field metadata for write operations
data WritePrimFieldsData ctx = WritePrimFieldsData{
      fields    :: [(HsType, Int, Idx ctx)] -- ^ Fields: (type, position, value variable)
    , arg1      :: Idx ctx                  -- ^ First argument variable
    , arg2      :: Idx ctx                  -- ^ Second argument variable
    , arg3      :: Idx ctx                  -- ^ Third argument variable
    , numFields :: Int                      -- ^ Total number of fields
    }
  deriving stock (Generic, Show)

-- | Write fields to MutableByteArray# with state threading
--
-- Generates sequential writes that thread State# through:
--
-- > case writeByteArray# arr# (n# *# i# +# 0#) x s0 of
-- >   s1 -> writeByteArray# arr# (n# *# i# +# 1#) y s1
--
type WriteByteArrayFields :: Ctx -> Star
data WriteByteArrayFields ctx = WriteByteArrayFields {
      metadata :: WritePrimFieldsData ctx
    }
  deriving stock (Generic, Show)

-- | Write fields to Addr# with state threading
--
-- Generates sequential writes that thread State# through:
--
-- > case writeOffAddr# addr# (n# *# i# +# 0#) x s0 of
-- >   s1 -> writeOffAddr# addr# (n# *# i# +# 1#) y s1
--
type WriteOffAddrFields :: Ctx -> Star
data WriteOffAddrFields ctx = WriteOffAddrFields {
      metadata :: WritePrimFieldsData ctx
    }
  deriving stock (Generic, Show)

{-------------------------------------------------------------------------------
  'HasCField'
-------------------------------------------------------------------------------}

-- | 'HasCField' instance
data HasCFieldInstance = HasCFieldInstance {
      -- | The haskell type of the parent C object
      parentType :: HsType

      -- | The name of the field
    , fieldName :: Hs.Name Hs.NsVar

      -- | The haskell type of the field
    , cFieldType :: HsType

      -- | The offset (in number of bytes) of the field wrt the parent object
    , fieldOffset :: Int
    }
  deriving stock (Generic, Show)

{-------------------------------------------------------------------------------
  'HasCBitfield'
-------------------------------------------------------------------------------}

-- | 'HasCBitfield' instance
data HasCBitfieldInstance = HasCBitfieldInstance {
      -- | The haskell type of the parent C object
      parentType :: HsType

      -- | The name of the bit-field
    , fieldName :: Hs.Name Hs.NsVar

      -- | The haskell type of the bit-field
    , cBitfieldType :: HsType

      -- | The offset (in number of bit) of the bit-field wrt the parent object
    , bitOffset :: Int

      -- | The width (in number of bits) of the bit-field.
    , bitWidth :: Int
    }
  deriving stock (Generic, Show)

{-------------------------------------------------------------------------------
  'HasField'
-------------------------------------------------------------------------------}

-- | 'HasField' instance (via a 'HasCField' or 'HasCBitfield' instance).
data HasFieldInstance = HasFieldInstance {
      -- | The haskell type of the parent C object
      parentType :: HsType

      -- | The name of the (bit-)field
    , fieldName :: Hs.Name Hs.NsVar

      -- | The haskell type of the (bit-)field
    , fieldType :: HsType

      -- | Implement the instance via a 'HasCField' or 'HasCBitfield' instance.
    , deriveVia :: HasFieldInstanceVia
    }
  deriving stock (Generic, Show)

-- | See 'hasFieldInstanceVia'
data HasFieldInstanceVia = ViaHasCField | ViaHasCBitfield
  deriving stock (Generic, Show)

{-------------------------------------------------------------------------------
  Statements
-------------------------------------------------------------------------------}

-- | Simple sequential composition (no bindings)
newtype Seq t ctx = Seq [t ctx]
  deriving stock (Generic, Show)

{-------------------------------------------------------------------------------
  Type synonyms
-------------------------------------------------------------------------------}

-- | Type synonyms
--
-- At the moment, we do not support type variables.
data TypSyn = TypSyn{
      name    :: Hs.Name Hs.NsTypeConstr
    , typ     :: HsType
      -- TODO https://github.com/well-typed/hs-bindgen/issues/1448: Temporary
      -- origin; will be gone.
    , origin  :: Origin.Decl Origin.EmptyData
    , comment :: Maybe HsDoc.Comment
    }
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
