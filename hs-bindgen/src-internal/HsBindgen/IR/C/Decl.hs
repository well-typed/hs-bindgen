-- | C declarations
--
-- This module should only be used within the @HsBindgen.IR@ hierarchy.  From
-- outside the @HsBindgen.IR@ hierarchy, "HsBindgen.IR.C" should be used.
--
-- Within @HsBindgen.IR@, all modules aside from "HsBindgen.IR.C" should import
-- this module qualified for consistency.
--
-- > import HsBindgen.IR.C.Decl qualified as C
module HsBindgen.IR.C.Decl (
    -- * Declarations
    Decl(..)
  , Availability(..)
  , EnclosingRef(..)
  , DeclInfo(..)
  , HeaderInfo(..)
  , FieldInfo(..)
  , DeclKind(..)
  , OpaqueSize(..)
  , Struct(..)
  , StructField(..)
  , Union(..)
  , UnionField(..)
  , Typedef(..)
  , Enum(..)
  , EnumConstant(..)
  , AnonEnumConstant(..)
  , Function(..)
  , FunctionArg(..)
  , FunctionAttributes(..)
  , FunctionPurity(..)
  , decideFunctionPurity
  , Global(..)
    -- ** Comments
  , Comment(..)
  , CommentRef(..)
  ) where

import Prelude hiding (Enum)
import Prelude qualified as P

import Clang.HighLevel.Types

import HsBindgen.Imports
import HsBindgen.IR.C.HashIncludeArg qualified as C
import HsBindgen.IR.C.Naming qualified as C
import HsBindgen.IR.C.Type qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Language.C (PrimType)
import HsBindgen.Macro.Type

import Doxygen.Parser.Types qualified as Doxy

{-------------------------------------------------------------------------------
  Declarations

  NOTE: Struct and union fields, as well as enum constants, have their /own/
  'SingleLoc' (in addition to the 'SingleLoc' of the enclosing declaration).
-------------------------------------------------------------------------------}

data Decl l (p :: Pass) = Decl {
      info :: DeclInfo p
    , kind :: DeclKind l p
    , ann  :: Ann "Decl" p
    }
  deriving stock (Generic)

-- | Availability of declarations.
--
-- See 'Clang.LowLevel.Core.CXAvailabilityKind'.
data Availability =
    -- | Available and recommended for use
    Available
    -- | Available but deprecated; may result in compilation error
  | Deprecated
    -- | Unavailable or unaccessible; results in compilation error
  | Unavailable
  deriving stock (Bounded, Eq, Generic, Ord, P.Enum, Show)

-- | Reference to enclosing declaration
data EnclosingRef p =
    EnclosingRef (Id p)
  | UnusableEnclosingRef C.DeclId

deriving stock instance (Eq   (Id p)) => Eq   (EnclosingRef p)
deriving stock instance (Ord  (Id p)) => Ord  (EnclosingRef p)
deriving stock instance (Show (Id p)) => Show (EnclosingRef p)

data DeclInfo (p :: Pass) = DeclInfo{
      loc           :: SingleLoc
    , id            :: Id p
    -- | Sequence number
    --
    -- Declarations with lower sequence numbers come before declarations with
    -- higher sequence numbers in the translation unit. We can populate sequence
    -- numbers only with Clang version 20.1 or newer.
    , seqNr         :: Maybe Natural
    , headerInfo    :: HeaderInfo
    , availability  :: Availability
    , comment       :: CommentDecl p
      -- ^ Doxygen comment for this declaration
      --
      -- Pre-'HsBindgen.Frontend.Pass.EnrichComments.IsPass.EnrichComments'
      -- passes have @CommentDecl p = ()@: the type system guarantees comments
      -- cannot exist. Post-@EnrichComments@ passes have
      -- @CommentDecl p = Maybe (Comment p)@.
    , enclosing :: [EnclosingRef p]
      -- ^ List of enclosing declarations, if this declaration is nested.
      --
      -- Set during parsing for declarations nested inside another declaration
      -- (e.g., anonymous or named structs\/unions inside an enclosing
      -- struct\/union). Empty for top-level declarations.
      --
      -- Used by 'EnrichComments' to build doxygen-qualified names and look up
      -- enclosing field comments in the doxygen state.
    }
  deriving stock (Generic)

data HeaderInfo = HeaderInfo{
      -- | User-specified headers that provide the declaration
      --
      -- Note that the declaration may not be in this header directly, but in
      -- one of its (transitive) includes.
      mainHeaders :: NonEmpty C.HashIncludeArg

      -- | @#include@ argument used to include the file where the declaration is
      -- actually declared
    , includeArg :: C.HashIncludeArg

      -- | Raw macro used as a @#include@ argument, when applicable
      --
      -- For example, @#include FOO@ would record the macro text @FOO@ here.
    , includeMacroArg :: Maybe Text
    }
  deriving stock (Show, Eq, Generic)

data FieldInfo (p :: Pass) = FieldInfo {
      loc     :: SingleLoc
    , name    :: ScopedName p
    , comment :: CommentDecl p
    }
  deriving stock (Generic)

data DeclKind l p =
    DeclStruct (Struct p)
  | DeclUnion (Union p)
  | DeclTypedef (Typedef p)
  | DeclEnum (Enum p)
    -- | Anonymous Enum Constant
    --
    -- Represents individual constants from an anonymous enum (e.g., @enum { FOO, BAR }@)
    -- as separate pattern synonym declarations.
  | DeclAnonEnumConstant (AnonEnumConstant p)
    -- | Opaque type
    --
    -- When parsing, a C @struct@, @union@, or @enum@ may be opaque.  Users may
    -- specify any kind of type to be opaque using a prescriptive binding
    -- specification, however, including @typedef@ types.
    --
    -- The size and alignment are retained when known (i.e. when a /complete/ C
    -- type is given the @emptydata@ representation), and 'Nothing' when the type
    -- is genuinely opaque in C (e.g. a forward declaration).
  | DeclOpaque (Maybe OpaqueSize)
  | DeclMacro (MacroBody p l)
  | DeclFunction (Function p)
    -- | A global variable, whether it be declared @extern@, @static@ or neither.
  | DeclGlobal (Global p)

-- | Size and alignment of an opaque type, when known
--
-- A complete C type given the @emptydata@ representation retains its size and
-- alignment here, which is what enables generating a @StaticSize@ instance for
-- the otherwise field-less Haskell type.
data OpaqueSize = OpaqueSize {
      sizeof    :: Int
    , alignment :: Int
    }
  deriving stock (Show, Eq, Generic)

data Struct (p :: Pass) = Struct {
      sizeof    :: Int
    , alignment :: Int
    , fields    :: [StructField p]
    , flam      :: Maybe (StructField p) -- ^ FLAM element type, if any
    , ann       :: Ann "Struct" p
    }
  deriving stock (Generic)

data StructField (p :: Pass) = StructField {
      info   :: FieldInfo p
    , typ    :: C.Type p
    , offset :: Int     -- ^ Offset in bits
    , width  :: Maybe Int
    , ann    :: Ann "StructField" p
    }
  deriving stock (Generic)

data Union (p :: Pass) = Union {
      sizeof    :: Int
    , alignment :: Int
    , fields    :: [UnionField p]
    , ann       :: Ann "Union" p
    }
  deriving stock (Generic)

data UnionField (p :: Pass) = UnionField {
      info :: FieldInfo p
    , typ  :: C.Type p
    , ann  :: Ann "UnionField" p
    }
  deriving stock (Generic)

data Typedef (p :: Pass) = Typedef {
      typ :: C.Type p
    , ann :: Ann "Typedef" p
    }
  deriving stock (Generic)

data Enum (p :: Pass) = Enum {
      typ       :: C.Type p
    , sizeof    :: Int
    , alignment :: Int
    , constants :: [EnumConstant p]
    , ann       :: Ann "Enum" p
    }
  deriving stock (Generic)

data EnumConstant (p :: Pass) = EnumConstant {
      info  :: FieldInfo p
    , value :: Integer
    }
  deriving stock (Generic)

-- | Anonymous Enum Constant
--
-- This represents an anonymous enum constant (e.g., from @enum { FOO, BAR }@)
-- that will be rendered as a pattern synonym in Haskell (e.g., @pattern fOO :: CUInt@)
data AnonEnumConstant (p :: Pass) = AnonEnumConstant {
      typ       :: PrimType
    , constant  :: EnumConstant p
    }
  deriving stock (Generic)

data Function (p :: Pass) = Function {
      args  :: [FunctionArg p]
    , res   :: C.Type p
    , attrs :: FunctionAttributes
    , ann   :: Ann "Function" p
    }
  deriving stock (Generic)

data FunctionArg (p :: Pass) = FunctionArg {
      name   :: Maybe (ScopedName p)
    , argTyp :: C.TypeFunArg p
    }
    deriving stock (Generic)

-- | Function attributes specify properties for C functions
--
-- Function attributes may help the C compiler. In addition, @hs-bindgen@ can in
-- some cases modify the bindings it generates based on these function
-- attributes.
--
-- This type is an interpretation of the syntactic function attributes that are
-- put on C functions. For example, a C function can have multiple @pure@
-- and\/or @const@ attributes, but we interpret these attributes together as a
-- 'FunctionPurity', see 'decideFunctionPurity'.
data FunctionAttributes = FunctionAttributes {
      purity :: FunctionPurity
    }
  deriving stock (Eq, Generic, Ord, Show)

-- | The diagnosed purity of a C function determines whether to include 'IO' in
-- its foreign import.
data FunctionPurity =
    -- | C functions that are impure in the Haskell sense of the word.
    --
    -- C functions without a @const@ or @pure@ function attribute are
    -- Haskell-impure. They do not guarantee to return the same output for the
    -- same inputs. Foreign imports of such Haskell-impure functions can /not/
    -- omit the 'IO' in their return type.
    ImpureFunction
    -- | C functions that are pure in the Haskell sense of the word.
    --
    -- C functions with a @const@ function attribute are Haskell-pure. They
    -- always return the same output for the same inputs. Foreign imports of
    -- such Haskell-pure C functions can omit the 'IO' in their return type.
    --
    -- > int square (int) __attribute__ ((const));
    --
    -- As far as the @hs-bindgen@ authors are aware, @clang@\/@gcc@ do /not
    -- always/ diagnose whether C functions with a @const@ attribute satisfy all
    -- the requirements imposed by the attribute. If a C function has a @const@
    -- attribute when it should not, then it is arguably a bug in the C library
    -- and not in @hs-bindgen@.
    --
    -- If C functions have both @const@ and @pure@ attributes, then we always
    -- pick @const@ over @pure@, because @const@ is the stronger attribute of
    -- the two.
    --
    -- <https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html#index-const-function-attribute>
  | HaskellPureFunction
    -- | C functions that are pure in the C sense of the word.
    --
    -- C functions with a @pure@ function attribute are C-pure. C-pure is
    -- different from Haskell-pure, in that C-pure functions only return the
    -- same output for the same input as long as the /the state of the program
    -- observable by the C function did not change/. In the @hash@ example
    -- below, the observable state includes the contents of the input array
    -- itself. Such C-pure functions may read from pointers, and since the
    -- contents of pointers can change between invocations of the function,
    -- foreign imports of such C-pure C functions can /not/ omit the 'IO' in
    -- their return type.
    --
    -- > int hash (char *) __attribute__ ((pure));
    --
    -- Note that uses of a C-pure function can sometimes be safely encapsulated
    -- with @unsafePerformIO@ to obtain a Haskell-pure function. For example:
    --
    -- > unsafePerformIO $ withCString "abc" hash
    --
    -- As far as the @hs-bindgen@ authors are aware, @clang@\/@gcc@ do /not
    -- always/ diagnose whether C functions with a @pure@ attribute satisfy all
    -- the requirements imposed by the attribute. If a C function has a @pure@
    -- attribute when it should not, then it is arguably a bug in the C library
    -- and not in @hs-bindgen@.
    --
    -- If C functions have both @const@ and @pure@ attributes, then we always
    -- pick @const@ over @pure@, because @const@ is the stronger attribute of
    -- the two.
    --
    -- <https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html#index-pure-function-attribute>
  | CPureFunction
  deriving stock (Eq, Generic, Ord, Show)

decideFunctionPurity :: [FunctionPurity] -> FunctionPurity
decideFunctionPurity = foldr prefer ImpureFunction
  where
    prefer HaskellPureFunction _                   = HaskellPureFunction
    prefer _                   HaskellPureFunction = HaskellPureFunction
    prefer CPureFunction       _                   = CPureFunction
    prefer _                   CPureFunction       = CPureFunction
    prefer _                   _                   = ImpureFunction

    -- In case we add new constructors, this case expression throsw a compiler
    -- error, which should hopefully indicate to the reader that
    -- 'decideFunctionPurity' has to be updated.
    _coveredAllCases' = \case
      ImpureFunction -> ()
      HaskellPureFunction -> ()
      CPureFunction -> ()

data Global (p :: Pass) = Global {
      typ :: C.Type p
    , ann :: Ann "Global" p
    }
  deriving stock (Generic)

{-------------------------------------------------------------------------------
  Comments
-------------------------------------------------------------------------------}

newtype Comment p = Comment{
      doxygen :: Doxy.Comment (CommentRef p)
    }
  deriving stock (Generic)

-- | Cross-reference in a Doxygen comment
--
-- The 'Doxy.RefKind' from the Doxygen XML @kindref@ attribute narrows the
-- search in 'HsBindgen.Frontend.Pass.MangleNames.IsPass.MangleNames': compounds
-- (struct\/union) are looked up in the type constructor namespace, members
-- (function\/typedef\/macro) in the variable and type constructor namespaces.
data CommentRef p = CommentRef Text (Maybe (Id p)) (Maybe Doxy.RefKind)

{-------------------------------------------------------------------------------
  Eq and Show instances
-------------------------------------------------------------------------------}

deriving stock instance IsPass p => Eq (AnonEnumConstant p)
deriving stock instance IsPass p => Eq (Comment          p)
deriving stock instance IsPass p => Eq (CommentRef       p)
deriving stock instance IsPass p => Eq (DeclInfo         p)
deriving stock instance IsPass p => Eq (Enum             p)
deriving stock instance IsPass p => Eq (EnumConstant     p)
deriving stock instance IsPass p => Eq (FieldInfo        p)
deriving stock instance IsPass p => Eq (Function         p)
deriving stock instance IsPass p => Eq (FunctionArg      p)
deriving stock instance IsPass p => Eq (Global           p)
deriving stock instance IsPass p => Eq (Struct           p)
deriving stock instance IsPass p => Eq (StructField      p)
deriving stock instance IsPass p => Eq (Typedef          p)
deriving stock instance IsPass p => Eq (Union            p)
deriving stock instance IsPass p => Eq (UnionField       p)

deriving stock instance IsPass p => Show (AnonEnumConstant p)
deriving stock instance IsPass p => Show (Comment          p)
deriving stock instance IsPass p => Show (CommentRef       p)
deriving stock instance IsPass p => Show (DeclInfo         p)
deriving stock instance IsPass p => Show (Enum             p)
deriving stock instance IsPass p => Show (EnumConstant     p)
deriving stock instance IsPass p => Show (FieldInfo        p)
deriving stock instance IsPass p => Show (Function         p)
deriving stock instance IsPass p => Show (FunctionArg      p)
deriving stock instance IsPass p => Show (Global           p)
deriving stock instance IsPass p => Show (Struct           p)
deriving stock instance IsPass p => Show (StructField      p)
deriving stock instance IsPass p => Show (Typedef          p)
deriving stock instance IsPass p => Show (Union            p)
deriving stock instance IsPass p => Show (UnionField       p)

deriving stock instance (HasMacroTypes l, IsPass p) => Eq (DeclKind l p)

deriving stock instance (HasMacroTypes l, IsPass p) => Show (Decl     l p)
deriving stock instance (HasMacroTypes l, IsPass p) => Show (DeclKind l p)

{-------------------------------------------------------------------------------
  CoercePass instances
-------------------------------------------------------------------------------}

instance (
      CoercePass DeclInfo p p'
    , CoercePass (DeclKind l) p p'
    , Ann "Decl" p ~ Ann "Decl" p'
    ) => CoercePass (Decl l) p p' where
  coercePass decl = Decl{
        info = coercePass decl.info
      , kind = coercePass decl.kind
      , ann  = decl.ann
      }

instance (CoercePassId p p') => CoercePass EnclosingRef p p' where
    coercePass = \case
      EnclosingRef x ->
        EnclosingRef (coercePassId (Proxy @'(p, p')) x)
      UnusableEnclosingRef x ->
        UnusableEnclosingRef x

instance (
      CoercePassId p p'
    , CoercePassCommentDecl p p'
    ) => CoercePass DeclInfo p p' where
  coercePass info = DeclInfo{
        loc          = info.loc
      , id           = coercePassId (Proxy @'(p, p')) info.id
      , seqNr        = info.seqNr
      , headerInfo   = info.headerInfo
      , availability = info.availability
      , comment      = coercePassCommentDecl (Proxy @'(p, p')) info.comment
      , enclosing    = map coercePass info.enclosing
      }

instance (
      CoercePassCommentDecl p p'
    , ScopedName p ~ ScopedName p'
    ) => CoercePass FieldInfo p p' where
  coercePass info = FieldInfo{
        comment = coercePassCommentDecl (Proxy @'(p, p')) info.comment
      , name    = info.name
      , loc     = info.loc
      }

instance (
       CoercePass Struct   p p'
     , CoercePass Enum     p p'
     , CoercePass Union    p p'
     , CoercePass Typedef  p p'
     , CoercePass Function p p'
     , CoercePass Global   p p'
     , CoercePass AnonEnumConstant p p'
     , CoercePassMacroBody p p'
     ) => CoercePass (DeclKind l) p p' where
  coercePass = \case
      DeclStruct           x -> DeclStruct           $ coercePass x
      DeclUnion            x -> DeclUnion            $ coercePass x
      DeclTypedef          x -> DeclTypedef          $ coercePass x
      DeclEnum             x -> DeclEnum             $ coercePass x
      DeclAnonEnumConstant x -> DeclAnonEnumConstant $ coercePass x
      DeclFunction         x -> DeclFunction         $ coercePass x
      DeclGlobal           x -> DeclGlobal           $ coercePass x
      DeclMacro            x -> DeclMacro            $ coercePassMacroBody (Proxy @'(p, p')) x
      DeclOpaque        mSize -> DeclOpaque mSize

instance (
      CoercePass StructField p p'
    , Ann "Struct" p ~ Ann "Struct" p'
    ) => CoercePass Struct p p' where
  coercePass struct = Struct{
        fields    = coercePass <$> struct.fields
      , flam      = coercePass <$> struct.flam
      , sizeof    = struct.sizeof
      , alignment = struct.alignment
      , ann       = struct.ann
      }

instance (
      CoercePass C.Type p p'
    , CoercePassCommentDecl p p'
    , ScopedName p ~ ScopedName p'
    , Ann "StructField" p ~ Ann "StructField" p'
    ) => CoercePass StructField p p' where
  coercePass field = StructField{
        info   = coercePass field.info
      , typ    = coercePass field.typ
      , offset = field.offset
      , width  = field.width
      , ann    = field.ann
      }

instance (
      CoercePass UnionField p p'
    , Ann "Union" p ~ Ann "Union" p'
    ) => CoercePass Union p p' where
  coercePass union = Union{
        fields    = coercePass <$> union.fields
      , sizeof    = union.sizeof
      , alignment = union.alignment
      , ann       = union.ann
      }

instance (
      CoercePass C.Type p p'
    , CoercePassCommentDecl p p'
    , ScopedName p ~ ScopedName p'
    , Ann "UnionField" p ~ Ann "UnionField" p'
    ) => CoercePass UnionField p p' where
  coercePass field = UnionField{
        info = coercePass field.info
      , typ  = coercePass field.typ
      , ann  = field.ann
      }

instance (
      CoercePass C.Type p p'
    , Ann "Typedef" p ~ Ann "Typedef" p'
    ) => CoercePass Typedef p p' where
  coercePass typedef = Typedef{
        typ = coercePass typedef.typ
      , ann = typedef.ann
      }

instance (
       CoercePass C.Type p p'
     , CoercePass EnumConstant p p'
     , Ann "Enum" p ~ Ann "Enum" p'
     ) => CoercePass Enum p p' where
  coercePass enum = Enum{
        typ       = coercePass enum.typ
      , constants = coercePass <$> enum.constants
      , sizeof    = enum.sizeof
      , alignment = enum.alignment
      , ann       = enum.ann
      }

instance (
      CoercePassCommentDecl p p'
    , ScopedName p ~ ScopedName p'
    ) => CoercePass EnumConstant p p' where
  coercePass constant = EnumConstant{
        info  = coercePass constant.info
      , value = constant.value
      }

instance (
       CoercePass EnumConstant p p'
     , Ann "PatternSynonym" p ~ Ann "PatternSynonym" p'
     ) => CoercePass AnonEnumConstant p p' where
  coercePass (AnonEnumConstant typ' constant') = AnonEnumConstant{
        typ      = typ'
      , constant = coercePass constant'
      }

instance (
      CoercePassId p p'
    , CoercePassMacroId p p'
    , CoercePassMacroUnderlying p p'
    , CoercePassAnn "TypeFunArg" p p'
    , ScopedName p ~ ScopedName p'
    , ExtBinding p ~ ExtBinding p'
    , Ann "Function" p ~ Ann "Function" p'
    ) => CoercePass Function p p' where
  coercePass function = Function{
        args  = map coercePass function.args
      , res   = coercePass function.res
      , attrs = function.attrs
      , ann   = function.ann
      }

instance (
      CoercePassId p p'
    , CoercePassMacroId p p'
    , CoercePassMacroUnderlying p p'
    , CoercePassAnn "TypeFunArg" p p'
    , ScopedName p ~ ScopedName p'
    , ExtBinding p ~ ExtBinding p'
    ) => CoercePass FunctionArg p p' where
  coercePass functionArg = FunctionArg{
        name   = functionArg.name
      , argTyp = coercePass functionArg.argTyp
      }

instance (
      CoercePassId p p'
    , CoercePassMacroId p p'
    , CoercePassMacroUnderlying p p'
    , CoercePassAnn "TypeFunArg" p p'
    , ScopedName p ~ ScopedName p'
    , ExtBinding p ~ ExtBinding p'
    , Ann "Global" p ~ Ann "Global" p'
    ) => CoercePass Global p p' where
  coercePass global = Global{
        typ = coercePass global.typ
      , ann = global.ann
      }

instance (
      CoercePass Doxy.Comment (CommentRef p) (CommentRef p')
    ) => CoercePass Comment p p' where
  coercePass (Comment c) = Comment (coercePass c)

instance (
      CoercePassId p p'
    ) => CoercePass Doxy.Comment (CommentRef p) (CommentRef p') where
  coercePass comment = fmap coercePass comment

instance (
      CoercePassId p p'
    ) => CoercePass CommentRef p p' where
  coercePass (CommentRef c hs k) =
      CommentRef c (coercePassId (Proxy @'(p, p')) <$> hs) k
