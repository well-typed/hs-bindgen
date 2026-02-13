-- | Internal AST as it is constructed step by step in the frontend
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.AST.Decl qualified as C
module HsBindgen.Frontend.AST.Decl (
    TranslationUnit(..)
    -- * Declarations
  , Decl(..)
  , Availability(..)
  , DeclInfo(..)
  , HeaderInfo(..)
  , FieldInfo(..)
  , DeclKind(..)
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
    -- ** Comments
  , Comment(..)
  , CommentRef(..)
  ) where

import Prelude hiding (Enum)
import Prelude qualified as P

import Clang.HighLevel.Documentation qualified as CDoc
import Clang.HighLevel.Types

import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.RootHeader (HashIncludeArg)
import HsBindgen.Imports
import HsBindgen.Language.C (PrimType)

{-------------------------------------------------------------------------------
  Declarations

  NOTE: Struct and union fields, as well as enum constants, have their /own/
  'SingleLoc' (in addition to the 'SingleLoc' of the enclosing declaration).
-------------------------------------------------------------------------------}

data TranslationUnit p = TranslationUnit{
      -- | Declarations in the unit
      --
      -- Declarations from all headers that we have processed. Passes may remove
      -- some declarations. For example,
      --
      -- * The 'Parse' pass filters out declarations not matching the selection
      --   predicate (without program slicing).
      --
      -- * If program slicing is enabled, the 'Slice' pass filters selected
      --   declarations and their transitive dependencies.
      --
      -- * The 'ResolveBindingSpecs' pass removes declarations for which we have
      --   existing external bindings, as well as declarations omitted by a
      --   prescriptive binding specification.
      decls :: [Decl p]

      -- | Include graph
      --
      -- This is used to declare TH dependencies.
      --
      -- It can also be useful for users to see this graph, as it may provide
      -- insight into the binding generation process. For example, suppose we
      -- have a large library (say Gtk), with a few main entry points (for which
      -- we should generate separate Haskell modules) and a core of "common"
      -- definitions; it may be quite useful to look at the include graph to
      -- figure out what this set of "core" headers is.
    , includeGraph :: IncludeGraph

      -- | Pass-specific annotation
    , ann :: Ann "TranslationUnit" p
    }
  deriving stock (Generic)

data Decl p = Decl {
      info :: DeclInfo p
    , kind :: DeclKind p
    , ann  :: Ann "Decl" p
    }
  deriving stock (Generic)

-- | Availability of declarations.
--
-- See 'Clang.LowLevel.Core.CXAvailabilityKind'.
data Availability =
    -- | Available and recommended for use.
    Available
    -- | Available but deprecated; may result in compilation error.
  | Deprecated
    -- | Unavailable or unaccessible; results in compilation error.
  | Unavailable
  deriving stock (Show, Eq, Ord, P.Enum, Bounded, Generic)

data DeclInfo p = DeclInfo{
      loc          :: SingleLoc
    , id           :: Id p
    , headerInfo   :: HeaderInfo
    , availability :: Availability
    , comment      :: Maybe (Comment p)
    }
  deriving stock (Generic)

data HeaderInfo = HeaderInfo{
      -- | User-specified headers that provide the declaration
      --
      -- Note that the declaration may not be in this header directly, but in
      -- one of its (transitive) includes.
      mainHeaders :: NonEmpty HashIncludeArg

      -- | @#include@ argument used to include the file where the declaration is
      -- actually declared
    , includeArg :: HashIncludeArg
    }
  deriving stock (Show, Eq, Generic)

data FieldInfo p = FieldInfo {
      loc     :: SingleLoc
    , name    :: ScopedName p
    , comment :: Maybe (Comment p)
    }
  deriving stock (Generic)

data DeclKind p =
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
  | DeclOpaque
  | DeclMacro (MacroBody p)
  | DeclFunction (Function p)
    -- | A global variable, whether it be declared @extern@, @static@ or neither.
  | DeclGlobal (C.Type p)

data Struct p = Struct {
      sizeof    :: Int
    , alignment :: Int
    , fields    :: [StructField p]
    , flam      :: Maybe (StructField p) -- ^ FLAM element type, if any
    , ann       :: Ann "Struct" p
    }
  deriving stock (Generic)

data StructField p = StructField {
      info   :: FieldInfo p
    , typ    :: C.Type p
    , offset :: Int     -- ^ Offset in bits
    , width  :: Maybe Int
    , ann    :: Ann "StructField" p
    }
  deriving stock (Generic)

data Union p = Union {
      sizeof    :: Int
    , alignment :: Int
    , fields    :: [UnionField p]
    , ann       :: Ann "Union" p
    }
  deriving stock (Generic)

data UnionField p = UnionField {
      info :: FieldInfo p
    , typ  :: C.Type p
    , ann  :: Ann "UnionField" p
    }
  deriving stock (Generic)

data Typedef p = Typedef {
      typ :: C.Type p
    , ann :: Ann "Typedef" p
    }
  deriving stock (Generic)

data Enum p = Enum {
      typ       :: C.Type p
    , sizeof    :: Int
    , alignment :: Int
    , constants :: [EnumConstant p]
    , ann       :: Ann "Enum" p
    }
  deriving stock (Generic)

data EnumConstant p = EnumConstant {
      info  :: FieldInfo p
    , value :: Integer
    }
  deriving stock (Generic)

-- | Anonymous Enum Constant
--
-- This represents an anonymous enum constant (e.g., from @enum { FOO, BAR }@)
-- that will be rendered as a pattern synonym in Haskell (e.g., @pattern fOO :: CUInt@)
--
data AnonEnumConstant p = AnonEnumConstant{
      typ       :: PrimType
    , constant  :: EnumConstant p
    }
  deriving stock (Generic, Show, Eq)

data Function p = Function {
      args  :: [FunctionArg p]
    , res   :: C.Type p
    , attrs :: FunctionAttributes
    , ann   :: Ann "Function" p
    }
  deriving stock (Generic)

data FunctionArg p = FunctionArg {
      name :: Maybe (ScopedName p)
    , typ  :: C.Type p
    }
    deriving stock (Generic)

-- | Function attributes specify properties for C functions.
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
  deriving stock (Show, Eq, Ord, Generic)

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
  deriving stock (Show, Eq, Ord, Generic)

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

{-------------------------------------------------------------------------------
  Comments
-------------------------------------------------------------------------------}

newtype Comment p = Comment{
      doxygen :: CDoc.Comment (CommentRef p)
    }
  deriving stock (Generic)

-- | Cross-reference in a Doxygen comment
--
-- Doxygen references are just strings; in particular, they do not distinguish
-- between namespaces (i.e., @struct foo@ is simply referred to as @foo@). In
-- 'MangleNames' we will /search/ for a matching name and set the 'Id'
-- accordingly, so that we can generate an approprate reference in the Haddocks.
data CommentRef p = CommentRef Text (Maybe (Id p))

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

deriving stock instance IsPass p => Show (Decl             p)
deriving stock instance IsPass p => Show (DeclInfo         p)
deriving stock instance IsPass p => Show (FieldInfo        p)
deriving stock instance IsPass p => Show (DeclKind         p)
deriving stock instance IsPass p => Show (Enum             p)
deriving stock instance IsPass p => Show (EnumConstant     p)
deriving stock instance IsPass p => Show (Function         p)
deriving stock instance IsPass p => Show (FunctionArg      p)
deriving stock instance IsPass p => Show (Struct           p)
deriving stock instance IsPass p => Show (StructField      p)
deriving stock instance IsPass p => Show (TranslationUnit  p)
deriving stock instance IsPass p => Show (Typedef          p)
deriving stock instance IsPass p => Show (Union            p)
deriving stock instance IsPass p => Show (UnionField       p)
deriving stock instance IsPass p => Show (Comment          p)
deriving stock instance IsPass p => Show (CommentRef       p)

deriving stock instance IsPass p => Eq (Comment          p)
deriving stock instance IsPass p => Eq (DeclKind         p)
deriving stock instance IsPass p => Eq (Enum             p)
deriving stock instance IsPass p => Eq (EnumConstant     p)
deriving stock instance IsPass p => Eq (FieldInfo        p)
deriving stock instance IsPass p => Eq (Function         p)
deriving stock instance IsPass p => Eq (FunctionArg      p)
deriving stock instance IsPass p => Eq (CommentRef       p)
deriving stock instance IsPass p => Eq (Struct           p)
deriving stock instance IsPass p => Eq (StructField      p)
deriving stock instance IsPass p => Eq (Typedef          p)
deriving stock instance IsPass p => Eq (Union            p)
deriving stock instance IsPass p => Eq (UnionField       p)
