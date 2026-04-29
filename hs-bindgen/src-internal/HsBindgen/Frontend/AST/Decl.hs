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
  , EnclosingRef(..)
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
  , Global(..)
    -- ** Comments
  , Comment(..)
  , CommentRef(..)
  ) where

import Prelude hiding (Enum)
import Prelude qualified as P

import Clang.HighLevel.Types

import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.RootHeader (HashIncludeArg)
import HsBindgen.Imports
import HsBindgen.Language.C (PrimType)

import Doxygen.Parser.Types qualified as Doxy

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

-- | Reference to enclosing declaration
data EnclosingRef p =
    EnclosingRef (Id p)
  | UnusableEnclosingRef DeclId

deriving stock instance (Show (Id p)) => Show (EnclosingRef p)
deriving stock instance (Eq   (Id p)) => Eq   (EnclosingRef p)
deriving stock instance (Ord  (Id p)) => Ord  (EnclosingRef p)

data DeclInfo p = DeclInfo{
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
      mainHeaders :: NonEmpty HashIncludeArg

      -- | @#include@ argument used to include the file where the declaration is
      -- actually declared
    , includeArg :: HashIncludeArg

      -- | Raw macro used as a @#include@ argument, when applicable
    , includeMacroArg :: Maybe Text
    }
  deriving stock (Show, Eq, Generic)

data FieldInfo p = FieldInfo {
      loc     :: SingleLoc
    , name    :: ScopedName p
    , comment :: CommentDecl p
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
  | DeclGlobal (Global p)

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
  deriving stock (Generic)

deriving stock instance (IsPass p, Show (CommentDecl p)) => Show (AnonEnumConstant p)
deriving stock instance (IsPass p, Eq   (CommentDecl p)) => Eq   (AnonEnumConstant p)

data Function p = Function {
      args  :: [FunctionArg p]
    , res   :: C.Type p
    , attrs :: FunctionAttributes
    , ann   :: Ann "Function" p
    }
  deriving stock (Generic)

data FunctionArg p = FunctionArg {
      name   :: Maybe (ScopedName p)
    , argTyp :: C.TypeFunArg p
    }
    deriving stock (Generic)

data Global p = Global {
      typ :: C.Type p
    , ann :: Ann "Global" p
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
      doxygen :: Doxy.Comment (CommentRef p)
    }
  deriving stock (Generic)

-- | Cross-reference in a Doxygen comment
--
-- The @RefKind@ from the Doxygen XML @kindref@ attribute narrows the search
-- in 'MangleNames': compounds (struct\/union) are looked up in the type
-- constructor namespace, members (function\/typedef\/macro) in the variable
-- and type constructor namespaces.
data CommentRef p = CommentRef Text (Maybe (Id p)) (Maybe Doxy.RefKind)

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

deriving stock instance IsPass p => Show (Comment          p)
deriving stock instance IsPass p => Show (CommentRef       p)
deriving stock instance (IsPass p, Show (CommentDecl p)) => Show (Decl             p)
deriving stock instance (IsPass p, Show (CommentDecl p)) => Show (DeclInfo         p)
deriving stock instance (IsPass p, Show (CommentDecl p)) => Show (FieldInfo        p)
deriving stock instance (IsPass p, Show (CommentDecl p)) => Show (DeclKind         p)
deriving stock instance (IsPass p, Show (CommentDecl p)) => Show (Enum             p)
deriving stock instance (IsPass p, Show (CommentDecl p)) => Show (EnumConstant     p)
deriving stock instance IsPass p => Show (Function         p)
deriving stock instance IsPass p => Show (FunctionArg      p)
deriving stock instance IsPass p => Show (Global           p)
deriving stock instance (IsPass p, Show (CommentDecl p)) => Show (Struct           p)
deriving stock instance (IsPass p, Show (CommentDecl p)) => Show (StructField      p)
deriving stock instance (IsPass p, Show (CommentDecl p)) => Show (TranslationUnit  p)
deriving stock instance IsPass p => Show (Typedef          p)
deriving stock instance (IsPass p, Show (CommentDecl p)) => Show (Union            p)
deriving stock instance (IsPass p, Show (CommentDecl p)) => Show (UnionField       p)

deriving stock instance IsPass p => Eq (Comment          p)
deriving stock instance IsPass p => Eq (CommentRef       p)
deriving stock instance (IsPass p, Eq (CommentDecl p)) => Eq (DeclKind         p)
deriving stock instance (IsPass p, Eq (CommentDecl p)) => Eq (Enum             p)
deriving stock instance (IsPass p, Eq (CommentDecl p)) => Eq (EnumConstant     p)
deriving stock instance (IsPass p, Eq (CommentDecl p)) => Eq (FieldInfo        p)
deriving stock instance IsPass p => Eq (Function         p)
deriving stock instance IsPass p => Eq (FunctionArg      p)
deriving stock instance IsPass p => Eq (Global           p)
deriving stock instance (IsPass p, Eq (CommentDecl p)) => Eq (Struct           p)
deriving stock instance (IsPass p, Eq (CommentDecl p)) => Eq (StructField      p)
deriving stock instance IsPass p => Eq (Typedef          p)
deriving stock instance (IsPass p, Eq (CommentDecl p)) => Eq (Union            p)
deriving stock instance (IsPass p, Eq (CommentDecl p)) => Eq (UnionField       p)
