{-# LANGUAGE PatternSynonyms #-}

-- | Internal AST as it is constructed step by step in the frontend
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.AST.Internal (ValidPass)
-- > import HsBindgen.Frontend.AST.Internal qualified as C
module HsBindgen.Frontend.AST.Internal (
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
  , Function(..)
  , FunctionAttributes(..)
  , FunctionPurity(..)
  , decideFunctionPurity
    -- ** Comments
  , Comment(..)
  , CommentRef(..)
    -- ** Macros
  , CheckedMacro(..)
  , CheckedMacroType(..)
  , CheckedMacroExpr(..)
    -- * Types (at use sites)
  , Type(TypePrim, TypeRef, TypeTypedef, TypeFun, TypeVoid,
         TypeConstArray, TypeExtBinding, TypeIncompleteArray, TypeBlock,
         TypeConst, TypeComplex)
    -- ** Pattern synonyms for safe pointer handling
  , pattern TypePointers
    -- * Haskell names
  , NamePair(..)
  , nameHs
  , RecordNames(..)
  , NewtypeNames(..)
    -- * Show
  , ValidPass
    -- * Helper functions
  , declName
  ) where

import Prelude hiding (Enum)
import Prelude qualified as P

import C.Expr.Syntax qualified as CExpr.DSL
import C.Expr.Typecheck.Type qualified as CExpr.DSL

import Clang.HighLevel.Documentation qualified as CDoc
import Clang.HighLevel.Types

import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.RootHeader (HashIncludeArg)
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer

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
      unitDecls :: [Decl p]

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
    , unitIncludeGraph :: IncludeGraph

      -- | Pass-specific annotation
    , unitAnn :: Ann "TranslationUnit" p
    }
    deriving (Generic)

data Decl p = Decl {
      declInfo :: DeclInfo p
    , declKind :: DeclKind p
    , declAnn  :: Ann "Decl" p
    }

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
      declLoc     :: SingleLoc
    , declId      :: Id p

      -- | Source header information
      --
      -- This is 'Nothing' when the declaration is a builtin, as builtin
      -- declarations have no source location.
    , declHeaderInfo :: Maybe HeaderInfo
    , declAvailability :: Availability

    , declComment :: Maybe (Comment p)
    }

data HeaderInfo = HeaderInfo{
      -- | User-specified headers that provide the declaration
      --
      -- Note that the declaration may not be in this header directly, but in
      -- one of its (transitive) includes.
      headerMainHeaders :: NonEmpty HashIncludeArg

      -- | @#include@ argument used to include the file where the declaration is
      -- actually declared
    , headerInclude :: HashIncludeArg
    }
  deriving stock (Show, Eq, Generic)

data FieldInfo p = FieldInfo {
      fieldLoc     :: SingleLoc
    , fieldName    :: FieldName p
    , fieldComment :: Maybe (Comment p)
    }

data DeclKind p =
    DeclStruct (Struct p)
  | DeclUnion (Union p)
  | DeclTypedef (Typedef p)
  | DeclEnum (Enum p)
    -- | Opaque type
    --
    -- When parsing, a C @struct@, @union@, or @enum@ may be opaque.  Users may
    -- specify any kind of type to be opaque using a prescriptive binding
    -- specification, however, including @typedef@ types.
  | DeclOpaque C.NameKind
  | DeclMacro (MacroBody p)
  | DeclFunction (Function p)
    -- | A global variable, whether it be declared @extern@, @static@ or neither.
  | DeclGlobal (Type p)

data Struct p = Struct {
      structSizeof    :: Int
    , structAlignment :: Int
    , structFields    :: [StructField p]
    , structAnn       :: Ann "Struct" p
    }

data StructField p = StructField {
      structFieldInfo    :: FieldInfo p
    , structFieldType    :: Type p
    , structFieldOffset  :: Int     -- ^ Offset in bits
    , structFieldWidth   :: Maybe Int
    , structFieldAnn     :: Ann "StructField" p
    }

data Union p = Union {
      unionSizeof    :: Int
    , unionAlignment :: Int
    , unionFields    :: [UnionField p]
    , unionAnn       :: Ann "Union" p
    }

data UnionField p = UnionField {
      unionFieldInfo    :: FieldInfo p
    , unionFieldType    :: Type p
    , unionFieldAnn     :: Ann "UnionField" p
    }

data Typedef p = Typedef {
      typedefType    :: Type p
    , typedefAnn     :: Ann "Typedef" p
    }

data Enum p = Enum {
      enumType      :: Type p
    , enumSizeof    :: Int
    , enumAlignment :: Int
    , enumConstants :: [EnumConstant p]
    , enumAnn       :: Ann "Enum" p
    }

data EnumConstant p = EnumConstant {
      enumConstantInfo  :: FieldInfo p
    , enumConstantValue :: Integer
    }

data Function p = Function {
      functionArgs    :: [(ArgumentName p, Type p)]
    , functionRes     :: Type p
    , functionAttrs   :: FunctionAttributes
    , functionAnn     :: Ann "Function" p
    }

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
      functionPurity :: FunctionPurity
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

newtype Comment p =
  Comment
    { unComment :: CDoc.Comment (CommentRef p) }

-- | Cross-reference in a Doxygen comment
--
-- Doxygen references are just strings; in particular, they do not distinguish
-- between namespaces (i.e., @struct foo@ is simply referred to as @foo@). In
-- 'MangleNames' we will /search/ for a matching name and set the @HaskellId@
-- accordingly, so that we can generate an approprate reference in the Haddocks.
data CommentRef p = CommentRef Text (Maybe (HaskellId p))

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

data CheckedMacro p =
    MacroType (CheckedMacroType p)
  | MacroExpr CheckedMacroExpr

data CheckedMacroType p = CheckedMacroType{
      macroType        :: Type p
    , macroTypeAnn     :: Ann "CheckedMacroType" p
    }

-- | Checked expression (function) macro
--
-- TODO: This is wrong, it does not allow name mangling to do its job. To fix
-- that we'd have to change 'CExpr.DSL.MExpr'.
data CheckedMacroExpr = CheckedMacroExpr{
      macroExprArgs :: [CExpr.DSL.Name]
    , macroExprBody :: CExpr.DSL.MExpr CExpr.DSL.Ps
    , macroExprType :: CExpr.DSL.Quant (CExpr.DSL.Type CExpr.DSL.Ty)
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Types (at use sites)
-------------------------------------------------------------------------------}

data Type p =
    TypePrim C.PrimType
  | TypeRef (Id p)
  | TypeTypedef (Id p) (Type p)
  | TypeUnsafePointer (Type p)
  | TypeFun [Type p] (Type p)
  | TypeVoid
  | TypeConstArray Natural (Type p)
  | TypeExtBinding (ExtBinding p)

    -- | Arrays of unknown size
    --
    -- Arrays normally have a known size, but not always:
    --
    -- * Arrays of unknown size are allowed as function arguments; such arrays
    --   are interpreted as pointers.
    -- * Arrays of unknown size may be declared for externs; this is considered
    --   an incomplete type.
    -- * Structs may contain an array of undefined size as their last field,
    --   known as a "flexible array member" (FLAM).
    --
    -- We treat the FLAM case separately.
    --
    -- See <https://en.cppreference.com/w/c/language/array#Arrays_of_unknown_size>
  | TypeIncompleteArray (Type p)

    -- | Block type
    --
    -- Blocks are a clang-specific C extension.
    --
    -- See <https://clang.llvm.org/docs/BlockLanguageSpec.html>
  | TypeBlock (Type p)

    -- | Type qualifier @const@.
  | TypeConst (Type p)

    -- | A complex floating-point type, such as @float complex@ or
    -- @double complex@. This also allows one to define, e.g. @char complex@
  | TypeComplex C.PrimType

{-------------------------------------------------------------------------------
  Pattern synonyms for safe pointer handling
-------------------------------------------------------------------------------}

-- | Bidirectional pattern synonym for N layers of pointer indirection
--
-- This pattern can be used both for matching and construction.
--
-- Examples (matching):
--   * @TypePointers 1 inner@ matches @TypeUnsafePointer inner@
--   * @TypePointers 2 inner@ matches @TypeUnsafePointer (TypeUnsafePointer inner)@
--   * @TypePointers 3 inner@ matches @TypeUnsafePointer (TypeUnsafePointer (TypeUnsafePointer inner))@
--
-- Examples (construction):
--   * @TypePointers 1 someType@ creates @TypeUnsafePointer someType@
--   * @TypePointers 2 someType@ creates @TypeUnsafePointer (TypeUnsafePointer someType)@
--
-- The inner type can be anything (TypeFun, TypeRef, TypePrim, etc.).
--
pattern TypePointers :: Int -> Type p -> Type p
pattern TypePointers n inner <- (stripPointers -> Just (n, inner))
  where
    TypePointers n inner = buildPointers n inner

-- | Helper for TypePointers pattern synonym (matching direction)
--
-- Strips all pointer layers and returns the count and inner type.
-- Returns Nothing if there are no pointer layers.
stripPointers :: Type p -> Maybe (Int, Type p)
stripPointers = go 0
  where
    go :: Int -> Type p -> Maybe (Int, Type p)
    go !n (TypeUnsafePointer inner) = go (n + 1) inner
    go !n inner
      | n > 0     = Just (n, inner)
      | otherwise = Nothing

-- | Helper for TypePointers pattern synonym (construction direction)
--
-- Builds N layers of pointers around an inner type.
buildPointers :: Int -> Type p -> Type p
buildPointers n inner
  | n <= 0    = inner
  | otherwise = TypeUnsafePointer (buildPointers (n - 1) inner)

-- | COMPLETE pragma to ensure exhaustiveness checking works
--
-- This tells GHC that pattern matching on these patterns (instead of the raw
-- TypeUnsafePointer) is complete and exhaustive.
--
{-# COMPLETE TypePrim, TypeRef, TypeTypedef, TypePointers, TypeFun, TypeVoid,
             TypeConstArray, TypeExtBinding, TypeIncompleteArray, TypeBlock,
             TypeConst, TypeComplex #-}

{-------------------------------------------------------------------------------
  Haskell names
-------------------------------------------------------------------------------}

-- | Pair of a (scoped) C name and the corresponding Haskell name
--
-- This is only used for scoped names: field names, macro arguments, etc. It is
-- /not/ used for declarations.
--
-- Invariant: the 'Hs.Identifier' must satisfy the rules for legal Haskell
-- names, for its intended use (constructor, variable, ..).
data NamePair = NamePair {
      nameC       :: C.ScopedName
    , nameHsIdent :: Hs.Identifier
    }
  deriving stock (Show, Eq, Ord, Generic)

-- | Extract namespaced Haskell name
--
-- The invariant on 'NamePair' justifies this otherwise unsafe operation.
nameHs :: NamePair -> Hs.Name ns
nameHs NamePair{nameHsIdent = Hs.Identifier name} = Hs.Name name

-- | Names for a Haskell record type
--
-- This is used in addition to a 'NamePair'.
data RecordNames = RecordNames {
      recordConstr :: Hs.Name Hs.NsConstr
    }
  deriving stock (Show, Eq, Ord, Generic)

-- | Names for a Haskell newtype
--
-- This is used in addition to a 'NamePair'.
data NewtypeNames = NewtypeNames {
      newtypeConstr :: Hs.Name Hs.NsConstr
    , newtypeField  :: Hs.Name Hs.NsVar
    }
  deriving stock (Show, Eq, Ord, Generic)

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

-- | Valid pass
--
-- A pass is valid if the various type family instances satisfy constraints that
-- we need, primarily for debugging and testing.
--
-- This is intentionally /not/ defined as a class alias, so that we get an error
-- the moment we declare a pass to be 'ValidPass', rather than at use sites.
class ( IsPass p

        -- 'Show' constraints for debugging

      , Show (ArgumentName p)
      , Show (ExtBinding   p)
      , Show (FieldName    p)
      , Show (HaskellId    p)
      , Show (Id           p)
      , Show (MacroBody    p)

      , Show (Ann "CheckedMacroType" p)
      , Show (Ann "Decl"             p)
      , Show (Ann "Enum"             p)
      , Show (Ann "Function"         p)
      , Show (Ann "Struct"           p)
      , Show (Ann "StructField"      p)
      , Show (Ann "TranslationUnit"  p)
      , Show (Ann "Typedef"          p)
      , Show (Ann "Union"            p)
      , Show (Ann "UnionField"       p)

        -- 'Ord' constraints for identifiers (which we often store in maps)

      , Ord (Id        p)
      , Ord (FieldName p)

        -- 'Eq'
        --
        -- We use equality on 'DeclKind' during construction of the 'DeclIndex'
        -- (it's OK to repeat a declaration in a header, as long as they are
        -- identical). All other 'Eq' constraints we provide are in order to
        -- support this equality.

      , Eq (ArgumentName p)
      , Eq (ExtBinding   p)
      , Eq (HaskellId    p)
      , Eq (MacroBody    p)

      , Eq (Ann "CheckedMacroType" p)
      , Eq (Ann "Enum"             p)
      , Eq (Ann "Function"         p)
      , Eq (Ann "Struct"           p)
      , Eq (Ann "StructField"      p)
      , Eq (Ann "Typedef"          p)
      , Eq (Ann "Union"            p)
      , Eq (Ann "UnionField"       p)
      ) => ValidPass p where

deriving stock instance ValidPass p => Show (CheckedMacro     p)
deriving stock instance ValidPass p => Show (CheckedMacroType p)
deriving stock instance ValidPass p => Show (Decl             p)
deriving stock instance ValidPass p => Show (DeclInfo         p)
deriving stock instance ValidPass p => Show (FieldInfo        p)
deriving stock instance ValidPass p => Show (DeclKind         p)
deriving stock instance ValidPass p => Show (Enum             p)
deriving stock instance ValidPass p => Show (EnumConstant     p)
deriving stock instance ValidPass p => Show (Function         p)
deriving stock instance ValidPass p => Show (Struct           p)
deriving stock instance ValidPass p => Show (StructField      p)
deriving stock instance ValidPass p => Show (TranslationUnit  p)
deriving stock instance ValidPass p => Show (Type             p)
deriving stock instance ValidPass p => Show (Typedef          p)
deriving stock instance ValidPass p => Show (Union            p)
deriving stock instance ValidPass p => Show (UnionField       p)
deriving stock instance ValidPass p => Show (Comment          p)
deriving stock instance ValidPass p => Show (CommentRef       p)

deriving stock instance ValidPass p => Eq (CheckedMacro     p)
deriving stock instance ValidPass p => Eq (CheckedMacroType p)
deriving stock instance ValidPass p => Eq (Comment          p)
deriving stock instance ValidPass p => Eq (DeclKind         p)
deriving stock instance ValidPass p => Eq (Enum             p)
deriving stock instance ValidPass p => Eq (EnumConstant     p)
deriving stock instance ValidPass p => Eq (FieldInfo        p)
deriving stock instance ValidPass p => Eq (Function         p)
deriving stock instance ValidPass p => Eq (CommentRef       p)
deriving stock instance ValidPass p => Eq (Struct           p)
deriving stock instance ValidPass p => Eq (StructField      p)
deriving stock instance ValidPass p => Eq (Type             p)
deriving stock instance ValidPass p => Eq (Typedef          p)
deriving stock instance ValidPass p => Eq (Union            p)
deriving stock instance ValidPass p => Eq (UnionField       p)

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance PrettyForTrace (C.Located (Id p)) => PrettyForTrace (DeclInfo p) where
  prettyForTrace DeclInfo{declId, declLoc} =
      prettyForTrace $ C.Located declLoc declId

instance Id p ~ C.DeclId p => PrettyForTrace (Decl p) where
  prettyForTrace decl = prettyForTrace decl.declInfo

{-------------------------------------------------------------------------------
  Helper functions
-------------------------------------------------------------------------------}

declName :: Id p ~ C.DeclId p => Decl p -> C.DeclName
declName decl = decl.declInfo.declId.name
