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
  , DeclInfo(..)
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
    -- ** Macros
  , CheckedMacro(..)
  , CheckedMacroType(..)
  , CheckedMacroExpr(..)
    -- * Types (at use sites)
  , Type(..)
    -- * Naming
  , AnonId(..)
  , NameOrigin(..)
    -- * Show
  , ValidPass
    -- * Helper functions
  , declNsPrelimDeclId
  , declQualPrelimDeclId
  , declQualName
  , declKindNameKind
  ) where

import Prelude hiding (Enum)

import Clang.HighLevel.Documentation
import Clang.HighLevel.Types
import Clang.Paths
import HsBindgen.C.Tc.Macro.Type qualified as Macro
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Macros.AST.Syntax qualified as Macro
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
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
      -- * The 'ResolveBindingSpec' pass removes declarations for which we have
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

data Decl p = Decl {
      declInfo :: DeclInfo p
    , declKind :: DeclKind p
    , declAnn  :: Ann "Decl" p
    }

data DeclInfo p = DeclInfo{
      declLoc     :: SingleLoc
    , declId      :: Id p
    , declAliases :: [C.Name]

      -- | User-specified header that provides this declaration
      --
      -- Note that the declaration may not be in this header directly, but in
      -- one of its (transitive) includes.
      --
      -- If the user specifies /multiple/ headers, all of which directly or
      -- indirectly contain the same declaration, then either
      --
      -- 1. the C headers will need to explicitly check
      --    (if this is already defined, do nothing), or
      -- 2. This is an error in the C code.
      --
      -- This means that there is always a /single/ header to choose here.
    , declHeader :: CHeaderIncludePath

    , declComment :: Maybe Comment
    }

data DeclKind p =
    DeclStruct (Struct p)
  | DeclStructOpaque
  | DeclUnion (Union p)
  | DeclUnionOpaque
  | DeclTypedef (Typedef p)
  | DeclEnum (Enum p)
  | DeclEnumOpaque
  | DeclMacro (MacroBody p)
  | DeclFunction (Function p)
    -- | A global variables, whether it be declared @extern@, @static@ or neither.
  | DeclGlobal (Type p)
  | DeclConst (Type p)

data Struct p = Struct {
      structSizeof    :: Int
    , structAlignment :: Int
    , structFields    :: [StructField p]
    , structAnn       :: Ann "Struct" p
    , structComment   :: Maybe Comment
    }

data StructField p = StructField {
      structFieldLoc     :: SingleLoc
    , structFieldName    :: FieldName p
    , structFieldType    :: Type p
    , structFieldOffset  :: Int     -- ^ Offset in bits
    , structFieldWidth   :: Maybe Int
    , structFieldAnn     :: Ann "StructField" p
    , structFieldComment :: Maybe Comment
    }

data Union p = Union {
      unionSizeof    :: Int
    , unionAlignment :: Int
    , unionFields    :: [UnionField p]
    , unionAnn       :: Ann "Union" p
    , unionComment   :: Maybe Comment
    }

data UnionField p = UnionField {
      unionFieldLoc     :: SingleLoc
    , unionFieldName    :: FieldName p
    , unionFieldType    :: Type p
    , unionFieldAnn     :: Ann "UnionField" p
    , unionFieldComment :: Maybe Comment
    }

data Typedef p = Typedef {
      typedefType    :: Type p
    , typedefAnn     :: Ann "Typedef" p
    , typedefComment :: Maybe Comment
    }

data Enum p = Enum {
      enumType      :: Type p
    , enumSizeof    :: Int
    , enumAlignment :: Int
    , enumConstants :: [EnumConstant p]
    , enumAnn       :: Ann "Enum" p
    , enumComment   :: Maybe Comment
    }

data EnumConstant p = EnumConstant {
      enumConstantLoc     :: SingleLoc
    , enumConstantName    :: FieldName p
    , enumConstantValue   :: Integer
    , enumConstantComment :: Maybe Comment
    }

data Function p = Function {
      functionArgs    :: [Type p]
    , functionRes     :: Type p
    , functionAttrs   :: FunctionAttributes
    , functionAnn     :: Ann "Function" p
    , functionComment :: Maybe Comment
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
  deriving stock (Show, Eq, Generic)

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
  deriving stock (Show, Eq, Generic)

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
  Macros
-------------------------------------------------------------------------------}

data CheckedMacro p =
    MacroType (CheckedMacroType p)
  | MacroExpr CheckedMacroExpr

data CheckedMacroType p = CheckedMacroType{
      macroType        :: Type p
    , macroTypeAnn     :: Ann "CheckedMacroType" p
    , macroTypeComment :: Maybe Comment
    }

-- | Checked expression (function) macro
--
-- TODO: This is wrong, it does not allow name mangling to do its job. To fix
-- that we'd have to change 'Macro.MExpr'.
data CheckedMacroExpr = CheckedMacroExpr{
      macroExprArgs :: [C.Name]
    , macroExprBody :: Macro.MExpr Macro.Ps
    , macroExprType :: Macro.Quant (Macro.Type Macro.Ty)
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Types (at use sites)
-------------------------------------------------------------------------------}

data Type p =
    TypePrim C.PrimType
  | TypeStruct (Id p)
  | TypeUnion (Id p)
  | TypeEnum (Id p)
  | TypeTypedef (TypedefRef p)

    -- | Macro-defined type
    --
    -- These behave very similar to 'TypeTypedef'.
  | TypeMacroTypedef (Id p)

  | TypePointer (Type p)
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

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

class    ( Show (Ann ix p)
         , Eq   (Ann ix p)
         ) => ValidAnn (ix :: Symbol) (p :: Pass)
instance ( Show (Ann ix p)
         , Eq   (Ann ix p)
         ) => ValidAnn (ix :: Symbol) (p :: Pass)

-- | Valid pass
--
-- A pass is valid if the various type family instances satisfy constraints that
-- we need, primarily for debugging and testing.
--
-- This is intentionally /not/ defined as a class alias, so that we get an error
-- the moment we declare a pass to be 'ValidPass', rather than at use sites.
class ( IsPass p

        -- Identifiers
        --
        -- We often store identifiers in maps etc., so we insist on 'Ord'

      , Show (Id p)
      , Show (FieldName p)

      , Ord (Id p)
      , Ord (FieldName p)

        -- Other constructs

      , Show (ExtBinding p)
      , Show (MacroBody  p)
      , Show (TypedefRef p)

      , Eq (ExtBinding p)
      , Eq (MacroBody  p)
      , Eq (TypedefRef p)

        -- Annotations

      , ValidAnn "CheckedMacroType" p
      , ValidAnn "Decl"             p
      , ValidAnn "Enum"             p
      , ValidAnn "Function"         p
      , ValidAnn "Struct"           p
      , ValidAnn "StructField"      p
      , ValidAnn "TranslationUnit"  p
      , ValidAnn "Typedef"          p
      , ValidAnn "Union"            p
      , ValidAnn "UnionField"       p
      ) => ValidPass p where

deriving stock instance ValidPass p => Show (CheckedMacro     p)
deriving stock instance ValidPass p => Show (CheckedMacroType p)
deriving stock instance ValidPass p => Show (Decl             p)
deriving stock instance ValidPass p => Show (DeclInfo         p)
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

deriving stock instance ValidPass p => Eq (CheckedMacro     p)
deriving stock instance ValidPass p => Eq (CheckedMacroType p)
deriving stock instance ValidPass p => Eq (Decl             p)
deriving stock instance ValidPass p => Eq (DeclInfo         p)
deriving stock instance ValidPass p => Eq (DeclKind         p)
deriving stock instance ValidPass p => Eq (Enum             p)
deriving stock instance ValidPass p => Eq (EnumConstant     p)
deriving stock instance ValidPass p => Eq (Function         p)
deriving stock instance ValidPass p => Eq (Struct           p)
deriving stock instance ValidPass p => Eq (StructField      p)
deriving stock instance ValidPass p => Eq (TranslationUnit  p)
deriving stock instance ValidPass p => Eq (Type             p)
deriving stock instance ValidPass p => Eq (Typedef          p)
deriving stock instance ValidPass p => Eq (Union            p)
deriving stock instance ValidPass p => Eq (UnionField       p)

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance PrettyForTrace (Located (Id p)) => PrettyForTrace (DeclInfo p) where
  prettyForTrace DeclInfo{declId, declLoc} =
    prettyForTrace $ Located declLoc declId

{-------------------------------------------------------------------------------
  Helper functions
-------------------------------------------------------------------------------}

declNsPrelimDeclId :: Id p ~ PrelimDeclId => Decl p -> NsPrelimDeclId
declNsPrelimDeclId Decl{declInfo = DeclInfo{declId}, declKind} =
    nsPrelimDeclId declId $ C.nameKindTypeNamespace (declKindNameKind declKind)

declQualPrelimDeclId :: Id p ~ PrelimDeclId => Decl p -> QualPrelimDeclId
declQualPrelimDeclId Decl{declInfo = DeclInfo{declId}, declKind} =
    qualPrelimDeclId declId (declKindNameKind declKind)

declQualName :: Id p ~ DeclId => Decl p -> C.QualName
declQualName Decl{declInfo = DeclInfo{declId}, declKind} =
    C.QualName (declIdName declId) (declKindNameKind declKind)

declKindNameKind :: DeclKind p -> C.NameKind
declKindNameKind = \case
    DeclStruct{}       -> C.NameKindStruct
    DeclStructOpaque{} -> C.NameKindStruct
    DeclUnion{}        -> C.NameKindUnion
    DeclUnionOpaque{}  -> C.NameKindUnion
    DeclEnum{}         -> C.NameKindEnum
    DeclEnumOpaque{}   -> C.NameKindEnum
    _otherwise         -> C.NameKindOrdinary
