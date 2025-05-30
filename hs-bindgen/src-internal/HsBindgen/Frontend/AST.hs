-- | Raw AST
module HsBindgen.Frontend.AST (
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
  , Enu(..)
  , EnumConstant(..)
  , Function(..)
    -- * Types (at use sites)
  , Type(..)
    -- ** Primitive types
  , PrimType(..)
  , PrimIntType(..)
  , PrimFloatType(..)
  , PrimSignChar(..)
  , PrimSign(..)
    -- * Namespaces
  , QualId(..)
  , Namespace(..)
  , coerceQualId
  , declQualId
    -- * Show
  , ValidPass
  ) where

import Clang.HighLevel.Types
import HsBindgen.Frontend.Graph.Includes (IncludeGraph)
import HsBindgen.Frontend.Pass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

data TranslationUnit p = TranslationUnit{
      -- | Declarations in the unit
      --
      -- This includes all declarations from all headers that we have processed,
      -- except
      --
      -- * declarations that were filtered out by a selection predicate
      -- * declarations for which we have existing external bindings
      -- * declarations that were filtered out by a binding specification
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
      declLoc :: SingleLoc
    , declId  :: Id p
    }

data DeclKind p =
    DeclStruct (Struct p)
  | DeclStructOpaque
  | DeclUnion (Union p)
  | DeclUnionOpaque
  | DeclTypedef (Typedef p)
  | DeclEnum Enu
  | DeclEnumOpaque
  | DeclMacro (Macro p)
  | DeclFunction (Function p)

data Struct p = Struct {
      structSizeof    :: Int
    , structAlignment :: Int
    , structFields    :: [StructField p]
    }

data StructField p = StructField {
      structFieldName   :: Text
    , structFieldType   :: Type p
    , structFieldOffset :: Int     -- ^ Offset in bits
    , structFieldAnn    :: Ann "StructField" p
    }

data Union p = Union {
      unionSizeof    :: Int
    , unionAlignment :: Int
    , unionFields    :: [UnionField p]
    }

data UnionField p = UnionField {
      unionFieldName   :: Text
    , unionFieldType   :: Type p
    , unionFieldAnn    :: Ann "UnionField" p
    }

data Typedef p = Typedef {
      typedefType :: Type p
    , typedefAnn  :: Ann "Typedef" p
    }

data Enu = Enu {
      enumSizeof    :: Int
    , enumAlignment :: Int
    , enumConstants :: [EnumConstant]
    }
  deriving stock (Show)

data EnumConstant = EnumConstant {
      enumConstantName  :: Text
    , enumConstantValue :: Integer
    }
  deriving stock (Show)

data Function p = Function {
      functionName :: Text
    , functionArgs :: [Type p]
    , functionRes  :: Type p
    , functionAnn  :: Ann "Function" p
    }

{-------------------------------------------------------------------------------
  Types (at use sites)
-------------------------------------------------------------------------------}

data Type p =
    TypePrim PrimType
  | TypeStruct (Id p)
  | TypeUnion (Id p)
  | TypeEnum (Id p)
  | TypeTypedef (Id p) (Ann "TypeTypedef" p)
  | TypePointer (Type p)
  | TypeFunction [Type p] (Type p)
  | TypeVoid

data PrimType =
    PrimChar PrimSignChar
  | PrimIntegral PrimIntType PrimSign
  | PrimFloating PrimFloatType
  deriving stock (Show)

data PrimIntType =
    PrimShort
  | PrimInt
  | PrimLong
  | PrimLongLong
  deriving stock (Show)

data PrimFloatType =
    PrimFloat
  | PrimDouble
  | PrimLongDouble
  deriving stock (Show)

data PrimSignChar =
    PrimSignExplicit PrimSign
  | PrimSignImplicit (Maybe PrimSign)
  deriving stock (Show)

data PrimSign = Signed | Unsigned
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Namespaces
-------------------------------------------------------------------------------}

data Namespace =
    NamespaceTypedef
  | NamespaceStruct
  | NamespaceUnion
  | NamespaceEnum
  | NamespaceMacro
  | NamespaceFunction
  deriving stock (Show, Eq, Ord)

data QualId p = QualId (Id p) Namespace

deriving instance Eq  (Id p) => Eq  (QualId p)
deriving instance Ord (Id p) => Ord (QualId p)

-- TODO: It would be nicer if we could avoid this
coerceQualId :: (Id p ~ Id p') => QualId p -> QualId p'
coerceQualId (QualId uid ns) = QualId uid ns

declQualId :: Decl p -> QualId p
declQualId Decl{declInfo = DeclInfo{declId}, declKind} = QualId (declId) $
    case declKind of
      DeclStruct{}       -> NamespaceStruct
      DeclStructOpaque{} -> NamespaceStruct
      DeclUnion{}        -> NamespaceUnion
      DeclUnionOpaque{}  -> NamespaceUnion
      DeclEnum{}         -> NamespaceEnum
      DeclEnumOpaque{}   -> NamespaceEnum
      DeclTypedef{}      -> NamespaceTypedef
      DeclMacro{}        -> NamespaceMacro
      DeclFunction{}     -> NamespaceFunction

{-------------------------------------------------------------------------------
  Show instances
-------------------------------------------------------------------------------}

class ( IsPass p

        -- We often store identifiers in maps etc.
      , Ord (Id p)

        -- Show constraints
      , Show (Id    p)
      , Show (Macro p)

        -- Annotations
      , Show (Ann "Decl"            p)
      , Show (Ann "StructField"     p)
      , Show (Ann "UnionField"      p)
      , Show (Ann "TranslationUnit" p)
      , Show (Ann "Typedef"         p)
      , Show (Ann "TypeTypedef"     p)
      , Show (Ann "Function"        p)
      ) => ValidPass p where

deriving stock instance ValidPass p => Show (Decl            p)
deriving stock instance ValidPass p => Show (DeclInfo        p)
deriving stock instance ValidPass p => Show (DeclKind        p)
deriving stock instance ValidPass p => Show (Struct          p)
deriving stock instance ValidPass p => Show (StructField     p)
deriving stock instance ValidPass p => Show (Union           p)
deriving stock instance ValidPass p => Show (UnionField      p)
deriving stock instance ValidPass p => Show (Function        p)
deriving stock instance ValidPass p => Show (QualId          p)
deriving stock instance ValidPass p => Show (TranslationUnit p)
deriving stock instance ValidPass p => Show (Type            p)
deriving stock instance ValidPass p => Show (Typedef         p)
