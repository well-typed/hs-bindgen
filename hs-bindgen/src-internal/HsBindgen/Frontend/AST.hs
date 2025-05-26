-- | Raw AST
module HsBindgen.Frontend.AST (
    TranslationUnit(..)
    -- * Declarations
  , Decl(..)
  , DeclInfo(..)
  , DeclKind(..)
  , Field(..)
  , Typedef(..)
  , Enumerator(..)
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
    DeclStruct [Field p]
  | DeclStructOpaque
  | DeclTypedef (Typedef p)
  | DeclEnum [Enumerator p]
  | DeclEnumOpaque
  | DeclMacro (Macro p)

data Field p = Field {
      fieldName   :: Text
    , fieldType   :: Type p
    , fieldOffset :: Int     -- ^ Offset in bits
    , fieldAnn    :: Ann "Field" p
    }

data Typedef p = Typedef {
      typedefType :: Type p
    , typedefAnn  :: Ann "Typedef" p
    }

data Enumerator p = Enumerator {
      enumeratorName  :: Text
    , enumeratorValue :: Integer
    , enumeratorAnn   :: Ann "Enumerator" p
    }

{-------------------------------------------------------------------------------
  Types (at use sites)
-------------------------------------------------------------------------------}

data Type p =
    TypePrim PrimType
  | TypeStruct (Id p)
  | TypeEnum (Id p)
  | TypeTypedef (Id p) (Ann "TypeTypedef" p)
  | TypePointer (Type p)

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
  | NamespaceEnum
  | NamespaceMacro
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
      DeclEnum{}         -> NamespaceEnum
      DeclEnumOpaque{}   -> NamespaceEnum
      DeclTypedef{}      -> NamespaceTypedef
      DeclMacro{}        -> NamespaceMacro

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
      , Show (Ann "Field"           p)
      , Show (Ann "TranslationUnit" p)
      , Show (Ann "Typedef"         p)
      , Show (Ann "TypeTypedef"     p)
      , Show (Ann "Enumerator"      p)
      ) => ValidPass p where

deriving stock instance ValidPass p => Show (Decl            p)
deriving stock instance ValidPass p => Show (DeclInfo        p)
deriving stock instance ValidPass p => Show (DeclKind        p)
deriving stock instance ValidPass p => Show (Enumerator      p)
deriving stock instance ValidPass p => Show (Field           p)
deriving stock instance ValidPass p => Show (QualId          p)
deriving stock instance ValidPass p => Show (TranslationUnit p)
deriving stock instance ValidPass p => Show (Type            p)
deriving stock instance ValidPass p => Show (Typedef         p)
