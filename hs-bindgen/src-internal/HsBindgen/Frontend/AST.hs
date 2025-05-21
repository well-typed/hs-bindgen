-- | Raw AST
module HsBindgen.Frontend.AST (
    TranslationUnit(..)
    -- * Declarations
  , Decl(..)
  , DeclInfo(..)
  , DeclKind(..)
  , Field(..)
  , Typedef(..)
    -- * Types (at use sites)
  , Type(..)
    -- ** Primitive types
  , PrimType(..)
  , PrimIntType(..)
  , PrimFloatType(..)
  , PrimSignChar(..)
  , PrimSign(..)
    -- * Utility: translate between passes
  , LiftPass(..)
    -- * Show
  , ShowPass
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
  | DeclMacro (Macro p)

data Field p = Field{
      fieldName   :: Text
    , fieldType   :: Type p
    , fieldOffset :: Int     -- ^ Offset in bits
    , fieldAnn    :: Ann "Field" p
    }

data Typedef p = Typedef {
      typedefType :: Type p
    , typedefAnn  :: Ann "Typedef" p
    }

{-------------------------------------------------------------------------------
  Types (at use sites)
-------------------------------------------------------------------------------}

data Type p =
    TypePrim PrimType
  | TypeStruct (Id p)
  | TypeTypedef (Id p)
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
  Utility: translate between passes
-------------------------------------------------------------------------------}

class LiftPass a where
  liftIds :: forall p p'. Id p ~ Id p' => a p -> a p'

instance LiftPass Type where
  liftIds (TypePrim    x) = TypePrim    x
  liftIds (TypeStruct  x) = TypeStruct  x
  liftIds (TypeTypedef x) = TypeTypedef x
  liftIds (TypePointer x) = TypePointer (liftIds x)

instance LiftPass DeclInfo where
  liftIds DeclInfo{declLoc, declId} = DeclInfo{declLoc, declId}

{-------------------------------------------------------------------------------
  Show instances
-------------------------------------------------------------------------------}

class ( IsPass p
        -- Show constraints
      , Show (Id    p)
      , Show (Macro p)
      , Show (Ann "TranslationUnit" p)
      , Show (Ann "Decl"            p)
      , Show (Ann "Field"           p)
      , Show (Ann "Typedef"         p)
      ) => ShowPass p where

deriving stock instance ShowPass p => Show (TranslationUnit p)
deriving stock instance ShowPass p => Show (Decl            p)
deriving stock instance ShowPass p => Show (DeclInfo        p)
deriving stock instance ShowPass p => Show (DeclKind        p)
deriving stock instance ShowPass p => Show (Field           p)
deriving stock instance ShowPass p => Show (Typedef         p)
deriving stock instance ShowPass p => Show (Type            p)

