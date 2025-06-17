module HsBindgen.Language.C.Name (
    -- * Types
    CName(..)
  , Namespace(..)
  , NameKind(..)
  , NameOrigin(..)
  ) where

import Data.Text (unpack)

import HsBindgen.Imports
import HsBindgen.Util.Tracer (PrettyTrace (prettyTrace))

{-------------------------------------------------------------------------------
  Names and namespaces
-------------------------------------------------------------------------------}

-- TODO Rename CName to Name, for qualified import

-- | C name
--
-- This type represents a C name, with no prefix.
--
-- Example: @foo@
newtype CName = CName {
      getCName :: Text
    }
  deriving newtype (Show, Eq, Ord, IsString, Semigroup)
  deriving stock (Generic)

instance PrettyTrace CName where
  prettyTrace (CName name) = unpack name

-- | C namespace
--
-- C has the following namespaces:
--
-- * Label namespace, which we are not concerned with
-- * Ordinary namespace, including variable, function, and @typedef@ names
-- * Tag namespace, which is a /single/ namespace for @struct@, @union@, and
--   @enum@ tags
-- * Field namespaces, separate per @struct@ and @union@
--
-- This type is used to describe C types, so only the ordinary and tag
-- namespaces are represented.
data Namespace =
    -- | Ordinary namespace
    --
    -- A name in the ordinary namespace is written without a prefix.
    NamespaceOrdinary

    -- | Tag namespace
    --
    -- A name in the tag namespace is written with a prefix, which must be
    -- determined from context.
  | NamespaceTag
  deriving stock (Show, Eq, Ord, Generic)

instance PrettyTrace Namespace where
  prettyTrace = show

-- | C name kind
--
-- This type is the same as 'Namespace` except that it distinguishes tag kinds.
-- It is needed when the kind is not determined by a context.
data NameKind =
    -- | Ordinary kind
    --
    -- An ordinary name is written without a prefix.
    NameKindOrdinary

    -- | Struct kind
    --
    -- A struct name is written with a @struct@ prefix.
  | NameKindStruct

    -- | Union kind
    --
    -- A union name is written with a @union@ prefix.
  | NameKindUnion

    -- | Enum kind
    --
    -- An enum name is written with an @enum@ prefix.
  | NameKindEnum
  deriving stock (Show, Eq, Ord, Generic)

instance PrettyTrace NameKind where
  prettyTrace = show

-- | C name origin
--
-- This type describes the origin of a C name.
data NameOrigin =
    -- | Name in source
    --
    -- The name may be used to construct a valid C type.
    NameOriginInSource

    -- | Name is generated
    --
    -- The name may not be used to construct a valid C type.
  | NameOriginGenerated

    -- | Name is renamed
    --
    -- The name may not be used to construct a valid C type, but this original
    -- name may be used to construct a valid C type.
  | NameOriginRenamedFrom CName
  deriving stock (Show, Eq, Ord, Generic)

instance PrettyTrace NameOrigin where
  prettyTrace = show
