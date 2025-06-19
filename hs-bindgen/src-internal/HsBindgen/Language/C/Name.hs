module HsBindgen.Language.C.Name (
    -- * Types
    AnonId(..)
  , CName(..)
  , NameKind(..)
  , NameOrigin(..)
  ) where

import Data.Text (unpack)

import Clang.HighLevel.Types (SingleLoc)
import HsBindgen.Imports
import HsBindgen.Util.Tracer (PrettyTrace (prettyTrace))

{-------------------------------------------------------------------------------
  Names and namespaces
-------------------------------------------------------------------------------}

-- | Identity of an anonymous declaration
newtype AnonId = AnonId SingleLoc
  deriving stock (Show, Eq, Ord, Generic)

instance PrettyTrace AnonId where
  prettyTrace (AnonId loc) = "<" ++ show loc ++ ">"

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

-- | C name kind
--
-- This type distinguishes ordinary names and tagged names.  It is needed when
-- the kind is not determined by a context.
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
  | NameOriginGenerated AnonId

    -- | Name is renamed
    --
    -- The name may not be used to construct a valid C type, but this original
    -- name may be used to construct a valid C type.
  | NameOriginRenamedFrom CName
  deriving stock (Show, Eq, Ord, Generic)

instance PrettyTrace NameOrigin where
  prettyTrace = show
