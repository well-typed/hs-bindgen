-- | General Haskell language types
--
-- Intended for qualified import.
--
-- > import HsBindgen.Language.Haskell qualified as Hs
module HsBindgen.Language.Haskell (
    -- * Module names
    ModuleName -- opaque
  , moduleNameFromText
  , moduleNameToText
  , moduleNameFromString
  , moduleNameToString
    -- * References
  , Identifier(..)
  , ExtRef(..)
  , Ref(..)
    -- * Namespaced names
  , Namespace(..)
  , SNamespace(..)
  , namespaceOf
  , SingNamespace(..)
  , Name(..)
    -- * Instances
  , TypeClass(..)
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Ord qualified as Ord
import Data.Text qualified as Text
import Text.Read (readMaybe)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Module names
-------------------------------------------------------------------------------}

-- | Haskell module name
--
-- Example: @HsBindgen.Runtime.LibC@
newtype ModuleName = ModuleName { moduleNameToText :: Text }
  deriving stock (Generic)
  -- 'Show' instance valid due to 'IsString' instance
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, Eq, IsString, Ord, Show)

moduleNameFromText :: Text -> ModuleName
moduleNameFromText = ModuleName

moduleNameFromString :: String -> ModuleName
moduleNameFromString = moduleNameFromText . Text.pack

moduleNameToString :: ModuleName -> String
moduleNameToString = Text.unpack . moduleNameToText

instance PrettyForTrace ModuleName where
  prettyForTrace = PP.textToCtxDoc . moduleNameToText

{-------------------------------------------------------------------------------
  References
-------------------------------------------------------------------------------}

-- | Haskell identifier
--
-- Example: @Tm@
--
-- This type is used to reference Haskell types, constructors, fields, etc.  It
-- does /not/ specify a 'Namespace' like the 'Name' type below.
newtype Identifier = Identifier { getIdentifier :: Text }
  deriving stock (Generic)
  -- 'Show' instance valid due to 'IsString' instance
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, Eq, IsString, Ord, Show)

-- | External reference
--
-- An external reference specifies the 'ModuleName' and 'Identifier'
data ExtRef = ExtRef {
      extRefModule     :: ModuleName
    , extRefIdentifier :: Identifier
    }
  deriving stock (Eq, Generic, Ord, Show)

-- | Reference
data Ref =
    -- | Reference to an identifier in the local scope
    RefLocal Identifier

  | -- | Reference to an identifier in a different module
    RefExt ExtRef
  deriving stock (Eq, Show)

{-------------------------------------------------------------------------------
  Namespaced names
-------------------------------------------------------------------------------}

-- | Namespace
--
-- See section 1.4, "Namespaces" of the Haskell report
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch1.html#x6-130001.4>
data Namespace =
    NsTypeConstr
  | NsConstr
  | NsVar
  deriving (Eq, Ord, Show)

-- | Namespace singleton
data SNamespace :: Namespace -> Star where
  SNsTypeConstr :: SNamespace 'NsTypeConstr
  SNsConstr     :: SNamespace 'NsConstr
  SNsVar        :: SNamespace 'NsVar

-- | Get the namespace of a namespace singleton
namespaceOf :: SNamespace ns -> Namespace
namespaceOf = \case
    SNsTypeConstr -> NsTypeConstr
    SNsConstr     -> NsConstr
    SNsVar        -> NsVar

-- | Namespace singleton class
class SingNamespace ns where
  singNamespace :: SNamespace ns

instance SingNamespace 'NsTypeConstr where singNamespace = SNsTypeConstr
instance SingNamespace 'NsConstr     where singNamespace = SNsConstr
instance SingNamespace 'NsVar        where singNamespace = SNsVar

-- | Haskell name in namespace @ns@
newtype Name (ns :: Namespace) = Name { getName :: Text }
  -- 'Show' instance valid due to 'IsString' instance
  deriving newtype (Eq, IsString, Ord, Semigroup, Show)

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

-- | Type class
data TypeClass =
    -- Haskell98 derivable classes
    -- <https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/deriving.html>
    Eq
  | Ord
  | Enum
  | Ix
  | Bounded
  | Read
  | Show

    -- Classes we can only derive through newtype deriving
  | Bits
  | FiniteBits
  | Floating
  | Fractional
  | Integral
  | Num
  | Real
  | RealFloat
  | RealFrac

    -- Classes we can generate when all components have instances
  | StaticSize
  | ReadRaw
  | WriteRaw
  | Storable
  deriving stock (Eq, Generic, Read, Show)

instance Aeson.FromJSON TypeClass where
  parseJSON = Aeson.withText "TypeClass" $ \t ->
    let s = Text.unpack t
    in  case readMaybe s of
          Just clss -> return clss
          Nothing   -> Aeson.parseFail $ "unknown type class: " ++ s

instance Aeson.ToJSON TypeClass where
  toJSON = Aeson.String . Text.pack . show

-- Order lexicographically, not by order of definition
instance Ord TypeClass where
  compare = Ord.comparing show
