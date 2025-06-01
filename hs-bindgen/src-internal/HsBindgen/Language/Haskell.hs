-- | General Haskell language types
module HsBindgen.Language.Haskell (
    -- * References
    HsRef(..)
  , ExtHsRef(..)
  , HsModuleName(..)
  , HsIdentifier(..)
    -- * Namespaced names
  , Namespace(..)
  , SNamespace(..)
  , namespaceOf
  , SingNamespace(..)
  , HsName(..)
    -- * Instances
  , HsTypeClass(..)
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Text qualified as Text
import Text.Read (readMaybe)

import HsBindgen.Imports

{-------------------------------------------------------------------------------
  References
-------------------------------------------------------------------------------}

-- | Haskell reference
data HsRef =
    -- | Reference to an identifier in the local scope
    HsRefLocal HsIdentifier

  | -- | Reference to an identifier in a different module
    HsRefExt ExtHsRef
  deriving stock (Eq, Show)

-- | External Haskell reference
data ExtHsRef = ExtHsRef {
      extHsRefModule     :: HsModuleName
    , extHsRefIdentifier :: HsIdentifier
    }
  deriving stock (Eq, Show)

-- | Haskell module name
--
-- Example: @HsBindgen.Runtime.LibC@
newtype HsModuleName = HsModuleName { getHsModuleName :: Text }
  deriving stock (Generic)
  -- 'Show' instance valid due to 'IsString' instance
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, Eq, IsString, Ord, Show)

-- | Haskell identifier
--
-- Example: @CTm@
--
-- This type is different from 'HsBindgen.Hs.AST.HsName' in that it does not
-- specify a 'HsBindgen.Hs.AST.Namespace'.
newtype HsIdentifier = HsIdentifier { getHsIdentifier :: Text }
  deriving stock (Generic)
  -- 'Show' instance valid due to 'IsString' instance
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, Eq, IsString, Ord, Show)

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
newtype HsName (ns :: Namespace) = HsName { getHsName :: Text }
  -- 'Show' instance valid due to 'IsString' instance
  deriving newtype (Show, Eq, Ord, IsString, Semigroup)

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

-- | Type class
data HsTypeClass =
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
  deriving stock (Eq, Generic, Ord, Read, Show)

instance Aeson.FromJSON HsTypeClass where
  parseJSON = Aeson.withText "HsTypeClass" $ \t ->
    let s = Text.unpack t
    in  case readMaybe s of
          Just clss -> return clss
          Nothing   -> Aeson.parseFail $ "unknown type class: " ++ s

instance Aeson.ToJSON HsTypeClass where
  toJSON = Aeson.String . Text.pack . show
