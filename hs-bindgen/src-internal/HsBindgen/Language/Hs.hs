-- | General Haskell language types
module HsBindgen.Language.Hs (
    -- * References
    HsModuleName(..)
  , HsIdentifier(..)
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
