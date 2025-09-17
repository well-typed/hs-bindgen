{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Binding specification version management
--
-- This /private/ module may only be used by "HsBindgen.BindingSpec" and
-- sub-modules.
--
-- Intended for unqualified import.
module HsBindgen.BindingSpec.Private.Version (
    -- * Version
    Version
  , constVersion
  , parseVersion
  , isCompatVersions
  ) where

import Data.Aeson.Types qualified as Aeson
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Text qualified as Text
import Language.Haskell.TH.Syntax qualified as THS
import Text.Read (readMaybe)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Errors (failCode)
import HsBindgen.Imports
import HsBindgen.Util.Tracer (PrettyForTrace (prettyForTrace))

{-------------------------------------------------------------------------------
  Version
-------------------------------------------------------------------------------}

-- | Binding specification version
--
-- Binding specification versions are in @MAJOR.MINOR@ format where
-- @MAJOR >= 1@ and @MINOR >= 0@.
--
-- Versioning of binding specifications is separate from versioning of
-- @hs-bindgen@ itself.  Each release of @hs-bindgen@ is associated with a
-- specific binding specification version, which is the maximum version that it
-- can support.  It generates binding specifications of that version.
data Version = UnsafeVersion Int Int
  deriving stock (Eq, Ord, THS.Lift)

instance Aeson.FromJSON Version where
  parseJSON = Aeson.withText "Version" $ \t -> case parseVersion t of
    Right v -> return v
    Left  e -> Aeson.parseFail $ "Invalid version: " ++ e ++ ": " ++ show t

instance Aeson.ToJSON Version where
  toJSON = Aeson.String . Text.pack . show

instance PrettyForTrace Version where
  prettyForTrace = PP.string . show

instance Show Version where
  show (UnsafeVersion x y) = show x ++ '.' : show y

-- | Construct a 'Version'
mkVersion ::
     Int  -- ^ Major version (@>= 1@)
  -> Int  -- ^ Minor version (@>= 0@)
  -> Either String Version
mkVersion major minor
    | major < 1 = Left "major version less than 1"
    | minor < 0 = Left "minor version less than 0"
    | otherwise = Right (UnsafeVersion major minor)

-- | Construct a 'Version', validating it a compile-time
constVersion :: (MonadFail m, THS.Quote m) => Int -> Int -> THS.Code m Version
constVersion major minor = case mkVersion major minor of
    Right v -> [|| v ||]
    Left  e -> failCode $ "Invalid version: " ++ e

-- | Parse a 'Version'
parseVersion :: Text -> Either String Version
parseVersion t = case List.uncons <$> span Char.isDigit (Text.unpack t) of
    (majorS, Just ('.', minorS)) -> case (readMaybe majorS, readMaybe minorS) of
      (Just major, Just minor) -> mkVersion major minor
      _otherwise -> Left "not in MAJOR.MINOR format"
    _otherwise -> Left "not in MAJOR.MINOR format"

-- | Check 'Version' compatibility
--
-- Compatible binding specifications have the same representation.  Compatible
-- binding specification versions have the same major version.
isCompatVersions :: Version -> Version -> Bool
isCompatVersions (UnsafeVersion majorL _) (UnsafeVersion majorR _) =
    majorL == majorR
