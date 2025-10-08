{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Binding specification version management
--
-- This /private/ module may only be used by "HsBindgen.BindingSpec" and
-- sub-modules.
--
-- Intended for unqualified import.
module HsBindgen.BindingSpec.Private.Version (
    -- * HsBindgenVersion
    HsBindgenVersion
  , prettyForTraceHsBindgenVersion
    -- * BindingSpecVersion
  , BindingSpecVersion
  , constBindingSpecVersion
  , parseBindingSpecVersion
    -- * Compatibility
  , BindingSpecCompatibility(..)
  , isCompatBindingSpecVersions
    -- * AVersion
  , AVersion(..)
  , mkAVersion
  , getAVersion
  ) where

import Data.Aeson ((.:), (.=))
import Data.Aeson.Types qualified as Aeson
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Version qualified
import Language.Haskell.TH.Syntax qualified as THS
import Text.Read (readMaybe)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Errors (failCode)
import HsBindgen.Imports
import HsBindgen.Util.Tracer (PrettyForTrace (prettyForTrace))

import Paths_hs_bindgen qualified as Package

{-------------------------------------------------------------------------------
  HsBindgenVersion
-------------------------------------------------------------------------------}

-- | @hs-bindgen@ version
type HsBindgenVersion = Data.Version.Version

prettyForTraceHsBindgenVersion :: HsBindgenVersion -> PP.CtxDoc
prettyForTraceHsBindgenVersion = PP.string . Data.Version.showVersion

{-------------------------------------------------------------------------------
  BindingSpecVersion
-------------------------------------------------------------------------------}

-- | Binding specification version
--
-- Binding specification versions are in @MAJOR.MINOR@ format where
-- @MAJOR >= 1@ and @MINOR >= 0@.
data BindingSpecVersion = UnsafeBindingSpecVersion Int Int
  deriving stock (Eq, Ord, THS.Lift)

instance Aeson.FromJSON BindingSpecVersion where
  parseJSON = Aeson.withText "BindingSpecVersion" $ \t ->
    case parseBindingSpecVersion t of
      Right v -> return v
      Left  e -> Aeson.parseFail $
        "Invalid binding specification version: " ++ e ++ ": " ++ show t

instance Aeson.ToJSON BindingSpecVersion where
  toJSON = Aeson.String . Text.pack . show

instance PrettyForTrace BindingSpecVersion where
  prettyForTrace = PP.string . show

instance Show BindingSpecVersion where
  show (UnsafeBindingSpecVersion x y) = show x ++ '.' : show y

-- | Construct a 'BindingSpecVersion'
mkBindingSpecVersion ::
     Int  -- ^ Major version (@>= 1@)
  -> Int  -- ^ Minor version (@>= 0@)
  -> Either String BindingSpecVersion
mkBindingSpecVersion major minor
    | major < 1 = Left "major version less than 1"
    | minor < 0 = Left "minor version less than 0"
    | otherwise = Right (UnsafeBindingSpecVersion major minor)

-- | Construct a 'BindingSpecVersion', validating it a compile-time
constBindingSpecVersion ::
     (MonadFail m, THS.Quote m)
  => Int  -- ^ Major version
  -> Int  -- ^ Minor version
  -> THS.Code m BindingSpecVersion
constBindingSpecVersion major minor = case mkBindingSpecVersion major minor of
    Right v -> [|| v ||]
    Left  e -> failCode $ "Invalid binding specification version: " ++ e

-- | Parse a 'BindingSpecVersion'
parseBindingSpecVersion :: Text -> Either String BindingSpecVersion
parseBindingSpecVersion t =
    case List.uncons <$> span Char.isDigit (Text.unpack t) of
      (majorS, Just ('.', minorS)) ->
        case (readMaybe majorS, readMaybe minorS) of
          (Just major, Just minor) -> mkBindingSpecVersion major minor
          _otherwise               -> Left "not in MAJOR.MINOR format"
      _otherwise -> Left "not in MAJOR.MINOR format"

{-------------------------------------------------------------------------------
  Compatibility
-------------------------------------------------------------------------------}

-- | Binding specification compatibility strictness
data BindingSpecCompatibility =
    -- | Do not allow newer minor versions
    BindingSpecStrict
  | -- | Allow newer minor versions
    BindingSpecAllowNewer
  deriving stock (Eq, Show)

instance Default BindingSpecCompatibility where
  def = BindingSpecStrict

-- | Check 'BindingSpecVersion' compatibility
isCompatBindingSpecVersions ::
     BindingSpecCompatibility
  -> BindingSpecVersion  -- ^ Version of binding specification being read
  -> BindingSpecVersion  -- ^ Version of binding specification module
  -> Bool
isCompatBindingSpecVersions
  BindingSpecStrict
  (UnsafeBindingSpecVersion majorL minorL)
  (UnsafeBindingSpecVersion majorR minorR) =
    majorL == majorR && minorL <= minorR
isCompatBindingSpecVersions
  BindingSpecAllowNewer
  (UnsafeBindingSpecVersion majorL _)
  (UnsafeBindingSpecVersion majorR _) =
    majorL == majorR

{-------------------------------------------------------------------------------
  AVersion
-------------------------------------------------------------------------------}

-- | JSON/YAML version information
data AVersion = AVersion {
      aVersionHsBindgen            :: HsBindgenVersion
    , aVersionBindingSpecification :: BindingSpecVersion
    }
  deriving stock Show

instance Aeson.FromJSON AVersion where
  parseJSON = Aeson.withObject "AVersion" $ \o -> do
    aVersionHsBindgen            <- o .: "hs_bindgen"
    aVersionBindingSpecification <- o .: "binding_specification"
    return AVersion{..}

instance Aeson.ToJSON AVersion where
  toJSON AVersion{..} = Aeson.object [
      "hs_bindgen"            .= aVersionHsBindgen
    , "binding_specification" .= aVersionBindingSpecification
    ]

-- | Construct an 'AVersion' with the current versions
mkAVersion :: BindingSpecVersion -> AVersion
mkAVersion aVersionBindingSpecification =
    let aVersionHsBindgen = Package.version
    in  AVersion{..}

-- | Internal type used to parse just the version information
newtype ABindingSpecVersion = ABindingSpecVersion {
      aVersion :: AVersion
    }
  deriving stock Show

instance Aeson.FromJSON ABindingSpecVersion where
  parseJSON = Aeson.withObject "file" $ \o -> do
    aVersion <- o .: "version"
    return ABindingSpecVersion{..}

instance Aeson.ToJSON ABindingSpecVersion where
  toJSON ABindingSpecVersion{..} = Aeson.object [
      "version" .= aVersion
    ]

-- | Parse just the version information
getAVersion :: Aeson.Value -> Either String AVersion
getAVersion value = case Aeson.fromJSON value of
    Aeson.Success ABindingSpecVersion{..} -> Right aVersion
    Aeson.Error   err                     -> Left  err
