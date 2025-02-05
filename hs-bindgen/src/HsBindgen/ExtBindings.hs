module HsBindgen.ExtBindings (
    -- * Types
    CNameSpelling
  , CHeaderPath
  , HsPackageName
  , HsModuleName
  , HsIdent
  , ExtIdentifier(..)
  , ExtBindings(..)
    -- * Configuration Files
  , loadJson
  , loadYaml
  ) where

import Data.Aeson qualified as Aeson
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Yaml qualified as Yaml

import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Spelling of a C name
--
-- A value must specify @struct@ or @union@ when required.
--
-- Examples: @int8_t@, @struct tm@
type CNameSpelling = Text

-- | C header path
--
-- A value must be specified as used in the C source code (relative to an
-- include directory).
--
-- Example: @time.h@
type CHeaderPath = Text

-- | Haskell package name
--
-- Example: @hs-bindgen-runtime@
type HsPackageName = Text

-- | Haskell module name
--
-- Example: @HsBindgen.Runtime.LibC@
type HsModuleName = Text

-- | Haskell identifier
--
-- Example @CTm@
type HsIdent = Text

-- | External identifier
data ExtIdentifier = ExtIdentifier {
      extIdentifierPackage :: HsPackageName
    , extIdentifierModule  :: HsModuleName
    , extIdentifierIdent   :: HsIdent
    }
  deriving (Eq, Ord, Show)

-- | External bindings
newtype ExtBindings = ExtBindings {
      extBindingsTypes :: Map CNameSpelling ([CHeaderPath], ExtIdentifier)
    }
  deriving Show

{-------------------------------------------------------------------------------
  Configuration File Representation (Internal)
-------------------------------------------------------------------------------}

-- | Configuration file
newtype Config = Config {
      configTypes :: [Mapping]
    }
  deriving (Generic, Show)

instance Aeson.FromJSON Config where
  parseJSON = Aeson.genericParseJSON $
    Aeson.defaultOptions {
        Aeson.fieldLabelModifier = stripPrefix "config"
      }

-- | Mapping from C name and headers to Haskell package, module, and identifier
data Mapping = Mapping {
      mappingCname      :: CNameSpelling
    , mappingHeaders    :: [CHeaderPath]
    , mappingIdentifier :: HsIdent
    , mappingModule     :: HsModuleName
    , mappingPackage    :: HsPackageName
    }
  deriving (Generic, Show)

instance Aeson.FromJSON Mapping where
  parseJSON = Aeson.genericParseJSON $
    Aeson.defaultOptions {
        Aeson.fieldLabelModifier = stripPrefix "mapping"
      }

{-------------------------------------------------------------------------------
  Configuration Files
-------------------------------------------------------------------------------}

-- | Load 'ExtBindings' from a JSON file
--
-- This function fails on error.
loadJson :: FilePath -> IO ExtBindings
loadJson path =
        failOnError path . (mkExtBindings =<<)
    =<< Aeson.eitherDecodeFileStrict' path

-- | Load 'ExtBindings' from a YAML file
--
-- This function fails on error.
loadYaml :: FilePath -> IO ExtBindings
loadYaml path =
        failOnError path . (mkExtBindings =<<)
    =<< decodeYamlStrict path

{-------------------------------------------------------------------------------
  Auxiliary Functions (Internal)
-------------------------------------------------------------------------------}

-- | Convert a field name to a JSON object key by stripping a prefix and
-- converting to @snake_case@
stripPrefix :: String -> String -> String
stripPrefix prefix s = case List.stripPrefix prefix s of
    Just s' | not (null s') -> Aeson.camelTo2 '_' s'
    _otherwise  -> s

-- | Decode a YAML file, treating warnings as errors
decodeYamlStrict :: Aeson.FromJSON a => FilePath -> IO (Either String a)
decodeYamlStrict path = do
    eewx <- Yaml.decodeFileWithWarnings path
    return $ case eewx of
      Right ([], x)        -> Right x
      Right (warnings, _x) -> Left $ show warnings
      Left err             -> Left $ show err

-- | Create 'ExtBindings' from a 'Config'
--
-- An error is returned if any invariants are violated.
mkExtBindings :: Config -> Either String ExtBindings
mkExtBindings Config{..} = do
    extBindingsTypes <- handleDups "duplicate types cnames: " $
      foldr typesInsert (Set.empty, Map.empty) configTypes
    return ExtBindings {..}
  where
    handleDups :: String -> (Set CNameSpelling, a) -> Either String a
    handleDups prefix (dupSet, x)
      | Set.null dupSet = Right x
      | otherwise = Left $
          prefix ++ List.intercalate ", " (Text.unpack <$> Set.toAscList dupSet)

    typesInsert ::
         Mapping
      -> (Set CNameSpelling, Map CNameSpelling ([CHeaderPath], ExtIdentifier))
      -> (Set CNameSpelling, Map CNameSpelling ([CHeaderPath], ExtIdentifier))
    typesInsert Mapping{..} (dupSet, typesMap) =
      let extIdentifier = ExtIdentifier {
              extIdentifierPackage = mappingPackage
            , extIdentifierModule  = mappingModule
            , extIdentifierIdent   = mappingIdentifier
            }
          v = (mappingHeaders, extIdentifier)
      in  case Map.insertLookupWithKey useNewValue mappingCname v typesMap of
            (Nothing, typesMap') -> (dupSet,                         typesMap')
            (Just{},  typesMap') -> (Set.insert mappingCname dupSet, typesMap')

    useNewValue :: k -> a -> a -> a
    useNewValue _key new _old = new

-- | Fail on error, indicating the path
failOnError :: FilePath -> Either String a -> IO a
failOnError path = \case
    Right x  -> return x
    Left err -> fail $ "error loading " ++ path ++ ": " ++ err
