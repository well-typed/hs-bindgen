module HsBindgen.ExtBindings (
    -- * Types
    HsPackageName(..)
  , HsModuleName(..)
  , HsIdentifier(..)
  , ExtIdentifier(..)
  , UnresolvedExtBindings(..)
  , ExtBindings(..)
    -- * Configuration Files
  , loadJson
  , loadYaml
    -- * Resolution
  , resolveExtBindings
  ) where

import Data.Aeson qualified as Aeson
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Yaml qualified as Yaml

import HsBindgen.Clang.CNameSpelling
import HsBindgen.Clang.Paths
import HsBindgen.Imports
import HsBindgen.Orphans ()

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Haskell package name
--
-- Example: @hs-bindgen-runtime@
newtype HsPackageName = HsPackageName { getHsPackageName :: Text }
  deriving newtype (Aeson.FromJSON, Eq, Ord, Show)

-- | Haskell module name
--
-- Example: @HsBindgen.Runtime.LibC@
newtype HsModuleName = HsModuleName { getHsModuleName :: Text }
  deriving newtype (Aeson.FromJSON, Eq, Ord, Show)

-- | Haskell identifier
--
-- Example: @CTm@
--
-- This type is different from 'HsBindgen.Hs.AST.HsName' in that it does not
-- include a 'HsBindgen.Hs.AST.Namespace'.
newtype HsIdentifier = HsIdentifier { getHsIdentifier :: Text }
  deriving newtype (Aeson.FromJSON, Eq, Ord, Show)

-- | External identifier
data ExtIdentifier = ExtIdentifier {
      extIdentifierPackage    :: HsPackageName
    , extIdentifierModule     :: HsModuleName
    , extIdentifierIdentifier :: HsIdentifier
    }
  deriving (Eq, Ord, Show)

-- | External bindings with unresolved header paths
--
-- Header paths are relative, as specified in the configuration file.
newtype UnresolvedExtBindings = UnresolvedExtBindings {
      unresolvedExtBindingsTypes ::
        Map CNameSpelling (Set CHeaderRelPath, ExtIdentifier)
    }
  deriving Show

-- | External bindings
--
-- Header paths are absolute and confirmed to exist.
newtype ExtBindings = ExtBindings {
      extBindingsTypes :: Map CNameSpelling (Set CHeaderAbsPath, ExtIdentifier)
    }
  deriving Show

{-------------------------------------------------------------------------------
  Configuration Files
-------------------------------------------------------------------------------}

-- | Load 'ExtBindings' from a JSON file
--
-- This function fails on error.
loadJson :: FilePath -> IO UnresolvedExtBindings
loadJson path =
        failOnError' path . (mkUnresolvedExtBindings =<<)
    =<< Aeson.eitherDecodeFileStrict' path

-- | Load 'ExtBindings' from a YAML file
--
-- This function fails on error.
loadYaml :: FilePath -> IO UnresolvedExtBindings
loadYaml path =
        failOnError' path . (mkUnresolvedExtBindings =<<)
    =<< decodeYamlStrict path

{-------------------------------------------------------------------------------
  Resolution
-------------------------------------------------------------------------------}

-- | Resolve external bindings header paths
--
-- This function fails on error.
resolveExtBindings ::
     [CIncludePathDir]
  -> UnresolvedExtBindings
  -> IO ExtBindings
resolveExtBindings includePathDirs UnresolvedExtBindings{..} = do
    let relPathSet = mconcat (fst <$> Map.elems unresolvedExtBindingsTypes)
    headerMap <- fmap Map.fromList . forM (Set.toList relPathSet) $ \relPath ->
      (relPath,) <$> resolveHeader includePathDirs relPath
    let resolve'         = Set.map (headerMap Map.!)
        extBindingsTypes = Map.map (first resolve') unresolvedExtBindingsTypes
    return ExtBindings{..}

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
    , mappingHeaders    :: [CHeaderRelPath]
    , mappingIdentifier :: HsIdentifier
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
  Auxiliary Functions (Internal)
-------------------------------------------------------------------------------}

-- | Convert a field name to a JSON object key by stripping a prefix and
-- converting to @snake_case@
stripPrefix :: String -> String -> String
stripPrefix prefix s = case List.stripPrefix prefix s of
    Just s' | not (null s') -> Aeson.camelTo2 '_' s'
    _otherwise  -> s

-- | Fail on error, indicating the path
failOnError' :: FilePath -> Either String a -> IO a
failOnError' path = \case
    Right x  -> return x
    Left err -> fail $ "error loading " ++ path ++ ": " ++ err

-- | Decode a YAML file, treating warnings as errors
decodeYamlStrict :: Aeson.FromJSON a => FilePath -> IO (Either String a)
decodeYamlStrict path = do
    eewx <- Yaml.decodeFileWithWarnings path
    return $ case eewx of
      Right ([], x)        -> Right x
      Right (warnings, _x) -> Left $ show warnings
      Left err             -> Left $ show err

-- | Create 'UnresolvedExtBindings' from a 'Config'
--
-- An error is returned if any invariants are violated.
mkUnresolvedExtBindings :: Config -> Either String UnresolvedExtBindings
mkUnresolvedExtBindings Config{..} = do
    unresolvedExtBindingsTypes <- handleDups "duplicate types cnames: " $
      foldr typesInsert (Set.empty, Map.empty) configTypes
    return UnresolvedExtBindings{..}
  where
    handleDups :: String -> (Set Text, a) -> Either String a
    handleDups prefix (dupSet, x)
      | Set.null dupSet = Right x
      | otherwise = Left $
          prefix ++ List.intercalate ", " (Text.unpack <$> Set.toAscList dupSet)

    typesInsert ::
         Mapping
      -> (Set Text, Map CNameSpelling (Set CHeaderRelPath, ExtIdentifier))
      -> (Set Text, Map CNameSpelling (Set CHeaderRelPath, ExtIdentifier))
    typesInsert Mapping{..} (dupSet, typesMap) =
      let extIdentifier = ExtIdentifier {
              extIdentifierPackage    = mappingPackage
            , extIdentifierModule     = mappingModule
            , extIdentifierIdentifier = mappingIdentifier
            }
          v = (Set.fromList mappingHeaders, extIdentifier)
          dupSet' = Set.insert (getCNameSpelling mappingCname) dupSet
      in  case Map.insertLookupWithKey useNewValue mappingCname v typesMap of
            (Nothing, typesMap') -> (dupSet,  typesMap')
            (Just{},  typesMap') -> (dupSet', typesMap')

    useNewValue :: k -> a -> a -> a
    useNewValue _key new _old = new
