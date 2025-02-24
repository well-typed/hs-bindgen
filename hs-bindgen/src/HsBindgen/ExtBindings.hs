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
      -- | Types mapping
      --
      -- For a given 'CNameSpelling', the sets of 'CHeaderRelPath' are
      -- disjoint.  The type is therefore equivalent to
      -- @'Map' 'CNameSpelling' ('Map' 'CHeaderPath' 'ExtIdentifier')@ but this
      -- type is used as an optimization.  In most cases, each 'CNameSpelling'
      -- is mapped to exactly one value with a set of few headers.
      unresolvedExtBindingsTypes ::
        Map CNameSpelling [(Set CHeaderRelPath, ExtIdentifier)]
    }
  deriving Show

-- | External bindings
--
-- Header paths are absolute and confirmed to exist.
newtype ExtBindings = ExtBindings {
      -- | Types mapping
      --
      -- See the documentation for 'unresolvedExtBindingsTypes'.
      extBindingsTypes ::
        Map CNameSpelling [(Set CHeaderAbsPath, ExtIdentifier)]
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
     [CIncludeAbsPathDir]
  -> UnresolvedExtBindings
  -> IO ExtBindings
resolveExtBindings includePathDirs UnresolvedExtBindings{..} = do
    let relPaths = Set.toAscList . mconcat $
          fst <$> mconcat (Map.elems unresolvedExtBindingsTypes)
    headerMap <- fmap Map.fromList . forM relPaths $ \relPath ->
      either fail (return . (relPath,))
        =<< resolveHeader includePathDirs relPath
    let resolve'         = map $ first $ Set.map (headerMap Map.!)
        extBindingsTypes = Map.map resolve' unresolvedExtBindingsTypes
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
    unresolvedExtBindingsTypes <- first ("error in types: " ++) $
      mkMap configTypes
    return UnresolvedExtBindings{..}
  where
    mkMap ::
         [Mapping]
      -> Either String (Map CNameSpelling [(Set CHeaderRelPath, ExtIdentifier)])
    mkMap = mkMapErr . foldr mkMapInsert (Map.empty, Map.empty)

    mkMapErr :: (Map CNameSpelling (Set CHeaderRelPath), a) -> Either String a
    mkMapErr (dupMap, x)
      | Map.null dupMap = Right x
      | otherwise = Left $ List.intercalate ", " [
            unwords [
                "duplicate mapping for"
              , Text.unpack (getCNameSpelling cname)
              , "in header"
              , getCHeaderRelPath header
              ]
          | (cname, headerSet) <- Map.toAscList dupMap
          , header <- Set.toAscList headerSet
          ]

    mkMapInsert ::
         Mapping
      -> ( Map CNameSpelling (Set CHeaderRelPath)
         , Map CNameSpelling [(Set CHeaderRelPath, ExtIdentifier)]
         )
      -> ( Map CNameSpelling (Set CHeaderRelPath)
         , Map CNameSpelling [(Set CHeaderRelPath, ExtIdentifier)]
         )
    mkMapInsert Mapping{..} (dupMap, accMap) =
      let extIdentifier = ExtIdentifier {
              extIdentifierPackage    = mappingPackage
            , extIdentifierModule     = mappingModule
            , extIdentifierIdentifier = mappingIdentifier
            }
          newV = [(Set.fromList mappingHeaders, extIdentifier)]
      in  case Map.insertLookupWithKey (const (++)) mappingCname newV accMap of
            (Nothing,   accMap') -> (dupMap, accMap')
            (Just oldV, accMap') ->
              (mkMapDup mappingCname newV oldV dupMap, accMap')

    mkMapDup ::
         CNameSpelling
      -> [(Set CHeaderRelPath, ExtIdentifier)]
      -> [(Set CHeaderRelPath, ExtIdentifier)]
      -> Map CNameSpelling (Set CHeaderRelPath)
      -> Map CNameSpelling (Set CHeaderRelPath)
    mkMapDup cname newV oldV dupMap =
      let commonHeaders =
            Set.intersection (mconcat (fst <$> newV)) (mconcat (fst <$> oldV))
      in  if Set.null commonHeaders
            then dupMap
            else Map.insertWith Set.union cname commonHeaders dupMap
