module HsBindgen.ExtBindings (
    -- * Types
    HsPackageName(..)
  , HsModuleName(..)
  , HsIdentifier(..)
  , ExtIdentifier(..)
  , UnresolvedExtBindings(..)
  , ExtBindings(..)
    -- ** Exceptions
  , LoadUnresolvedExtBindingsException(..)
  , MergeExtBindingsException(..)
  , ExtBindingsException(..)
  , ExtBindingsExceptions(..)
  , WriteUnresolvedExtBindingsException(..)
    -- * API
  , emptyExtBindings
  , resolveExtBindings
  , mergeExtBindings
  , lookupExtBindingsType
  , lookupExtIdentifier
    -- ** Configuration Files
  , loadUnresolvedExtBindings
  , loadUnresolvedExtBindingsJson
  , loadUnresolvedExtBindingsYaml
  , encodeUnresolvedExtBindingsJson
  , encodeUnresolvedExtBindingsYaml
  , writeUnresolvedExtBindings
  , writeUnresolvedExtBindingsJson
  , writeUnresolvedExtBindingsYaml
    -- ** Public API
  , loadExtBindings'
  , loadExtBindings
  ) where

import Control.Exception (Exception(displayException))
import Control.Monad ((<=<))
import Data.Aeson ((.=), (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Either (partitionEithers)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Yaml qualified as Yaml
import Data.Yaml.Internal qualified

import Clang.Args
import Clang.CNameSpelling
import Clang.Paths
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Orphans ()
import HsBindgen.Resolve

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Haskell package name
--
-- Example: @hs-bindgen-runtime@
newtype HsPackageName = HsPackageName { getHsPackageName :: Text }
  deriving stock (Generic)
  -- 'Show' instance valid due to 'IsString' instance
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, Eq, IsString, Ord, Show)

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
-- include a 'HsBindgen.Hs.AST.Namespace'.
newtype HsIdentifier = HsIdentifier { getHsIdentifier :: Text }
  deriving stock (Generic)
  -- 'Show' instance valid due to 'IsString' instance
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, Eq, IsString, Ord, Show)

-- | External identifier
data ExtIdentifier = ExtIdentifier {
      extIdentifierPackage    :: HsPackageName
    , extIdentifierModule     :: HsModuleName
    , extIdentifierIdentifier :: HsIdentifier
    }
  deriving stock (Eq, Generic, Ord, Show)

-- | External bindings with unresolved header paths
--
-- Header paths are relative, as specified in the configuration file.
newtype UnresolvedExtBindings = UnresolvedExtBindings {
      -- | Types mapping
      --
      -- For a given 'CNameSpelling', the sets of 'CHeaderIncludePath' are
      -- disjoint.  The type is therefore equivalent to
      -- @'Map' 'CNameSpelling' ('Map' 'CHeaderIncludePath' 'ExtIdentifier')@
      -- but this type is used as an optimization.  In most cases, each
      -- 'CNameSpelling' is mapped to exactly one value with a set of few
      -- headers.
      unresolvedExtBindingsTypes ::
        Map CNameSpelling [(Set CHeaderIncludePath, ExtIdentifier)]
    }
  deriving (Eq, Show)

-- | External bindings with resolved header paths
newtype ExtBindings = ExtBindings {
      -- | Types mapping
      --
      -- See the documentation for 'unresolvedExtBindingsTypes'.
      extBindingsTypes ::
        Map CNameSpelling [(Set SourcePath, ExtIdentifier)]
    }
  deriving (Eq, Show)

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | Failed to load external bindings configuration file
data LoadUnresolvedExtBindingsException =
    -- | Unknown file extension
    LoadUnresolvedExtBindingsUnknownExtension FilePath
  | -- | Aeson parsing error
    LoadUnresolvedExtBindingsAesonError FilePath String
  | -- | YAML parsing error
    LoadUnresolvedExtBindingsYamlError FilePath Yaml.ParseException
  | -- | YAML parsing warnings (which should be treated like errors)
    LoadUnresolvedExtBindingsYamlWarning FilePath [Data.Yaml.Internal.Warning]
    -- | Multiple external bindings configurations for the same C name and
    -- header in the same configuration file
  | LoadUnresolvedExtBindingsConflict
      FilePath
      (Set (CNameSpelling, CHeaderIncludePath))
  deriving stock (Show)

instance Exception LoadUnresolvedExtBindingsException where
  displayException = \case
    LoadUnresolvedExtBindingsUnknownExtension path ->
      "unknown extension: " ++ path
    LoadUnresolvedExtBindingsAesonError path err ->
      "error parsing JSON: " ++ path ++ ": " ++ err
    LoadUnresolvedExtBindingsYamlError path err -> unlines [
        "error parsing YAML: " ++ path
      , Yaml.prettyPrintParseException err
      ]
    LoadUnresolvedExtBindingsYamlWarning path warnings ->
      unlines $
          ("duplicate keys in YAML file: " ++ path)
        : [ "  " ++ Data.Aeson.Types.formatPath jsonPath
          | Data.Yaml.Internal.DuplicateKey jsonPath <- warnings
          ]
    LoadUnresolvedExtBindingsConflict path conflicts ->
      unlines $
          ( "multiple external bindings for same C name and header: "
              ++ path
          )
        : [ "  " ++ Text.unpack (getCNameSpelling cname)
              ++ ' ' : getCHeaderIncludePath header
          | (cname, header) <- Set.toAscList conflicts
          ]

-- | Failed to merge external bindings
newtype MergeExtBindingsException =
    -- | Multiple external bindings configurations for the same C name and
    -- header
    MergeExtBindingsConflict (Set CNameSpelling)
  deriving stock (Show)

instance Exception MergeExtBindingsException where
  displayException = \case
    MergeExtBindingsConflict cnameSet ->
      unlines $
          "conflicting external bindings for same C name and header:"
        : [ "  " ++ Text.unpack (getCNameSpelling cname)
          | cname <- Set.toAscList cnameSet
          ]

-- | Failed loading, resolving, or merging external bindings
data ExtBindingsException =
    LoadUnresolvedExtBindingsException LoadUnresolvedExtBindingsException
  | MergeExtBindingsException          MergeExtBindingsException
  deriving stock (Show)

instance Exception ExtBindingsException where
  displayException = \case
    LoadUnresolvedExtBindingsException e -> displayException e
    MergeExtBindingsException          e -> displayException e

-- | Failed loading, resolving, or merging external bindings
newtype ExtBindingsExceptions = ExtBindingsExceptions [ExtBindingsException]
  deriving stock (Show)

instance Exception ExtBindingsExceptions where
  displayException (ExtBindingsExceptions es) =
    unlines $ map displayException es

-- | Failed to write external bindings configuration file
newtype WriteUnresolvedExtBindingsException =
    WriteUnresolvedExtBindingsUnknownExtension FilePath
  deriving stock (Show)

instance Exception WriteUnresolvedExtBindingsException where
  displayException = \case
    WriteUnresolvedExtBindingsUnknownExtension path ->
      "unknown extension: " ++ path

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Empty external bindings
emptyExtBindings :: ExtBindings
emptyExtBindings = ExtBindings Map.empty

-- | Resolve external bindings header paths
resolveExtBindings ::
     ClangArgs
  -> UnresolvedExtBindings
  -> IO (Set ResolveHeaderException, ExtBindings)
resolveExtBindings args UnresolvedExtBindings{..} = do
    let cPaths = Set.toAscList . mconcat $
          fst <$> mconcat (Map.elems unresolvedExtBindingsTypes)
    (errs, headerMap) <- bimap Set.fromList Map.fromList . partitionEithers
      <$> mapM (\cPath -> fmap (cPath,) <$> resolveHeader' args cPath) cPaths
    let resolveSet :: Set CHeaderIncludePath -> Set SourcePath
        resolveSet =
            Set.fromList
          . mapMaybe (`Map.lookup` headerMap)
          . Set.toList
        resolve1 ::
             (Set CHeaderIncludePath, ExtIdentifier)
          -> Maybe (Set SourcePath, ExtIdentifier)
        resolve1 (sU, eId) = case resolveSet sU of
          sR
            | Set.null sR -> Nothing
            | otherwise   -> Just (sR, eId)
        resolve ::
             [(Set CHeaderIncludePath, ExtIdentifier)]
          -> Maybe [(Set SourcePath, ExtIdentifier)]
        resolve lU = case mapMaybe resolve1 lU of
          lR
            | null lR   -> Nothing
            | otherwise -> Just lR
        extBindingsTypes = Map.mapMaybe resolve unresolvedExtBindingsTypes
    return (errs, ExtBindings{..})

-- | Merge external bindings
mergeExtBindings ::
     [ExtBindings]
  -> Either MergeExtBindingsException ExtBindings
mergeExtBindings = \case
    []   -> Right emptyExtBindings
    x:xs -> do
      extBindingsTypes <- aux Set.empty (extBindingsTypes x) $
        concatMap (Map.toList . extBindingsTypes) xs
      return ExtBindings{..}
  where
    aux ::
         Set CNameSpelling
      -> Map CNameSpelling [(Set SourcePath, ExtIdentifier)]
      -> [(CNameSpelling, [(Set SourcePath, ExtIdentifier)])]
      -> Either
           MergeExtBindingsException
           (Map CNameSpelling [(Set SourcePath, ExtIdentifier)])
    aux dupSet acc = \case
      []
        | Set.null dupSet -> Right acc
        | otherwise -> Left $ MergeExtBindingsConflict dupSet
      (cname, rs):ps ->
        case Map.insertLookupWithKey (const (++)) cname rs acc of
          (Nothing, acc') -> aux dupSet acc' ps
          (Just ls, acc') ->
            let lHeaderSet = Set.unions $ fst <$> ls
                rHeaderSet = Set.unions $ fst <$> rs
                iHeaderSet = Set.intersection lHeaderSet rHeaderSet
            in  if Set.null iHeaderSet
                  then aux dupSet acc' ps
                  else aux (Set.insert cname dupSet) acc' ps

-- | Lookup a type C name spelling in external bindings
lookupExtBindingsType ::
     CNameSpelling
  -> ExtBindings
  -> Maybe [(Set SourcePath, ExtIdentifier)]
lookupExtBindingsType cname = Map.lookup cname . extBindingsTypes

-- | Lookup an 'ExtIdentifier' associated with a set of header paths when at
-- least one in common with the specified set of header paths
--
-- This is purposefully separate from 'lookupExtBindingsType' because we do not
-- even need to compute the set of header paths unless there is a match for the
-- C name spelling.
lookupExtIdentifier ::
     Set SourcePath
  -> [(Set SourcePath, ExtIdentifier)]
  -> Maybe ExtIdentifier
lookupExtIdentifier headerSet = aux
  where
    aux :: [(Set SourcePath, ExtIdentifier)] -> Maybe ExtIdentifier
    aux = \case
      (extHeaderSet, extId):ps
        | Set.null (headerSet `Set.intersection` extHeaderSet) -> aux ps
        | otherwise                                            -> Just extId
      []                                                       -> Nothing

{-------------------------------------------------------------------------------
  Configuration Files
-------------------------------------------------------------------------------}

-- | Load 'UnresolvedExtBindings' from a configuration file
--
-- The format is determined by the filename extension.
loadUnresolvedExtBindings ::
     FilePath
  -> IO (Either LoadUnresolvedExtBindingsException UnresolvedExtBindings)
loadUnresolvedExtBindings path
    | ".yaml" `List.isSuffixOf` path = loadUnresolvedExtBindingsYaml path
    | ".json" `List.isSuffixOf` path = loadUnresolvedExtBindingsJson path
    | otherwise = return $ Left (LoadUnresolvedExtBindingsUnknownExtension path)

-- | Load 'UnresolvedExtBindings' from a JSON file
loadUnresolvedExtBindingsJson ::
     FilePath
  -> IO (Either LoadUnresolvedExtBindingsException UnresolvedExtBindings)
loadUnresolvedExtBindingsJson path = do
    eec <- Aeson.eitherDecodeFileStrict' path
    return $ case eec of
      Right config -> mkUnresolvedExtBindings path config
      Left err     -> Left (LoadUnresolvedExtBindingsAesonError path err)

-- | Load 'UnresolvedExtBindings' from a YAML file
loadUnresolvedExtBindingsYaml ::
     FilePath
  -> IO (Either LoadUnresolvedExtBindingsException UnresolvedExtBindings)
loadUnresolvedExtBindingsYaml path = do
    eewc <- Yaml.decodeFileWithWarnings path
    return $ case eewc of
      Right ([], config) -> mkUnresolvedExtBindings path config
      Right (warnings, _) ->
        Left (LoadUnresolvedExtBindingsYamlWarning path warnings)
      Left err -> Left (LoadUnresolvedExtBindingsYamlError path err)

-- | Encode 'UnresolvedExtBindings' as JSON
encodeUnresolvedExtBindingsJson :: UnresolvedExtBindings -> BSL.ByteString
encodeUnresolvedExtBindingsJson = Aeson.encode . encodeUnresolvedExtBindings

-- | Encode 'UnresolvedExtBindings' as YAML
encodeUnresolvedExtBindingsYaml :: UnresolvedExtBindings -> ByteString
encodeUnresolvedExtBindingsYaml = Yaml.encode . encodeUnresolvedExtBindings

-- | Write 'UnresolvedExtBindings' to a configuration file
--
-- The format is determined by the filename extension.
writeUnresolvedExtBindings ::
     FilePath
  -> UnresolvedExtBindings
  -> IO (Either WriteUnresolvedExtBindingsException ())
writeUnresolvedExtBindings path bindings
    | ".yaml" `List.isSuffixOf` path =
        Right <$> writeUnresolvedExtBindingsYaml path bindings
    | ".json" `List.isSuffixOf` path =
        Right <$> writeUnresolvedExtBindingsJson path bindings
    | otherwise =
        return $ Left (WriteUnresolvedExtBindingsUnknownExtension path)

-- | Write 'UnresolvedExtBindings' to a JSON file
writeUnresolvedExtBindingsJson :: FilePath -> UnresolvedExtBindings -> IO ()
writeUnresolvedExtBindingsJson path =
    Aeson.encodeFile path . encodeUnresolvedExtBindings

-- | Write 'UnresolvedExtBindings' to a YAML file
writeUnresolvedExtBindingsYaml :: FilePath -> UnresolvedExtBindings -> IO ()
writeUnresolvedExtBindingsYaml path =
    Yaml.encodeFile path . encodeUnresolvedExtBindings

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

-- | Load, resolve, and merge external bindings
--
-- The format is determined by filename extension.
loadExtBindings' ::
     ClangArgs
  -> [FilePath]
  -> IO (Either ExtBindingsExceptions (Set ResolveHeaderException, ExtBindings))
loadExtBindings' args paths = do
    (errs, uebs) <-
      first (map LoadUnresolvedExtBindingsException) . partitionEithers
        <$> mapM loadUnresolvedExtBindings paths
    (resolveErrs, ebs) <-
      first Set.unions . unzip <$> mapM (resolveExtBindings args) uebs
    return $ case first MergeExtBindingsException (mergeExtBindings ebs) of
      Right extBindings
        | null errs -> Right (resolveErrs, extBindings)
        | otherwise -> Left $ ExtBindingsExceptions errs
      Left mergeErr -> Left $ ExtBindingsExceptions (errs ++ [mergeErr])

-- | Load, resolve, and merge external bindings, throwing an
-- 'HsBindgenException' on error
--
-- The format is determined by filename extension.
loadExtBindings ::
     ClangArgs
  -> [FilePath]
  -> IO (Set ResolveHeaderException, ExtBindings)
loadExtBindings args =
    either (throwIO . HsBindgenException) return <=< loadExtBindings' args

{-------------------------------------------------------------------------------
  Configuration File Representation (Internal)
-------------------------------------------------------------------------------}

-- | Configuration file
newtype Config = Config {
      configTypes :: [Mapping]
    }
  deriving (Generic, Show)

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "Config" $ \obj -> do
      configTypes <- obj .: "types"
      return Config{configTypes}

instance Aeson.ToJSON Config where
  toJSON config = Aeson.object [
        "types" .= configTypes
      ]
    where
      Config{configTypes} = config

-- | Mapping from C name and headers to Haskell package, module, and identifier
data Mapping = Mapping {
      mappingCName      :: CNameSpelling
    , mappingHeaders    :: [CHeaderIncludePath]
    , mappingIdentifier :: HsIdentifier
    , mappingModule     :: HsModuleName
    , mappingPackage    :: HsPackageName
    }
  deriving (Generic, Show)

instance Aeson.FromJSON Mapping where
  parseJSON = Aeson.withObject "Mapping" $ \obj -> do
      mappingCName      <- obj .: "cname"
      mappingHeaders    <- obj .: "headers"
      mappingIdentifier <- obj .: "identifier"
      mappingModule     <- obj .: "module"
      mappingPackage    <- obj .: "package"
      return Mapping {
          mappingCName
        , mappingHeaders
        , mappingIdentifier
        , mappingModule
        , mappingPackage
        }

instance Aeson.ToJSON Mapping where
  toJSON mapping = Aeson.object [
        "cname"      .= mappingCName
      , "headers"    .= mappingHeaders
      , "identifier" .= mappingIdentifier
      , "module"     .= mappingModule
      , "package"    .= mappingPackage
      ]
    where
      Mapping {
          mappingCName
        , mappingHeaders
        , mappingIdentifier
        , mappingModule
        , mappingPackage
        } = mapping

{-------------------------------------------------------------------------------
  Auxiliary Functions (Internal)
-------------------------------------------------------------------------------}

-- | Create 'UnresolvedExtBindings' from a 'Config'
mkUnresolvedExtBindings ::
     FilePath
  -> Config
  -> Either LoadUnresolvedExtBindingsException UnresolvedExtBindings
mkUnresolvedExtBindings path Config{..} = do
    unresolvedExtBindingsTypes <- mkMap configTypes
    return UnresolvedExtBindings{..}
  where
    mkMap ::
         [Mapping]
      -> Either
           LoadUnresolvedExtBindingsException
           (Map CNameSpelling [(Set CHeaderIncludePath, ExtIdentifier)])
    mkMap = mkMapErr . foldr mkMapInsert (Map.empty, Map.empty)

    mkMapErr ::
         (Map CNameSpelling (Set CHeaderIncludePath), a)
      -> Either LoadUnresolvedExtBindingsException a
    mkMapErr (dupMap, x)
      | Map.null dupMap = Right x
      | otherwise = Left . LoadUnresolvedExtBindingsConflict path $ Set.fromList
          [ (cname, header)
          | (cname, headerSet) <- Map.toList dupMap
          , header <- Set.toList headerSet
          ]

    mkMapInsert ::
         Mapping
      -> ( Map CNameSpelling (Set CHeaderIncludePath)
         , Map CNameSpelling [(Set CHeaderIncludePath, ExtIdentifier)]
         )
      -> ( Map CNameSpelling (Set CHeaderIncludePath)
         , Map CNameSpelling [(Set CHeaderIncludePath, ExtIdentifier)]
         )
    mkMapInsert Mapping{..} (dupMap, accMap) =
      let extIdentifier = ExtIdentifier {
              extIdentifierPackage    = mappingPackage
            , extIdentifierModule     = mappingModule
            , extIdentifierIdentifier = mappingIdentifier
            }
          newV = [(Set.fromList mappingHeaders, extIdentifier)]
      in  case Map.insertLookupWithKey (const (++)) mappingCName newV accMap of
            (Nothing,   accMap') -> (dupMap, accMap')
            (Just oldV, accMap') ->
              (mkMapDup mappingCName newV oldV dupMap, accMap')

    mkMapDup ::
         CNameSpelling
      -> [(Set CHeaderIncludePath, ExtIdentifier)]
      -> [(Set CHeaderIncludePath, ExtIdentifier)]
      -> Map CNameSpelling (Set CHeaderIncludePath)
      -> Map CNameSpelling (Set CHeaderIncludePath)
    mkMapDup cname newV oldV dupMap =
      let commonHeaders =
            Set.intersection (mconcat (fst <$> newV)) (mconcat (fst <$> oldV))
      in  if Set.null commonHeaders
            then dupMap
            else Map.insertWith Set.union cname commonHeaders dupMap

encodeUnresolvedExtBindings :: UnresolvedExtBindings -> Config
encodeUnresolvedExtBindings UnresolvedExtBindings{..} = Config{..}
  where
    configTypes :: [Mapping]
    configTypes = [
        Mapping {
            mappingCName      = cname
          , mappingHeaders    = Set.toAscList headerSet
          , mappingIdentifier = extIdentifierIdentifier
          , mappingModule     = extIdentifierModule
          , mappingPackage    = extIdentifierPackage
          }
      | (cname, rs) <- Map.toAscList unresolvedExtBindingsTypes
      , (headerSet, ExtIdentifier{..}) <- rs
      ]
