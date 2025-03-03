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
  , ResolveExtBindingsException(..)
  , MergeExtBindingsException(..)
  , ExtBindingsException(..)
  , ExtBindingsExceptions(..)
    -- * API
  , emptyExtBindings
  , resolveExtBindings
  , mergeExtBindings
    -- ** Configuration Files
  , loadUnresolvedExtBindings
  , loadUnresolvedExtBindingsJson
  , loadUnresolvedExtBindingsYaml
    -- ** Convenience
  , loadExtBindings
  ) where

import Data.Aeson qualified as Aeson
import Data.Either (partitionEithers)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Yaml qualified as Yaml
import Data.Yaml.Internal qualified

import HsBindgen.Clang.Args
import HsBindgen.Clang.CNameSpelling
import HsBindgen.Clang.Paths
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
      -- For a given 'CNameSpelling', the sets of 'CHeaderIncludePath' are
      -- disjoint.  The type is therefore equivalent to
      -- @'Map' 'CNameSpelling' ('Map' 'CHeaderIncludePath' 'ExtIdentifier')@
      -- but this type is used as an optimization.  In most cases, each
      -- 'CNameSpelling' is mapped to exactly one value with a set of few
      -- headers.
      unresolvedExtBindingsTypes ::
        Map CNameSpelling [(Set CHeaderIncludePath, ExtIdentifier)]
    }
  deriving Show

-- | External bindings with resolved header paths
newtype ExtBindings = ExtBindings {
      -- | Types mapping
      --
      -- See the documentation for 'unresolvedExtBindingsTypes'.
      extBindingsTypes ::
        Map CNameSpelling [(Set SourcePath, ExtIdentifier)]
    }
  deriving Show

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
      [(CNameSpelling, CHeaderIncludePath)]
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Failed to resolve external bindings header(s)
newtype ResolveExtBindingsException =
    -- | One or more C headers were not found
    ResolveExtBindingsNotFound [CHeaderIncludePath]
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Failed to merge external bindings
newtype MergeExtBindingsException =
    -- | Multiple external bindings configurations for the same C name and
    -- header
    MergeExtBindingsConflict [CNameSpelling]
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Failed loading, resolving, or merging external bindings
data ExtBindingsException =
    LoadUnresolvedExtBindingsException LoadUnresolvedExtBindingsException
  | ResolveExtBindingsException        ResolveExtBindingsException
  | MergeExtBindingsException          MergeExtBindingsException
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Failed loading, resolving, or merging external bindings
newtype ExtBindingsExceptions = ExtBindingsExceptions [ExtBindingsException]
  deriving stock (Show)
  deriving anyclass (Exception)

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
  -> IO (Either ResolveExtBindingsException ExtBindings)
resolveExtBindings args UnresolvedExtBindings{..} = do
    let cPaths = Set.toAscList . mconcat $
          fst <$> mconcat (Map.elems unresolvedExtBindingsTypes)
    (mErr, headerMap) <- bimap convertErrors Map.fromList . partitionEithers
      <$> mapM (\cPath -> fmap (cPath,) <$> resolveHeader' args cPath) cPaths
    return $ case mErr of
      Nothing -> Right $
        let resolve' = map $ first $ Set.map (headerMap Map.!)
            extBindingsTypes = Map.map resolve' unresolvedExtBindingsTypes
        in  ExtBindings{..}
      Just err -> Left err
  where
    convertErrors ::
         [ResolveHeaderException]
      -> Maybe ResolveExtBindingsException
    convertErrors = \case
        []   -> Nothing
        errs -> Just . ResolveExtBindingsNotFound . List.sort $
          map (\(ResolveHeaderNotFound cPath) -> cPath) errs

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
        | otherwise -> Left (MergeExtBindingsConflict (Set.toAscList dupSet))
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

{-------------------------------------------------------------------------------
  Convenience
-------------------------------------------------------------------------------}

-- | Load, resolve, and merge external bindings
--
-- The format is determined by filename extension.
loadExtBindings ::
     ClangArgs
  -> [FilePath]
  -> IO (Either ExtBindingsExceptions ExtBindings)
loadExtBindings args paths = do
    (loadErrs, uebs) <-
      first (map LoadUnresolvedExtBindingsException) . partitionEithers
        <$> mapM loadUnresolvedExtBindings paths
    (resolveErrs, ebs) <-
      first (map ResolveExtBindingsException) . partitionEithers
        <$> mapM (resolveExtBindings args) uebs
    let errs = loadErrs ++ resolveErrs
    return $ case first MergeExtBindingsException (mergeExtBindings ebs) of
      Right extBindings
        | null errs -> Right extBindings
        | otherwise -> Left $ ExtBindingsExceptions errs
      Left mergeErr -> Left $ ExtBindingsExceptions (errs ++ [mergeErr])

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
    , mappingHeaders    :: [CHeaderIncludePath]
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
      | otherwise = Left $ LoadUnresolvedExtBindingsConflict path
          [ (cname, header)
          | (cname, headerSet) <- Map.toAscList dupMap
          , header <- Set.toAscList headerSet
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
      in  case Map.insertLookupWithKey (const (++)) mappingCname newV accMap of
            (Nothing,   accMap') -> (dupMap, accMap')
            (Just oldV, accMap') ->
              (mkMapDup mappingCname newV oldV dupMap, accMap')

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
