{-# LANGUAGE TemplateHaskell #-}

-- | Binding specification
--
-- This /private/ module may only be used by "HsBindgen.BindingSpec" and
-- sub-modules.
--
-- Intended for qualified import.
--
-- When defining the current public interface:
--
-- > import HsBindgen.BindingSpec.Private.V1 qualified as BindingSpec
--
-- When distinguishing separate versions:
--
-- > import HsBindgen.BindingSpec.Private.V1 qualified as V1
module HsBindgen.BindingSpec.Private.V1 (
    -- * Version
    version
    -- * Types
  , BindingSpec(..)
  , UnresolvedBindingSpec
  , ResolvedBindingSpec
  , BindingSpecTarget(..)
  , CTypeSpec(..)
  , HsTypeSpec(..)
    -- ** Instances
  , InstanceSpec(..)
  , StrategySpec(..)
  , ConstraintSpec(..)
    -- * API
  , empty
  , isCompatTarget
  , getCTypes
  , lookupCTypeSpec
  , lookupHsTypeSpec
    -- ** YAML/JSON
  , readFile
  , readFileJson
  , readFileYaml
  , parseValue
  , encodeJson
  , encodeYaml
  , writeFile
  , writeFileJson
  , writeFileYaml
    -- ** Header resolution
  , resolve
    -- ** Merging
  , MergedBindingSpecs
  , merge
  , lookupMergedBindingSpecs
  ) where

import Prelude hiding (readFile, writeFile)

import Data.Aeson ((.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as Aeson
import Data.ByteString qualified as BSS
import Data.ByteString.Lazy qualified as BSL
import Data.Function (on)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Yaml.Pretty qualified

import Clang.Args
import Clang.Paths

import HsBindgen.BindingSpec.Private.Common
import HsBindgen.BindingSpec.Private.Version
import HsBindgen.Config.ClangArgs qualified as ClangArgs
import HsBindgen.Errors
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Orphans ()
import HsBindgen.Resolve
import HsBindgen.Util.Monad
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Version
-------------------------------------------------------------------------------}

-- | Binding specification version
version :: BindingSpecVersion
version = $$(constBindingSpecVersion 1 0)

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Binding specification
--
-- This type serves two purposes:
--
-- * A /prescriptive binding specification/ is used to configure how bindings
--   are generated.
-- * An /external binding specification/ is used to specify existing bindings
--   that should be used, /external/ from the module being generated.
--
-- Note that a /generated binding specification/ may be used for either/both of
-- these two purposes.
--
-- The @header@ type parameter determines the representation of header paths.
-- See 'UnresolvedBindingSpec' and 'ResolvedBindingSpec'.
data BindingSpec header = BindingSpec {
      -- | Binding specification target
      bindingSpecTarget :: BindingSpecTarget

      -- | Binding specification module
      --
      -- Each binding specification is specific to a Haskell module.
    , bindingSpecModule :: Hs.ModuleName

      -- | C type specifications
      --
      -- A C type is identified using a 'C.QualName' and a set of headers that
      -- provide the type.  For a given 'C.QualName', the sets of headers are
      -- disjoint.  The type of this map is therefore equivalent to
      -- @'Map' 'C.QualName' ('Map' header ('Omittable' 'CTypeSpec'))@, but this
      -- type is used as an optimization.
    , bindingSpecCTypes :: Map C.QualName [(Set header, Omittable CTypeSpec)]

      -- | Haskell type specifications
    , bindingSpecHsTypes :: Map Hs.Identifier HsTypeSpec
    }
  deriving stock (Eq, Generic, Show)

-- | Binding specification with unresolved headers
--
-- The headers are as specified in a C include directive, relative to a
-- directory in the C include search path.
type UnresolvedBindingSpec = BindingSpec HashIncludeArg

-- | Binding specification with resolved headers
--
-- The resolved header is the filesystem path in the current environment.
type ResolvedBindingSpec = BindingSpec (HashIncludeArg, SourcePath)

--------------------------------------------------------------------------------

-- | Binding specification target
--
-- The target describes the generated bindings, /not/ just the specifications in
-- the binding specification.
data BindingSpecTarget =
    -- | Bindings should only be considered compatible with a specific target
    SpecificTarget ClangArgs.Target
  | -- | Bindings should be considered compatible with any target
    AnyTarget
  deriving stock (Show, Eq, Generic)

isCompatBindingSpecTarget :: BindingSpecTarget -> ClangArgs.Target -> Bool
isCompatBindingSpecTarget = \case
    AnyTarget             -> const True
    SpecificTarget target -> (== target)

--------------------------------------------------------------------------------

-- | Binding specification for a C type
newtype CTypeSpec = CTypeSpec {
      -- | Haskell identifier
      cTypeSpecIdentifier :: Maybe Hs.Identifier
    }
  deriving stock (Show, Eq, Ord, Generic)

instance Default CTypeSpec where
  def = CTypeSpec {
      cTypeSpecIdentifier = Nothing
    }

--------------------------------------------------------------------------------

-- | Binding specification for a Haskell type
newtype HsTypeSpec = HsTypeSpec {
      -- | Instance specification
      hsTypeSpecInstances :: Map Hs.TypeClass (Omittable InstanceSpec)
    }
  deriving stock (Show, Eq, Ord, Generic)

instance Default HsTypeSpec where
  def = HsTypeSpec {
      hsTypeSpecInstances = Map.empty
    }

{-------------------------------------------------------------------------------
  Types: Instances
-------------------------------------------------------------------------------}

-- | Instance specification
data InstanceSpec = InstanceSpec {
      -- | Strategy used to generate/derive the instance
      --
      -- A 'Nothing' value indicates that @hs-bindgen@ defaults should be used.
      instanceSpecStrategy :: Maybe StrategySpec

    , -- | Instance constraints
      --
      -- If specified, /all/ constraints must be listed.
      instanceSpecConstraints :: [ConstraintSpec]
    }
  deriving stock (Show, Eq, Ord, Generic)

instance Default InstanceSpec where
  def = InstanceSpec {
      instanceSpecStrategy    = Nothing
    , instanceSpecConstraints = []
    }

--------------------------------------------------------------------------------

-- | Strategy used to generate/derive an instance
data StrategySpec =
    -- | Generate an instance
    StrategySpecHsBindgen
  | -- | Derive an instance using the @newtype@ strategy
    StrategySpecNewtype
  | -- | Derive an instance using the @stock@ strategy
    StrategySpecStock
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

--------------------------------------------------------------------------------

-- | Constraint of an instance
data ConstraintSpec = ConstraintSpec {
      constraintSpecClass :: Hs.TypeClass
    , constraintSpecRef   :: Hs.ExtRef
    }
  deriving stock (Show, Eq, Ord, Generic)

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Construct an empty binding specification for the given target and module
empty :: ClangArgs.Target -> Hs.ModuleName -> BindingSpec header
empty target hsModuleName = BindingSpec {
      bindingSpecTarget  = SpecificTarget target
    , bindingSpecModule  = hsModuleName
    , bindingSpecCTypes  = Map.empty
    , bindingSpecHsTypes = Map.empty
    }

-- | Predicate that checks if a binding specification is compatible with a
-- specific target
isCompatTarget :: BindingSpec header -> ClangArgs.Target -> Bool
isCompatTarget = isCompatBindingSpecTarget . bindingSpecTarget

-- | Get the C types in a binding specification
getCTypes :: ResolvedBindingSpec -> Map C.QualName [Set SourcePath]
getCTypes =
      fmap (map (Set.map snd . fst))
    . bindingSpecCTypes

-- | Lookup a C type in a 'ResolvedBindingSpec'
lookupCTypeSpec ::
     C.QualName
  -> Set SourcePath
  -> ResolvedBindingSpec
  -> Maybe (Hs.ModuleName, Omittable CTypeSpec)
lookupCTypeSpec cQualName headers spec = do
    ps <- Map.lookup cQualName (bindingSpecCTypes spec)
    oCTypeSpec <- lookupBy (not . Set.disjoint headers . Set.map snd) ps
    return (bindingSpecModule spec, oCTypeSpec)

-- | Lookup a Haskell type in a 'ResolvedBindingSpec'
lookupHsTypeSpec ::
     Hs.Identifier
  -> ResolvedBindingSpec
  -> Maybe HsTypeSpec
lookupHsTypeSpec hsIdentifier = Map.lookup hsIdentifier . bindingSpecHsTypes

{-------------------------------------------------------------------------------
  API: YAML/JSON
-------------------------------------------------------------------------------}

-- | Read a binding specification file
--
-- The format is determined by the filename extension.
readFile ::
     Tracer BindingSpecReadMsg
  -> BindingSpecCompatibility
  -> FilePath
  -> IO (Maybe UnresolvedBindingSpec)
readFile = readFileAux readVersion

-- | Read a binding specification JSON file
readFileJson ::
     Tracer BindingSpecReadMsg
  -> BindingSpecCompatibility
  -> FilePath
  -> IO (Maybe UnresolvedBindingSpec)
readFileJson = readFileAux readVersionJson

-- | Read a binding specification YAML file
readFileYaml ::
     Tracer BindingSpecReadMsg
  -> BindingSpecCompatibility
  -> FilePath
  -> IO (Maybe UnresolvedBindingSpec)
readFileYaml = readFileAux readVersionYaml

readFileAux ::
     ReadVersionFunction
  -> Tracer BindingSpecReadMsg
  -> BindingSpecCompatibility
  -> FilePath
  -> IO (Maybe UnresolvedBindingSpec)
readFileAux doRead tracer cmpt path = doRead tracer path >>= \case
    Just (version', value) -> parseValue tracer cmpt path version' value
    Nothing                -> return Nothing

parseValue ::
     MonadIO m
  => Tracer BindingSpecReadMsg
  -> BindingSpecCompatibility
  -> FilePath
  -> AVersion
  -> Aeson.Value
  -> m (Maybe UnresolvedBindingSpec)
parseValue tracer cmpt path aVersion@AVersion{..} value
    | isCompatBindingSpecVersions cmpt aVersionBindingSpecification version = do
        traceWith tracer $ BindingSpecReadParseVersion path aVersion
        case Aeson.fromJSON value of
          Aeson.Success aspec -> do
            let (errs, spec) = fromABindingSpec path aspec
            mapM_ (traceWith tracer) errs
            return (Just spec)
          Aeson.Error err -> do
            traceWith tracer $ BindingSpecReadAesonError path err
            return Nothing
    -- | aVersionBindingSpecification < version -> -- no lower versions
    | otherwise = do
        traceWith tracer $ BindingSpecReadIncompatibleVersion path aVersion
        return Nothing

-- | Encode a binding specification as JSON
encodeJson :: UnresolvedBindingSpec -> BSL.ByteString
encodeJson = encodeJson' . toABindingSpec

-- | Encode a binding specification as YAML
encodeYaml :: UnresolvedBindingSpec -> BSS.ByteString
encodeYaml = encodeYaml' . toABindingSpec

-- | Write a binding specification to a file
--
-- The format is determined by the filename extension.
writeFile :: FilePath -> UnresolvedBindingSpec -> IO ()
writeFile path = case getFormat path of
    FormatYAML -> writeFileYaml path
    FormatJSON -> writeFileJson path

-- | Write a binding specification to a JSON file
writeFileJson :: FilePath -> UnresolvedBindingSpec -> IO ()
writeFileJson path = BSL.writeFile path . encodeJson

-- | Write a binding specification to a YAML file
writeFileYaml :: FilePath -> UnresolvedBindingSpec -> IO ()
writeFileYaml path = BSS.writeFile path . encodeYaml

encodeJson' :: ABindingSpec -> BSL.ByteString
encodeJson' = Aeson.encode

encodeYaml' :: ABindingSpec -> BSS.ByteString
encodeYaml' = Data.Yaml.Pretty.encodePretty yamlConfig
  where
    yamlConfig :: Data.Yaml.Pretty.Config
    yamlConfig =
          Data.Yaml.Pretty.setConfCompare (compare `on` keyPosition)
        $ Data.Yaml.Pretty.defConfig

    keyPosition :: Text -> Int
    keyPosition = \case
      "version"               ->  0  -- ABindingSpec:1
      "hs_bindgen"            ->  1  -- AVersion:1
      "binding_specification" ->  2  -- AVersion:2
      "omit"                  ->  3  -- AOmittable:1
      "class"                 ->  4  -- AInstanceSpecMapping:1, AConstraintSpec:1
      "strategy"              ->  5  -- AInstanceSpecMapping:2
      "constraints"           ->  6  -- AInstanceSpecMapping:3
      "target"                ->  7  -- ABindingSpec:2
      "hsmodule"              ->  8  -- ABindingSpec:3, AConstraintSpec:2
      "ctypes"                ->  9  -- ABindingSpec:4
      "hstypes"               -> 10  -- ABindingSpec:5
      "headers"               -> 11  -- ACTypeSpecMapping:1
      "cname"                 -> 12  -- ACTypeSpecMapping:2
      "hsname"                -> 13  -- ACTypeSpecMapping:3, AConstraintSpec:3
      "instances"             -> 14  -- ACTypeSpecMapping:4
      key -> panicPure $ "Unknown key: " ++ show key

{-------------------------------------------------------------------------------
  API: Header resolution
-------------------------------------------------------------------------------}

-- | Resolve headers in a binding specification
resolve ::
     Tracer BindingSpecResolveMsg
  -> (ResolveHeaderMsg -> BindingSpecResolveMsg)
  -> ClangArgs
  -> UnresolvedBindingSpec
  -> IO ResolvedBindingSpec
resolve tracer injResolveHeader args uSpec = do
    headerMap <-
      resolveHeaders (contramap injResolveHeader tracer) args allHeaders

    let lookup' :: HashIncludeArg -> Maybe (HashIncludeArg, SourcePath)
        lookup' uHeader = (uHeader,) <$> Map.lookup uHeader headerMap

        resolveSet ::
             Set HashIncludeArg
          -> Maybe (Set (HashIncludeArg, SourcePath))
        resolveSet uHeaders =
          -- ignore headers that are not found
          case mapMaybe lookup' (Set.toList uHeaders) of
            []       -> Nothing
            rHeaders -> Just (Set.fromList rHeaders)

        resolveType ::
             C.QualName
          -> (Set HashIncludeArg, a)
          -> IO (Maybe (Set (HashIncludeArg, SourcePath), a))
        resolveType cQualName (uHeaders, x) = case resolveSet uHeaders of
          Just rHeaders -> return $ Just (rHeaders, x)
          Nothing       -> do
            traceWith tracer $ BindingSpecResolveTypeDropped cQualName
            return Nothing

        resolveTypes ::
             C.QualName
          -> [(Set HashIncludeArg, a)]
          -> IO
               ( Maybe
                   (C.QualName, [(Set (HashIncludeArg, SourcePath), a)])
               )
        resolveTypes cQualName uKVs =
          mapMaybeM (resolveType cQualName) uKVs >>= \case
            rKVs
              | null rKVs -> return Nothing
              | otherwise -> return $ Just (cQualName, rKVs)

    cTypes <- Map.fromList <$>
      mapMaybeM (uncurry resolveTypes) (Map.toList (bindingSpecCTypes uSpec))
    return BindingSpec{
        bindingSpecTarget  = bindingSpecTarget uSpec
      , bindingSpecModule  = bindingSpecModule uSpec
      , bindingSpecCTypes  = cTypes
      , bindingSpecHsTypes = bindingSpecHsTypes uSpec
      }
  where
    allHeaders :: Set HashIncludeArg
    allHeaders = mconcat $ fst <$> concat (Map.elems (bindingSpecCTypes uSpec))

{-------------------------------------------------------------------------------
  API: Merging
-------------------------------------------------------------------------------}

-- | Merged (external) binding specifications
--
-- While a 'BindingSpec' is specific to a Haskell module, this type supports
-- binding specifications across multiple Haskell modules.  It is a performance
-- optimization for resolving external binding specifications.
newtype MergedBindingSpecs = MergedBindingSpecs {
      mergedBindingSpecs ::
        Map C.QualName [(Set SourcePath, ResolvedBindingSpec)]
    }
  deriving stock (Show)

-- | Merge (external) binding specifications
--
-- It is assumed that all of the passed binding specifications have valid
-- targets.
merge ::
     [ResolvedBindingSpec]
  -> ([BindingSpecMergeMsg], MergedBindingSpecs)
merge =
      bimap mkTypeErrs (MergedBindingSpecs . snd)
    . foldl' mergeSpec (Set.empty, (Map.empty, Map.empty))
  where
    mkTypeErrs :: Set C.QualName -> [BindingSpecMergeMsg]
    mkTypeErrs = fmap BindingSpecMergeConflict . Set.toList

    mergeSpec ::
         ( Set C.QualName
         , ( Map C.QualName (Set HashIncludeArg)
           , Map C.QualName [(Set SourcePath, ResolvedBindingSpec)]
           )
         )
      -> ResolvedBindingSpec
      -> ( Set C.QualName
         , ( Map C.QualName (Set HashIncludeArg)
           , Map C.QualName [(Set SourcePath, ResolvedBindingSpec)]
           )
         )
    mergeSpec ctx spec =
        foldl' (mergeType spec) ctx
      . map (fmap (map fst))
      $ Map.toList (bindingSpecCTypes spec)

    mergeType ::
         ResolvedBindingSpec
      -> ( Set C.QualName
         , ( Map C.QualName (Set HashIncludeArg)
           , Map C.QualName [(Set SourcePath, ResolvedBindingSpec)]
           )
         )
      -> (C.QualName, [Set (HashIncludeArg, SourcePath)])
      -> ( Set C.QualName
         , ( Map C.QualName (Set HashIncludeArg)
           , Map C.QualName [(Set SourcePath, ResolvedBindingSpec)]
           )
         )
    mergeType spec (dupSet, (seenMap, acc)) (cQualName, sourceSets) =
      let seenS = Set.unions $ map (Set.map fst) sourceSets
          keyS  = Set.unions $ map (Set.map snd) sourceSets
          acc'  = Map.insertWith (++) cQualName [(keyS, spec)] acc
      in  case Map.insertLookupWithKey (const (<>)) cQualName seenS seenMap of
            (Nothing, seenMap') -> (dupSet, (seenMap', acc'))
            (Just eS, seenMap')
              | Set.disjoint eS seenS -> (dupSet, (seenMap', acc'))
              | otherwise -> (Set.insert cQualName dupSet, (seenMap', acc'))

-- | Lookup type specs in 'MergedBindingSpecs'
lookupMergedBindingSpecs ::
     C.QualName
  -> Set SourcePath
  -> MergedBindingSpecs
  -> Maybe (Hs.ModuleName, Omittable CTypeSpec, Maybe HsTypeSpec)
lookupMergedBindingSpecs cQualName headers specs = do
    spec <- lookupBy (not . Set.disjoint headers)
      =<< Map.lookup cQualName (mergedBindingSpecs specs)
    (hsModuleName, oCTypeSpec) <- lookupCTypeSpec cQualName headers spec
    let mHsTypeSpec = case oCTypeSpec of
          Require cTypeSpec -> do
            hsIdentifier <- cTypeSpecIdentifier cTypeSpec
            lookupHsTypeSpec hsIdentifier spec
          Omit -> Nothing
    return (hsModuleName, oCTypeSpec, mHsTypeSpec)

{-------------------------------------------------------------------------------
  Auxiliary: Specification files
-------------------------------------------------------------------------------}

data ABindingSpec = ABindingSpec {
      aBindingSpecVersion  :: AVersion
    , aBindingSpecTarget   :: ABindingSpecTarget
    , aBindingSpecHsModule :: Hs.ModuleName
    , aBindingSpecCTypes   :: [AOCTypeSpecMapping]
    , aBindingSpecHsTypes  :: [AHsTypeSpecMapping]
    }
  deriving stock Show

instance Aeson.FromJSON ABindingSpec where
  parseJSON = Aeson.withObject "ABindingSpec" $ \o -> do
    aBindingSpecVersion  <- o .:  "version"
    aBindingSpecTarget   <- o .:  "target"
    aBindingSpecHsModule <- o .:  "hsmodule"
    aBindingSpecCTypes   <- o .:? "ctypes"  .!= []
    aBindingSpecHsTypes  <- o .:? "hstypes" .!= []
    return ABindingSpec{..}

instance Aeson.ToJSON ABindingSpec where
  toJSON ABindingSpec{..} = Aeson.Object . KM.fromList $ catMaybes [
      Just ("version"  .= aBindingSpecVersion)
    , Just ("target"   .= aBindingSpecTarget)
    , Just ("hsmodule" .= aBindingSpecHsModule)
    , ("ctypes"  .=) <$> omitWhenNull aBindingSpecCTypes
    , ("hstypes" .=) <$> omitWhenNull aBindingSpecHsTypes
    ]

--------------------------------------------------------------------------------

newtype ABindingSpecTarget = ABindingSpecTarget {
      unABindingSpecTarget :: BindingSpecTarget
    }
  deriving stock Show

instance Aeson.FromJSON ABindingSpecTarget where
  parseJSON = fmap ABindingSpecTarget . aux
    where
      aux :: Aeson.Value -> Aeson.Parser BindingSpecTarget
      aux = Aeson.withText "ABindingSpecTarget" $ \case
        "any" -> return AnyTarget
        t     -> case ClangArgs.parseTargetTriple (Text.unpack t) of
          Just target -> return $ SpecificTarget target
          Nothing     -> Aeson.parseFail $ "invalid target: " ++ show t

instance Aeson.ToJSON ABindingSpecTarget where
  toJSON = Aeson.String . Text.pack . aux . unABindingSpecTarget
    where
      aux :: BindingSpecTarget -> String
      aux = \case
        AnyTarget             -> "any"
        SpecificTarget target -> ClangArgs.targetTriple target

--------------------------------------------------------------------------------

type AOCTypeSpecMapping = AOmittable AKCTypeSpecMapping ACTypeSpecMapping

data AKCTypeSpecMapping = AKCTypeSpecMapping {
      akCTypeSpecMappingHeaders :: [FilePath]
    , akCTypeSpecMappingCName   :: Text
    }
  deriving stock Show

instance Aeson.FromJSON AKCTypeSpecMapping where
  parseJSON = Aeson.withObject "AKCTypeSpecMapping" $ \o -> do
    akCTypeSpecMappingHeaders <- o .: "headers" >>= listFromJSON
    akCTypeSpecMappingCName   <- o .: "cname"
    return AKCTypeSpecMapping{..}

instance Aeson.ToJSON AKCTypeSpecMapping where
  toJSON AKCTypeSpecMapping{..} = Aeson.Object $ KM.fromList [
      "headers" .= listToJSON akCTypeSpecMappingHeaders
    , "cname"   .= akCTypeSpecMappingCName
    ]

data ACTypeSpecMapping = ACTypeSpecMapping {
      aCTypeSpecMappingHeaders    :: [FilePath]
    , aCTypeSpecMappingCName      :: Text
    , aCTypeSpecMappingIdentifier :: Maybe Hs.Identifier
    }
  deriving stock Show

instance Aeson.FromJSON ACTypeSpecMapping where
  parseJSON = Aeson.withObject "ACTypeSpecMapping" $ \o -> do
    aCTypeSpecMappingHeaders    <- o .:  "headers" >>= listFromJSON
    aCTypeSpecMappingCName      <- o .:  "cname"
    aCTypeSpecMappingIdentifier <- o .:? "hsname"
    return ACTypeSpecMapping{..}

instance Aeson.ToJSON ACTypeSpecMapping where
  toJSON ACTypeSpecMapping{..} = Aeson.Object . KM.fromList $ catMaybes [
      Just ("headers" .= listToJSON aCTypeSpecMappingHeaders)
    , Just ("cname"   .= aCTypeSpecMappingCName)
    , ("hsname"    .=) <$> aCTypeSpecMappingIdentifier
    ]

--------------------------------------------------------------------------------

data AHsTypeSpecMapping = AHsTypeSpecMapping {
      aHsTypeSpecMappingIdentifier :: Hs.Identifier
    , aHsTypeSpecMappingInstances  :: [AOInstanceSpecMapping]
    }
  deriving stock Show

instance Aeson.FromJSON AHsTypeSpecMapping where
  parseJSON = Aeson.withObject "AHsTypeSpecMapping" $ \o -> do
    aHsTypeSpecMappingIdentifier <- o .:  "hsname"
    aHsTypeSpecMappingInstances  <- o .:? "instances" .!= []
    return AHsTypeSpecMapping{..}

instance Aeson.ToJSON AHsTypeSpecMapping where
  toJSON AHsTypeSpecMapping{..} = Aeson.Object . KM.fromList $ catMaybes [
      Just ("hsname" .= aHsTypeSpecMappingIdentifier)
    , ("instances" .=) <$> omitWhenNull aHsTypeSpecMappingInstances
    ]

--------------------------------------------------------------------------------

type AOInstanceSpecMapping = AOmittable Hs.TypeClass AInstanceSpecMapping

data AInstanceSpecMapping = AInstanceSpecMapping {
      aInstanceSpecMappingClass       :: Hs.TypeClass
    , aInstanceSpecMappingStrategy    :: Maybe AStrategySpec
    , aInstanceSpecMappingConstraints :: [AConstraintSpec]
    }
  deriving stock Show

instance Aeson.FromJSON AInstanceSpecMapping where
  parseJSON = \case
    s@Aeson.String{} -> do
      aInstanceSpecMappingClass <- Aeson.parseJSON s
      let aInstanceSpecMappingStrategy    = Nothing
          aInstanceSpecMappingConstraints = []
      return AInstanceSpecMapping{..}
    Aeson.Object o -> do
      aInstanceSpecMappingClass       <- o .:  "class"
      aInstanceSpecMappingStrategy    <- o .:? "strategy"
      aInstanceSpecMappingConstraints <- o .:? "constraints" .!= []
      return AInstanceSpecMapping{..}
    v -> Aeson.parseFail $
      "expected AInstanceSpecMapping String or Object, but encountered "
        ++ typeOf v

instance Aeson.ToJSON AInstanceSpecMapping where
  toJSON AInstanceSpecMapping{..}
    | isNothing aInstanceSpecMappingStrategy
        && null aInstanceSpecMappingConstraints =
          Aeson.toJSON aInstanceSpecMappingClass
    | otherwise = Aeson.Object . KM.fromList $ catMaybes [
          Just ("class" .= aInstanceSpecMappingClass)
        , ("strategy"    .=) <$> aInstanceSpecMappingStrategy
        , ("constraints" .=) <$> omitWhenNull aInstanceSpecMappingConstraints
        ]

--------------------------------------------------------------------------------

newtype AStrategySpec = AStrategySpec {
      unAStrategySpec :: StrategySpec
    }
  deriving stock Show

instance Aeson.FromJSON AStrategySpec where
  parseJSON = fmap AStrategySpec . aux
    where
      aux :: Aeson.Value -> Aeson.Parser StrategySpec
      aux = Aeson.withText "AStrategySpec" $ \t ->
        case Map.lookup t strategySpecFromText of
          Just strategy -> return strategy
          Nothing -> Aeson.parseFail $ "unknown strategy: " ++ Text.unpack t

instance Aeson.ToJSON AStrategySpec where
  toJSON = Aeson.String . strategySpecText . unAStrategySpec

strategySpecText :: StrategySpec -> Text
strategySpecText = \case
    StrategySpecHsBindgen -> "hs-bindgen"
    StrategySpecNewtype   -> "newtype"
    StrategySpecStock     -> "stock"

strategySpecFromText :: Map Text StrategySpec
strategySpecFromText = Map.fromList [
      (strategySpecText strat, strat)
    | strat <- [minBound..]
    ]

--------------------------------------------------------------------------------

newtype AConstraintSpec = AConstraintSpec ConstraintSpec
  deriving stock Show

instance Aeson.FromJSON AConstraintSpec where
  parseJSON = Aeson.withObject "AConstraintSpec" $ \o -> do
    constraintSpecClass <- o .: "class"
    extRefModule        <- o .: "hsmodule"
    extRefIdentifier    <- o .: "hsname"
    let constraintSpecRef = Hs.ExtRef{..}
    return $ AConstraintSpec ConstraintSpec{..}

instance Aeson.ToJSON AConstraintSpec where
  toJSON (AConstraintSpec ConstraintSpec{..}) =
    let Hs.ExtRef{..} = constraintSpecRef
    in  Aeson.object [
            "class"    .= constraintSpecClass
          , "hsmodule" .= extRefModule
          , "hsname"   .= extRefIdentifier
          ]

--------------------------------------------------------------------------------

fromABindingSpec ::
     FilePath
  -> ABindingSpec
  -> ([BindingSpecReadMsg], UnresolvedBindingSpec)
fromABindingSpec path ABindingSpec{..} =
    let bindingSpecTarget = unABindingSpecTarget aBindingSpecTarget
        bindingSpecModule = aBindingSpecHsModule
        (cTypeErrs, hsIds, bindingSpecCTypes) =
          mkCTypeMap path aBindingSpecCTypes
        (hsTypeErrs, bindingSpecHsTypes) =
          mkHsTypeMap path hsIds aBindingSpecHsTypes
    in  (cTypeErrs ++ hsTypeErrs, BindingSpec{..})

mkCTypeMap ::
     FilePath
  -> [AOCTypeSpecMapping]
  -> ( [BindingSpecReadMsg]
     , Set Hs.Identifier
     , Map C.QualName [(Set HashIncludeArg, Omittable CTypeSpec)]
     )
mkCTypeMap path =
    fin . foldr auxInsert (Set.empty, [], Map.empty, Set.empty, Map.empty)
  where
    fin ::
         ( Set Text
         , [HashIncludeArgMsg]
         , Map C.QualName (Set HashIncludeArg)
         , Set Hs.Identifier
         , Map C.QualName [(Set HashIncludeArg, Omittable CTypeSpec)]
         )
      -> ( [BindingSpecReadMsg]
         , Set Hs.Identifier
         , Map C.QualName [(Set HashIncludeArg, Omittable CTypeSpec)]
         )
    fin (invalids, msgs, conflicts, hsIds, cTypeMap) =
      let invalidErrs = BindingSpecReadInvalidCName path <$> Set.toList invalids
          argErrs = BindingSpecReadHashIncludeArg path <$> msgs
          conflictErrs = [
              BindingSpecReadCTypeConflict path cQualName header
            | (cQualName, headers) <- Map.toList conflicts
            , header <- Set.toList headers
            ]
      in  (invalidErrs ++ argErrs ++ conflictErrs, hsIds, cTypeMap)

    auxInsert ::
         AOCTypeSpecMapping
      -> ( Set Text
         , [HashIncludeArgMsg]
         , Map C.QualName (Set HashIncludeArg)
         , Set Hs.Identifier
         , Map C.QualName [(Set HashIncludeArg, Omittable CTypeSpec)]
         )
      -> ( Set Text
         , [HashIncludeArgMsg]
         , Map C.QualName (Set HashIncludeArg)
         , Set Hs.Identifier
         , Map C.QualName [(Set HashIncludeArg, Omittable CTypeSpec)]
         )
    auxInsert aoCTypeMapping (invalids, msgs, conflicts, hsIds, acc) =
      let (cname, headers, mHsId, oCTypeSpec) = case aoCTypeMapping of
            ARequire ACTypeSpecMapping{..} ->
              let cTypeSpec = CTypeSpec {
                      cTypeSpecIdentifier = aCTypeSpecMappingIdentifier
                    }
              in  ( aCTypeSpecMappingCName
                  , aCTypeSpecMappingHeaders
                  , aCTypeSpecMappingIdentifier
                  , Require cTypeSpec
                  )
            AOmit AKCTypeSpecMapping{..} ->
              ( akCTypeSpecMappingCName
              , akCTypeSpecMappingHeaders
              , Nothing
              , Omit
              )
          (msgs', headers') = bimap ((msgs ++) . concat) Set.fromList $
            unzip (map hashIncludeArg headers)
          hsIds' = maybe hsIds (`Set.insert` hsIds) mHsId
      in  case C.parseQualName cname of
            Nothing ->
              (Set.insert cname invalids, msgs', conflicts, hsIds', acc)
            Just cQualName ->
              let newV = [(headers', oCTypeSpec)]
                  x = Map.insertLookupWithKey (const (++)) cQualName newV acc
              in  case x of
                    (Nothing, acc') ->
                      (invalids, msgs', conflicts, hsIds', acc')
                    (Just oldV, acc') ->
                      let conflicts' = auxDup cQualName newV oldV conflicts
                      in  (invalids, msgs', conflicts', hsIds', acc')

    auxDup ::
         C.QualName
      -> [(Set HashIncludeArg, a)]
      -> [(Set HashIncludeArg, a)]
      -> Map C.QualName (Set HashIncludeArg)
      -> Map C.QualName (Set HashIncludeArg)
    auxDup cQualName newV oldV =
      case Set.intersection (mconcat (fst <$> newV)) (mconcat (fst <$> oldV)) of
        commonHeaders
          | Set.null commonHeaders -> id
          | otherwise -> Map.insertWith Set.union cQualName commonHeaders

mkHsTypeMap ::
     FilePath
  -> Set Hs.Identifier
  -> [AHsTypeSpecMapping]
  -> ([BindingSpecReadMsg], Map Hs.Identifier HsTypeSpec)
mkHsTypeMap path hsIds = fin . foldr auxInsert (Set.empty, Map.empty)
  where
    fin ::
         (Set Hs.Identifier, Map Hs.Identifier HsTypeSpec)
      -> ([BindingSpecReadMsg], Map Hs.Identifier HsTypeSpec)
    fin (conflicts, hsTypeMap) =
      let conflictErrs = BindingSpecReadHsTypeConflict path <$>
            Set.toList conflicts
          noRefErrs = BindingSpecReadHsIdentifierNoRef path <$>
            Set.toList (Map.keysSet hsTypeMap `Set.difference` hsIds)
      in  (conflictErrs ++ noRefErrs, hsTypeMap)

    auxInsert ::
         AHsTypeSpecMapping
      -> (Set Hs.Identifier, Map Hs.Identifier HsTypeSpec)
      -> (Set Hs.Identifier, Map Hs.Identifier HsTypeSpec)
    auxInsert AHsTypeSpecMapping{..} (conflicts, acc) =
      let hsId       = aHsTypeSpecMappingIdentifier
          hsTypeSpec = HsTypeSpec {
              hsTypeSpecInstances = mkInstanceMap aHsTypeSpecMappingInstances
            }
      in  case Map.insertLookupWithKey (\_ n _ -> n) hsId hsTypeSpec acc of
            (Nothing, acc') -> (conflicts,                 acc')
            (Just{},  acc') -> (Set.insert hsId conflicts, acc')

-- duplicates ignored, last value retained
mkInstanceMap ::
     [AOInstanceSpecMapping]
  -> Map Hs.TypeClass (Omittable InstanceSpec)
mkInstanceMap xs = Map.fromList . flip map xs $ \case
    ARequire AInstanceSpecMapping{..} ->
      let inst = InstanceSpec {
              instanceSpecStrategy =
                unAStrategySpec <$> aInstanceSpecMappingStrategy
            , instanceSpecConstraints = [
                  constr
                | AConstraintSpec constr <- aInstanceSpecMappingConstraints
                ]
            }
      in  (aInstanceSpecMappingClass, Require inst)
    AOmit hsTypeClass -> (hsTypeClass, Omit)

--------------------------------------------------------------------------------

toABindingSpec :: UnresolvedBindingSpec -> ABindingSpec
toABindingSpec BindingSpec{..} = ABindingSpec {
      aBindingSpecVersion  = mkAVersion version
    , aBindingSpecTarget   = ABindingSpecTarget bindingSpecTarget
    , aBindingSpecHsModule = bindingSpecModule
    , aBindingSpecCTypes   = toAOCTypes bindingSpecCTypes
    , aBindingSpecHsTypes  = toAHsTypes bindingSpecHsTypes
    }

toAOCTypes ::
     Map C.QualName [(Set HashIncludeArg, Omittable CTypeSpec)]
  -> [AOCTypeSpecMapping]
toAOCTypes cTypeMap = [
      case oCTypeSpec of
        Require CTypeSpec{..} -> ARequire ACTypeSpecMapping {
            aCTypeSpecMappingHeaders =
              map getHashIncludeArg (Set.toAscList headers)
          , aCTypeSpecMappingCName = C.qualNameText cQualName
          , aCTypeSpecMappingIdentifier = cTypeSpecIdentifier
          }
        Omit -> AOmit AKCTypeSpecMapping {
            akCTypeSpecMappingHeaders =
              map getHashIncludeArg (Set.toAscList headers)
          , akCTypeSpecMappingCName = C.qualNameText cQualName
          }
    | (cQualName, xs) <- Map.toAscList cTypeMap
    , (headers, oCTypeSpec) <- xs
    ]

toAHsTypes :: Map Hs.Identifier HsTypeSpec -> [AHsTypeSpecMapping]
toAHsTypes hsTypeMap = [
      AHsTypeSpecMapping {
          aHsTypeSpecMappingIdentifier = hsIdentifier
        , aHsTypeSpecMappingInstances  = toAOInstances hsTypeSpecInstances
        }
    | (hsIdentifier, HsTypeSpec{..}) <- Map.toAscList hsTypeMap
    ]

toAOInstances ::
     Map Hs.TypeClass (Omittable InstanceSpec)
  -> [AOInstanceSpecMapping]
toAOInstances instMap = [
      case oInstSpec of
        Require InstanceSpec{..} -> ARequire AInstanceSpecMapping {
            aInstanceSpecMappingClass = hsTypeClass
          , aInstanceSpecMappingStrategy =
              AStrategySpec <$> instanceSpecStrategy
          , aInstanceSpecMappingConstraints =
              map AConstraintSpec instanceSpecConstraints
          }
        Omit -> AOmit hsTypeClass
    | (hsTypeClass, oInstSpec) <- Map.toAscList instMap
    ]

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- 'List.lookup' using a predicate
lookupBy :: (a -> Bool) -> [(a, b)] -> Maybe b
lookupBy p = fmap snd . List.find (p . fst)
