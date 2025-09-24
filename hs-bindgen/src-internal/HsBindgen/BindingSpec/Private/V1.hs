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
  , TypeSpec(..)
  , defaultTypeSpec
    -- ** Instances
  , InstanceSpec(..)
  , StrategySpec(..)
  , ConstraintSpec(..)
    -- * API
  , empty
  , load
  , lookupTypeSpec
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
    -- ** Merging
  , merge
    -- ** Header resolution
  , resolve
  ) where

import Prelude hiding (readFile, writeFile)

import Control.Monad ((<=<))
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
import HsBindgen.Errors
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Language.Haskell
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
newtype BindingSpec header = BindingSpec {
      -- | Type specifications
      --
      -- A C type is identified using a 'C.QualName' and a set of headers that
      -- provide the type.  For a given 'C.QualName', the sets of headers are
      -- disjoint.  The type of this map is therefore equivalent to
      -- @'Map' 'C.QualName' ('Map' header ('Omittable' 'TypeSpec'))@, but this
      -- type is used as an optimization.
      bindingSpecTypes :: Map C.QualName [(Set header, Omittable TypeSpec)]
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

-- | Binding specification for a C type
data TypeSpec = TypeSpec {
      -- | Haskell module
      typeSpecModule :: Maybe HsModuleName

    , -- | Haskell identifier
      typeSpecIdentifier :: Maybe HsIdentifier

    , -- | Instance specification
      typeSpecInstances :: Map HsTypeClass (Omittable InstanceSpec)
    }
  deriving stock (Show, Eq, Generic)

defaultTypeSpec :: TypeSpec
defaultTypeSpec = TypeSpec {
      typeSpecModule     = Nothing
    , typeSpecIdentifier = Nothing
    , typeSpecInstances  = Map.empty
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

instance Aeson.FromJSON StrategySpec where
  parseJSON = Aeson.withText "StrategySpec" $ \t ->
    case Map.lookup t strategySpecFromText of
      Just strategy -> return strategy
      Nothing       -> Aeson.parseFail $ "unknown strategy: " ++ (Text.unpack t)

instance Aeson.ToJSON StrategySpec where
  toJSON = Aeson.String . strategySpecText

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

-- | Constraint of an instance
data ConstraintSpec = ConstraintSpec {
      constraintSpecClass :: HsTypeClass
    , constraintSpecRef   :: ExtHsRef
    }
  deriving stock (Show, Eq, Ord, Generic)

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Empty binding specification
empty :: BindingSpec header
empty = BindingSpec {
      bindingSpecTypes = Map.empty
    }

-- | Load, merge, and resolve binding specifications
--
-- The format is determined by filename extension.
load ::
     Tracer IO BindingSpecMsg
  -> (ResolveHeaderMsg -> BindingSpecResolveMsg)
     -- ^ Are we dealing with external or prescriptive bindings?
  -> ClangArgs
  -> UnresolvedBindingSpec
  -> [FilePath]
  -> IO (UnresolvedBindingSpec, ResolvedBindingSpec)
load tracer injResolveHeader args stdSpec paths = do
    uspecs <- mapM (readFile tracerRead) paths
    let (mergeMsgs, uspec) = merge (stdSpec : uspecs)
    mapM_ (traceWith tracer . BindingSpecMergeMsg) mergeMsgs
    (uspec,) <$> resolve tracerResolve injResolveHeader args uspec
  where
    tracerRead :: Tracer IO BindingSpecReadMsg
    tracerRead = contramap BindingSpecReadMsg tracer

    tracerResolve :: Tracer IO BindingSpecResolveMsg
    tracerResolve = contramap BindingSpecResolveMsg tracer

-- | Lookup the @'Omittable' 'TypeSpec'@ associated with a C type
lookupTypeSpec ::
     C.QualName
  -> Set SourcePath
  -> ResolvedBindingSpec
  -> Maybe (Omittable TypeSpec)
lookupTypeSpec cQualName headers =
        lookupBy (not . Set.disjoint headers . Set.map snd)
    <=< Map.lookup cQualName . bindingSpecTypes
  where
    -- 'List.lookup' using a predicate
    lookupBy :: (a -> Bool) -> [(a, b)] -> Maybe b
    lookupBy p = fmap snd . List.find (p . fst)

{-------------------------------------------------------------------------------
  API: YAML/JSON
-------------------------------------------------------------------------------}

-- | Read a binding specification file
--
-- The format is determined by the filename extension.
readFile ::
     Tracer IO BindingSpecReadMsg
  -> FilePath
  -> IO UnresolvedBindingSpec
readFile = readFileAux readVersion

-- | Read a binding specification JSON file
readFileJson ::
     Tracer IO BindingSpecReadMsg
  -> FilePath
  -> IO UnresolvedBindingSpec
readFileJson = readFileAux readVersionJson

-- | Read a binding specification YAML file
readFileYaml ::
     Tracer IO BindingSpecReadMsg
  -> FilePath
  -> IO UnresolvedBindingSpec
readFileYaml = readFileAux readVersionYaml

readFileAux ::
     ReadVersionFunction
  -> Tracer IO BindingSpecReadMsg
  -> FilePath
  -> IO UnresolvedBindingSpec
readFileAux doRead tracer path = fmap (fromMaybe empty) $
    doRead tracer path >>= \case
      Just (version', value) -> parseValue tracer path version' value
      Nothing                -> return Nothing

parseValue ::
     Monad m
  => Tracer m BindingSpecReadMsg
  -> FilePath
  -> BindingSpecVersion
  -> Aeson.Value
  -> m (Maybe UnresolvedBindingSpec)
parseValue tracer path bsVersion value
    | isCompatBindingSpecVersions bsVersion version = do
        traceWith tracer $ BindingSpecReadParseVersion path bsVersion
        case Aeson.fromJSON value of
          Aeson.Success aspec -> do
            let (errs, spec) = fromABindingSpec path aspec
            mapM_ (traceWith tracer) errs
            return (Just spec)
          Aeson.Error err -> do
            traceWith tracer $ BindingSpecReadAesonError path err
            return Nothing
    -- | bsVersion < version -> -- no lower versions
    | otherwise = do
        traceWith tracer $ BindingSpecReadIncompatibleVersion path bsVersion
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
      "version"     ->  0  -- ABindingSpec:1
      "omit"        ->  1  -- Omittable:1
      "types"       ->  2  -- ABindingSpec:2
      "class"       ->  3  -- AInstanceSpecMapping:1, AConstraintSpec:1
      "headers"     ->  4  -- ATypeSpecMapping:1
      "cname"       ->  5  -- ATypeSpecMapping:2
      "module"      ->  6  -- ATypeSpecMapping:3, AConstraintSpec:2
      "identifier"  ->  7  -- ATypeSpecMapping:4, AConstraintSpec:3
      "instances"   ->  8  -- ATypeSpecMapping:5
      "strategy"    ->  9  -- AInstanceSpecMapping:2
      "constraints" -> 10  -- AInstanceSpecMapping:3
      key -> panicPure $ "Unknown key: " ++ show key

{-------------------------------------------------------------------------------
  API: Merging
-------------------------------------------------------------------------------}

-- | Merge binding specifications
merge ::
     [UnresolvedBindingSpec]
  -> ([BindingSpecMergeMsg], UnresolvedBindingSpec)
merge = \case
    [] -> ([], empty)
    spec:specs ->
      let (typeErrs, bsTypes) =
              first mkTypeErrs
            . foldl' mergeTypes (Set.empty, bindingSpecTypes spec)
            $ concatMap (Map.toList . bindingSpecTypes) specs
          spec' = BindingSpec {
              bindingSpecTypes = bsTypes
            }
      in  (typeErrs, spec')
  where
    mkTypeErrs :: Set C.QualName -> [BindingSpecMergeMsg]
    mkTypeErrs = fmap BindingSpecMergeConflict . Set.toList

    mergeTypes ::
         (Set C.QualName, Map C.QualName [(Set HashIncludeArg, a)])
      -> (C.QualName, [(Set HashIncludeArg, a)])
      -> (Set C.QualName, Map C.QualName [(Set HashIncludeArg, a)])
    mergeTypes (dupSet, acc) (cQualName, rs) =
      case Map.insertLookupWithKey (const (++)) cQualName rs acc of
        (Nothing, acc') -> (dupSet, acc')
        (Just ls, acc')
          | Set.disjoint (Set.unions (fst <$> ls)) (Set.unions (fst <$> rs)) ->
              (dupSet, acc')
          | otherwise -> (Set.insert cQualName dupSet, acc')

{-------------------------------------------------------------------------------
  API: Header resolution
-------------------------------------------------------------------------------}

-- | Resolve headers in a binding specification
resolve ::
     Tracer IO BindingSpecResolveMsg
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

    bindingSpecTypes <- Map.fromList <$>
      mapMaybeM (uncurry resolveTypes) (Map.toList (bindingSpecTypes uSpec))
    return BindingSpec{..}
  where
    allHeaders :: Set HashIncludeArg
    allHeaders = mconcat $ fst <$> concat (Map.elems (bindingSpecTypes uSpec))

{-------------------------------------------------------------------------------
  Auxiliary: Specification files
-------------------------------------------------------------------------------}

data ABindingSpec = ABindingSpec {
      aBindingSpecVersion :: BindingSpecVersion
    , aBindingSpecTypes   :: [AOmittable ATypeSpecMapping]
    }
  deriving stock Show

instance Aeson.FromJSON ABindingSpec where
  parseJSON = Aeson.withObject "ABindingSpec" $ \o -> do
    aBindingSpecVersion <- o .: "version"
    aBindingSpecTypes   <- o .: "types"
    return ABindingSpec{..}

instance Aeson.ToJSON ABindingSpec where
  toJSON ABindingSpec{..} = Aeson.object [
      "version" .= aBindingSpecVersion
    , "types"   .= aBindingSpecTypes
    ]

--------------------------------------------------------------------------------

data ATypeSpecMapping = ATypeSpecMapping {
      aTypeSpecMappingHeaders    :: [FilePath]
    , aTypeSpecMappingCName      :: Text
    , aTypeSpecMappingModule     :: Maybe HsModuleName
    , aTypeSpecMappingIdentifier :: Maybe HsIdentifier
    , aTypeSpecMappingInstances  :: [AOmittable AInstanceSpecMapping]
    }
  deriving stock Show

instance Aeson.FromJSON ATypeSpecMapping where
  parseJSON = Aeson.withObject "ATypeSpecMapping" $ \o -> do
    aTypeSpecMappingHeaders    <- o .:  "headers" >>= listFromJSON
    aTypeSpecMappingCName      <- o .:  "cname"
    aTypeSpecMappingModule     <- o .:? "module"
    aTypeSpecMappingIdentifier <- o .:? "identifier"
    aTypeSpecMappingInstances  <- o .:? "instances" .!= []
    return ATypeSpecMapping{..}

instance Aeson.ToJSON ATypeSpecMapping where
  toJSON ATypeSpecMapping{..} = Aeson.Object . KM.fromList $ catMaybes [
      Just ("headers" .= listToJSON aTypeSpecMappingHeaders)
    , Just ("cname"   .= aTypeSpecMappingCName)
    , ("module"     .=) <$> aTypeSpecMappingModule
    , ("identifier" .=) <$> aTypeSpecMappingIdentifier
    , ("instances"  .=) <$> omitWhenNull aTypeSpecMappingInstances
    ]

--------------------------------------------------------------------------------

data AInstanceSpecMapping = AInstanceSpecMapping {
      aInstanceSpecMappingClass       :: HsTypeClass
    , aInstanceSpecMappingStrategy    :: Maybe StrategySpec
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

newtype AConstraintSpec = AConstraintSpec ConstraintSpec
  deriving stock Show

instance Aeson.FromJSON AConstraintSpec where
  parseJSON = Aeson.withObject "AConstraintSpec" $ \o -> do
    constraintSpecClass    <- o .: "class"
    extHsRefModule         <- o .: "module"
    extHsRefIdentifier     <- o .: "identifier"
    let constraintSpecRef = ExtHsRef{..}
    return $ AConstraintSpec ConstraintSpec{..}

instance Aeson.ToJSON AConstraintSpec where
  toJSON (AConstraintSpec ConstraintSpec{..}) =
    let ExtHsRef{..} = constraintSpecRef
    in  Aeson.object [
            "class"      .= constraintSpecClass
          , "module"     .= extHsRefModule
          , "identifier" .= extHsRefIdentifier
          ]

--------------------------------------------------------------------------------

fromABindingSpec ::
     FilePath
  -> ABindingSpec
  -> ([BindingSpecReadMsg], UnresolvedBindingSpec)
fromABindingSpec path ABindingSpec{..} =
    let (typeErrs, bindingSpecTypes) = mkTypeMap aBindingSpecTypes
    in  (typeErrs, BindingSpec{..})
  where
    mkTypeMap ::
         [AOmittable ATypeSpecMapping]
      -> ( [BindingSpecReadMsg]
         , Map C.QualName [(Set HashIncludeArg, Omittable TypeSpec)]
         )
    mkTypeMap =
        mkTypeMapErrs
      . foldr mkTypeMapInsert (Set.empty, [], Map.empty, Map.empty)

    mkTypeMapErrs ::
         (Set Text, [HashIncludeArgMsg], Map C.QualName (Set HashIncludeArg), a)
      -> ([BindingSpecReadMsg], a)
    mkTypeMapErrs (invalids, msgs, conflicts, x) =
      let invalidErrs = BindingSpecReadInvalidCName path <$> Set.toList invalids
          argErrs = BindingSpecReadHashIncludeArg path <$> msgs
          conflictErrs = [
              BindingSpecReadConflict path cQualName header
            | (cQualName, headers) <- Map.toList conflicts
            , header <- Set.toList headers
            ]
      in  (invalidErrs ++ argErrs ++ conflictErrs, x)

    mkTypeMapInsert ::
         AOmittable ATypeSpecMapping
      -> ( Set Text
         , [HashIncludeArgMsg]
         , Map C.QualName (Set HashIncludeArg)
         , Map C.QualName [(Set HashIncludeArg, Omittable TypeSpec)]
         )
      -> ( Set Text
         , [HashIncludeArgMsg]
         , Map C.QualName (Set HashIncludeArg)
         , Map C.QualName [(Set HashIncludeArg, Omittable TypeSpec)]
         )
    mkTypeMapInsert aoTypeMapping (invalids, msgs, conflicts, acc) =
      let (cname, headers, oTypeSpec) = case aoTypeMapping of
            ARequire ATypeSpecMapping{..} ->
              let typ = TypeSpec {
                      typeSpecModule     = aTypeSpecMappingModule
                    , typeSpecIdentifier = aTypeSpecMappingIdentifier
                    , typeSpecInstances  =
                        mkInstanceMap aTypeSpecMappingInstances
                    }
              in  (aTypeSpecMappingCName, aTypeSpecMappingHeaders, Require typ)
            AOmit ATypeSpecMapping{..} ->
              (aTypeSpecMappingCName, aTypeSpecMappingHeaders, Omit)
          (msgs', headers') = bimap ((msgs ++) . concat) Set.fromList $
            unzip (map hashIncludeArg headers)
      in  case C.parseQualName cname of
            Nothing -> (Set.insert cname invalids, msgs', conflicts, acc)
            Just cQualName ->
              let newV = [(headers', oTypeSpec)]
                  x = Map.insertLookupWithKey (const (++)) cQualName newV acc
              in  case x of
                    (Nothing,   acc') -> (invalids, msgs', conflicts, acc')
                    (Just oldV, acc') ->
                      let conflicts' =
                            mkTypeMapDup cQualName newV oldV conflicts
                      in  (invalids, msgs', conflicts', acc')

    mkTypeMapDup ::
         C.QualName
      -> [(Set HashIncludeArg, a)]
      -> [(Set HashIncludeArg, a)]
      -> Map C.QualName (Set HashIncludeArg)
      -> Map C.QualName (Set HashIncludeArg)
    mkTypeMapDup cQualName newV oldV =
      case Set.intersection (mconcat (fst <$> newV)) (mconcat (fst <$> oldV)) of
        commonHeaders
          | Set.null commonHeaders -> id
          | otherwise -> Map.insertWith Set.union cQualName commonHeaders

    -- duplicates ignored, last value retained
    mkInstanceMap ::
         [AOmittable AInstanceSpecMapping]
      -> Map HsTypeClass (Omittable InstanceSpec)
    mkInstanceMap xs = Map.fromList . flip map xs $ \case
      ARequire AInstanceSpecMapping{..} ->
        let inst = InstanceSpec {
                instanceSpecStrategy    = aInstanceSpecMappingStrategy
              , instanceSpecConstraints = [
                    constr
                  | AConstraintSpec constr <- aInstanceSpecMappingConstraints
                  ]
              }
        in  (aInstanceSpecMappingClass, Require inst)
      AOmit AInstanceSpecMapping{..} -> (aInstanceSpecMappingClass, Omit)

toABindingSpec :: UnresolvedBindingSpec -> ABindingSpec
toABindingSpec BindingSpec{..} = ABindingSpec{..}
  where
    aBindingSpecVersion :: BindingSpecVersion
    aBindingSpecVersion = version

    aBindingSpecTypes :: [AOmittable ATypeSpecMapping]
    aBindingSpecTypes = [
        case oType of
          Require TypeSpec{..} -> ARequire ATypeSpecMapping {
              aTypeSpecMappingHeaders    =
                map getHashIncludeArg (Set.toAscList headers)
            , aTypeSpecMappingCName      = C.qualNameText cQualName
            , aTypeSpecMappingModule     = typeSpecModule
            , aTypeSpecMappingIdentifier = typeSpecIdentifier
            , aTypeSpecMappingInstances  = [
                  case oInst of
                    Require InstanceSpec{..} -> ARequire AInstanceSpecMapping {
                        aInstanceSpecMappingClass       = clss
                      , aInstanceSpecMappingStrategy    = instanceSpecStrategy
                      , aInstanceSpecMappingConstraints =
                          map AConstraintSpec instanceSpecConstraints
                      }
                    Omit -> AOmit AInstanceSpecMapping {
                        aInstanceSpecMappingClass       = clss
                      , aInstanceSpecMappingStrategy    = Nothing
                      , aInstanceSpecMappingConstraints = []
                      }
                | (clss, oInst) <- Map.toAscList typeSpecInstances
                ]
            }
          Omit -> AOmit ATypeSpecMapping {
              aTypeSpecMappingHeaders    =
                map getHashIncludeArg (Set.toAscList headers)
            , aTypeSpecMappingCName      = C.qualNameText cQualName
            , aTypeSpecMappingModule     = Nothing
            , aTypeSpecMappingIdentifier = Nothing
            , aTypeSpecMappingInstances  = []
            }
      | (cQualName, xs) <- Map.toAscList bindingSpecTypes
      , (headers, oType) <- xs
      ]
