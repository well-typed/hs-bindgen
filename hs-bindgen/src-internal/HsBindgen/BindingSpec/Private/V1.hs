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
    currentBindingSpecVersion
    -- * Types
  , BindingSpec(..)
  , UnresolvedBindingSpec
  , ResolvedBindingSpec
  , BindingSpecTarget(..)
  , CTypeSpec(..)
  , CTypeRep(..)
  , HsTypeSpec(..)
  , HsTypeRep(..)
  , HsRecordRep(..)
  , HsNewtypeRep(..)
  , FFIType(..)
    -- ** Instances
  , InstanceSpec(..)
  , StrategySpec(..)
  , ConstraintSpec(..)
    -- * API
  , empty
  , isAnyTarget
  , isCompatTarget
  , getCTypes
  , lookupCTypeSpec
  , lookupHsTypeSpec
    -- ** YAML/JSON
  , readFile
  , parseValue
  , encode
  , defCompareCDeclId
    -- ** Header resolution
  , resolve
    -- ** Merging
  , MergedBindingSpecs
  , merge
  , lookupMergedBindingSpecs
  ) where

import Prelude hiding (readFile)

import Data.Aeson ((.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Function (on)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Ord qualified as Ord
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Yaml.Pretty qualified

import Clang.Args
import Clang.Paths

import HsBindgen.Runtime.BaseForeignType qualified as BFT

import HsBindgen.BindingSpec.Private.Common
import HsBindgen.BindingSpec.Private.Version
import HsBindgen.Config.ClangArgs qualified as ClangArgs
import HsBindgen.Errors
import HsBindgen.Frontend.Naming
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
currentBindingSpecVersion :: BindingSpecVersion
currentBindingSpecVersion = $$(constBindingSpecVersion 1 0)

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
      target :: BindingSpecTarget

      -- | Binding specification module
      --
      -- Each binding specification is specific to a Haskell module.
    , moduleName :: Hs.ModuleName

      -- | C type specifications
      --
      -- A C type is identified using a 'DeclId' and a set of headers that
      -- provide the type.  For a given 'DeclId', the sets of headers are
      -- disjoint.  The type of this map is therefore equivalent to
      -- @'Map' 'DeclId' ('Map' header ('Omittable' 'CTypeSpec'))@, but this
      -- type is used as an optimization.
    , cTypes :: Map DeclId [(Set header, Omittable CTypeSpec)]

      -- | Haskell type specifications
    , hsTypes :: Map Hs.Identifier HsTypeSpec
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
data CTypeSpec = CTypeSpec {
      -- | Haskell identifier
      hsIdent :: Maybe Hs.Identifier

       -- | C type representation
    , cRep :: Maybe CTypeRep
    }
  deriving stock (Show, Eq, Ord, Generic)

instance Default CTypeSpec where
  def = CTypeSpec {
      hsIdent = Nothing
    , cRep    = Nothing
    }

--------------------------------------------------------------------------------

-- | C type representation
data CTypeRep =
    -- | Default representation
    --
    -- The C declaration corresponds to both a type and a constructor.
    CTypeRepDefault

  | -- | Opaque representation
    --
    -- The C declaration corresponds to a type only, so it may only be used via
    -- a reference.
    CTypeRepOpaque

  | -- | Alias representation
    --
    -- The C type should be considered an alias of a different C type.
    CTypeRepAlias
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

--------------------------------------------------------------------------------

-- | Binding specification for a Haskell type
data HsTypeSpec = HsTypeSpec {
      -- | Haskell type representation
      hsRep :: Maybe HsTypeRep

       -- | Instance specification
    , instances :: Map Hs.TypeClass (Omittable InstanceSpec)
    }
  deriving stock (Show, Eq, Ord, Generic)

instance Default HsTypeSpec where
  def = HsTypeSpec {
      hsRep     = Nothing
    , instances = Map.empty
    }

--------------------------------------------------------------------------------

-- | Haskell type representation
data HsTypeRep =
    -- | Record representation
    --
    -- A type and constructor is generated using @data@.
    HsTypeRepRecord HsRecordRep

  | -- | Newtype representation
    --
    -- A type and constructor is generated using @newtype@.
    HsTypeRepNewtype HsNewtypeRep

  | -- | Opaque representation
    --
    -- A type but no constructor is generated using @data@.
    HsTypeRepOpaque

  | -- | Alias representation
    --
    -- A type is generated using @type@.
    HsTypeRepAlias
  deriving stock (Show, Eq, Ord, Generic)

-- | Haskell record representation
data HsRecordRep = HsRecordRep {
      constructor :: Maybe Hs.Identifier   -- ^ Constructor name
    , fields      :: Maybe [Hs.Identifier] -- ^ Field names
    }
  deriving stock (Show, Eq, Ord, Generic)

instance Default HsRecordRep where
  def = HsRecordRep {
        constructor = Nothing
      , fields      = Nothing
      }

-- | Haskell newtype representation
data HsNewtypeRep = HsNewtypeRep {
      -- | Constructor name
      constructor :: Maybe Hs.Identifier

      -- | Field name
    , field :: Maybe Hs.Identifier

      -- | The underlying FFI type.
      --
      -- If @hs-bindgen@ generates a foreign import declaration for a newtype,
      -- then it uses the underlying FFI type rather than the newtype itself.
      -- This is to prevent issues with out-of-scope newtype constructors (see
      -- issue #1282). In such cases, @hs-bindgen@ also generate a wrapper
      -- function around the foreign impot declaration that unwraps and rewraps
      -- newtypes.
      --
      -- If we have these newtypes:
      --
      -- > newtype A = A CChar
      -- > newtype B = B A
      --
      -- Then the underlying FFI type for both @A@ and @B@ is @CChar@.
      --
      -- If the 'hsNewtypeRepFFIType' field is 'Nothing', then it is assumed
      -- that the underlying type is not an FFI type. For example:
      --
      -- > data A = A CChar
      -- > newtype B = B A
      --
      -- Here @B@ does not have an underlying FFI type, and neither does @A@
      -- because it is not even a newtype.
    , ffiType :: Maybe FFIType
    }
  deriving stock (Show, Eq, Ord, Generic)

instance Default HsNewtypeRep where
  def = HsNewtypeRep {
        constructor = Nothing
      , field       = Nothing
      , ffiType     = Nothing
      }

{-------------------------------------------------------------------------------
  FFI type
-------------------------------------------------------------------------------}

data FFIType =
    -- | A basic foreign type.
    --
    -- For example:
    --
    -- > Int
    -- > Ptr Void
    Basic BFT.BasicForeignType
    -- | A builtin foreign type, i.e., a newtype around a basic foreign type
    -- that we support directly.
    --
    -- For example:
    --
    -- > CInt
    -- > ConstPtr Void
  | Builtin BFT.BuiltinForeignType
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Types: Instances
-------------------------------------------------------------------------------}

-- | Instance specification
data InstanceSpec = InstanceSpec {
      -- | Strategy used to generate/derive the instance
      --
      -- A 'Nothing' value indicates that @hs-bindgen@ defaults should be used.
      strategy :: Maybe StrategySpec

      -- | Instance constraints
      --
      -- If specified, /all/ constraints must be listed.
    , constraints :: [ConstraintSpec]
    }
  deriving stock (Show, Eq, Ord, Generic)

instance Default InstanceSpec where
  def = InstanceSpec {
      strategy    = Nothing
    , constraints = []
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
      clss :: Hs.TypeClass
    , ref  :: Hs.ExtRef
    }
  deriving stock (Show, Eq, Ord, Generic)

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Construct an empty binding specification for the given target and module
empty :: ClangArgs.Target -> Hs.ModuleName -> BindingSpec header
empty target hsModuleName = BindingSpec {
      target     = SpecificTarget target
    , moduleName = hsModuleName
    , cTypes     = Map.empty
    , hsTypes    = Map.empty
    }

-- | Predicate that checks if a binding specification target is 'AnyTarget'
isAnyTarget :: BindingSpec header -> Bool
isAnyTarget spec = spec.target == AnyTarget

-- | Predicate that checks if a binding specification is compatible with a
-- specific target
isCompatTarget :: BindingSpec header -> ClangArgs.Target -> Bool
isCompatTarget spec = isCompatBindingSpecTarget spec.target

-- | Get the C types in a binding specification
getCTypes :: ResolvedBindingSpec -> Map DeclId [Set SourcePath]
getCTypes spec = map (Set.map snd . fst) <$> spec.cTypes

-- | Lookup a C type in a 'ResolvedBindingSpec'
lookupCTypeSpec ::
     DeclId
  -> Set SourcePath
  -> ResolvedBindingSpec
  -> Maybe (Hs.ModuleName, Omittable CTypeSpec)
lookupCTypeSpec cDeclId headers spec = do
    ps <- Map.lookup cDeclId spec.cTypes
    oCTypeSpec <- lookupBy (not . Set.disjoint headers . Set.map snd) ps
    return (spec.moduleName, oCTypeSpec)

-- | Lookup a Haskell type in a 'ResolvedBindingSpec'
lookupHsTypeSpec :: Hs.Identifier -> ResolvedBindingSpec -> Maybe HsTypeSpec
lookupHsTypeSpec hsIdentifier spec = Map.lookup hsIdentifier spec.hsTypes

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
readFile tracer cmpt path = readVersion tracer path >>= \case
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
parseValue tracer cmpt path aVersion value
    | isCompatBindingSpecVersions cmpt aVersion.bindingSpec currentBindingSpecVersion = do
        traceWith tracer $ BindingSpecReadParseVersion path aVersion
        case Aeson.fromJSON value of
          Aeson.Success aspec -> do
            let (errs, spec) = fromABindingSpec path aspec
            mapM_ (traceWith tracer) errs
            return (Just spec)
          Aeson.Error err -> do
            traceWith tracer $ BindingSpecReadAesonError path err
            return Nothing
    | otherwise = do
        traceWith tracer $ BindingSpecReadIncompatibleVersion path aVersion
        return Nothing

-- | Encode a binding specification
encode ::
     (DeclId -> DeclId -> Ordering)
  -> Format
  -> UnresolvedBindingSpec
  -> ByteString
encode compareCDeclId = \case
    FormatJSON -> encodeJson' . toABindingSpec compareCDeclId
    FormatYAML -> encodeYaml' . toABindingSpec compareCDeclId

-- | Default ordering of @ctypes@
defCompareCDeclId :: DeclId -> DeclId -> Ordering
defCompareCDeclId = Ord.comparing renderDeclId

encodeJson' :: ABindingSpec -> ByteString
encodeJson' = BSL.toStrict . Aeson.encode

encodeYaml' :: ABindingSpec -> ByteString
encodeYaml' = Data.Yaml.Pretty.encodePretty yamlConfig
  where
    yamlConfig :: Data.Yaml.Pretty.Config
    yamlConfig =
          Data.Yaml.Pretty.setConfCompare (compare `on` keyPosition)
        $ Data.Yaml.Pretty.defConfig

    keyPosition :: Text -> Int
    keyPosition = \case
      -- ABindingSpec:1
      "version"               ->  0
      -- AVersion:1
      "hs_bindgen"            ->  1
      -- AVersion:2
      "binding_specification" ->  2
      -- AOmittable:1
      "omit"                  ->  3
      -- AInstanceSpecMapping:1, AConstraintSpec:1
      "class"                 ->  4
      -- AInstanceSpecMapping:2
      "strategy"              ->  5
      -- AInstanceSpecMapping:3
      "constraints"           ->  6
      -- ABindingSpec:2
      "target"                ->  7
      -- ABindingSpec:3, AConstraintSpec:2
      "hsmodule"              ->  8
      -- ABindingSpec:4
      "ctypes"                ->  9
      -- ABindingSpec:5
      "hstypes"               -> 10
      -- ACTypeSpecMapping:1
      "headers"               -> 11
      -- ACTypeSpecMapping:2
      "cname"                 -> 12
      -- ACTypeSpecMapping:3, AHsTypeSpecMapping:1, AConstraintSpec:3
      "hsname"                -> 13
      -- ACTypeSpecMapping:4, AHsTypeSpecMapping:2
      "representation"        -> 14
      -- AHsTypeSpecMapping:3
      "instances"             -> 15
      -- AHsTypeRep:1
      "record"                -> 16
      -- AHsTypeRep:2
      "newtype"               -> 17
      -- HsRecordRep:1, HsNewtypeRep:1
      "constructor"           -> 18
      -- HsRecordRep:2, HsNewtypeRep:2
      "fields"                -> 19
      -- HsNewtypeRep:3
      "ffitype"               -> 20
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
             DeclId
          -> (Set HashIncludeArg, a)
          -> IO (Maybe (Set (HashIncludeArg, SourcePath), a))
        resolveType cDeclId (uHeaders, x) = case resolveSet uHeaders of
          Just rHeaders -> return $ Just (rHeaders, x)
          Nothing       -> do
            traceWith tracer $ BindingSpecResolveTypeDropped cDeclId
            return Nothing

        resolveTypes ::
             DeclId
          -> [(Set HashIncludeArg, a)]
          -> IO
               ( Maybe
                   (DeclId, [(Set (HashIncludeArg, SourcePath), a)])
               )
        resolveTypes cDeclId uKVs =
          mapMaybeM (resolveType cDeclId) uKVs >>= \case
            rKVs
              | null rKVs -> return Nothing
              | otherwise -> return $ Just (cDeclId, rKVs)

    cTypes <- Map.fromList <$>
      mapMaybeM (uncurry resolveTypes) (Map.toList uSpec.cTypes)
    return BindingSpec{
        target     = uSpec.target
      , moduleName = uSpec.moduleName
      , cTypes     = cTypes
      , hsTypes    = uSpec.hsTypes
      }
  where
    allHeaders :: Set HashIncludeArg
    allHeaders = mconcat $ fst <$> concat (Map.elems uSpec.cTypes)

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
        Map DeclId [(Set SourcePath, ResolvedBindingSpec)]
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
    mkTypeErrs :: Set DeclId -> [BindingSpecMergeMsg]
    mkTypeErrs = fmap BindingSpecMergeConflict . Set.toList

    mergeSpec ::
         ( Set DeclId
         , ( Map DeclId (Set HashIncludeArg)
           , Map DeclId [(Set SourcePath, ResolvedBindingSpec)]
           )
         )
      -> ResolvedBindingSpec
      -> ( Set DeclId
         , ( Map DeclId (Set HashIncludeArg)
           , Map DeclId [(Set SourcePath, ResolvedBindingSpec)]
           )
         )
    mergeSpec ctx spec =
        foldl' (mergeType spec) ctx
      . map (fmap (map fst))
      $ Map.toList spec.cTypes

    mergeType ::
         ResolvedBindingSpec
      -> ( Set DeclId
         , ( Map DeclId (Set HashIncludeArg)
           , Map DeclId [(Set SourcePath, ResolvedBindingSpec)]
           )
         )
      -> (DeclId, [Set (HashIncludeArg, SourcePath)])
      -> ( Set DeclId
         , ( Map DeclId (Set HashIncludeArg)
           , Map DeclId [(Set SourcePath, ResolvedBindingSpec)]
           )
         )
    mergeType spec (dupSet, (seenMap, acc)) (cDeclId, sourceSets) =
      let seenS = Set.unions $ map (Set.map fst) sourceSets
          keyS  = Set.unions $ map (Set.map snd) sourceSets
          acc'  = Map.insertWith (++) cDeclId [(keyS, spec)] acc
      in  case Map.insertLookupWithKey (const (<>)) cDeclId seenS seenMap of
            (Nothing, seenMap') -> (dupSet, (seenMap', acc'))
            (Just eS, seenMap')
              | Set.disjoint eS seenS -> (dupSet, (seenMap', acc'))
              | otherwise -> (Set.insert cDeclId dupSet, (seenMap', acc'))

-- | Lookup type specs in 'MergedBindingSpecs'
lookupMergedBindingSpecs ::
     DeclId
  -> Set SourcePath
  -> MergedBindingSpecs
  -> Maybe (Hs.ModuleName, Omittable CTypeSpec, Maybe HsTypeSpec)
lookupMergedBindingSpecs cDeclId headers specs = do
    spec <- lookupBy (not . Set.disjoint headers)
      =<< Map.lookup cDeclId (mergedBindingSpecs specs)
    (hsModuleName, oCTypeSpec) <- lookupCTypeSpec cDeclId headers spec
    let mHsTypeSpec = case oCTypeSpec of
          Require cTypeSpec -> do
            hsIdentifier <- cTypeSpec.hsIdent
            lookupHsTypeSpec hsIdentifier spec
          Omit -> Nothing
    return (hsModuleName, oCTypeSpec, mHsTypeSpec)

{-------------------------------------------------------------------------------
  Auxiliary: Specification files
-------------------------------------------------------------------------------}

data ABindingSpec = ABindingSpec {
      version  :: AVersion
    , target   :: ABindingSpecTarget
    , hsModule :: Hs.ModuleName
    , cTypes   :: [AOCTypeSpecMapping]
    , hsTypes  :: [AHsTypeSpecMapping]
    }
  deriving stock Show

instance Aeson.FromJSON ABindingSpec where
  parseJSON = Aeson.withObject "ABindingSpec" $ \o -> do
    aBindingSpecVersion  <- o .:  "version"
    aBindingSpecTarget   <- o .:  "target"
    aBindingSpecHsModule <- o .:  "hsmodule"
    aBindingSpecCTypes   <- o .:? "ctypes"  .!= []
    aBindingSpecHsTypes  <- o .:? "hstypes" .!= []
    return ABindingSpec{
        version  = aBindingSpecVersion
      , target   = aBindingSpecTarget
      , hsModule = aBindingSpecHsModule
      , cTypes   = aBindingSpecCTypes
      , hsTypes  = aBindingSpecHsTypes
      }

instance Aeson.ToJSON ABindingSpec where
  toJSON spec = Aeson.Object . KM.fromList $ catMaybes [
      Just ("version"  .= spec.version)
    , Just ("target"   .= spec.target)
    , Just ("hsmodule" .= spec.hsModule)
    , ("ctypes"  .=) <$> omitWhenNull spec.cTypes
    , ("hstypes" .=) <$> omitWhenNull spec.hsTypes
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
      headers :: [FilePath]
    , cName   :: Text
    }
  deriving stock Show

instance Aeson.FromJSON AKCTypeSpecMapping where
  parseJSON = Aeson.withObject "AKCTypeSpecMapping" $ \o -> do
    akCTypeSpecMappingHeaders <- o .: "headers" >>= listFromJSON
    akCTypeSpecMappingCName   <- o .: "cname"
    return AKCTypeSpecMapping{
        headers = akCTypeSpecMappingHeaders
      , cName   = akCTypeSpecMappingCName
      }

instance Aeson.ToJSON AKCTypeSpecMapping where
  toJSON mapping = Aeson.Object $ KM.fromList [
      "headers" .= listToJSON mapping.headers
    , "cname"   .= mapping.cName
    ]

data ACTypeSpecMapping = ACTypeSpecMapping {
      headers    :: [FilePath]
    , cName      :: Text
    , identifier :: Maybe Hs.Identifier
    , rep        :: Maybe ACTypeRep
    }
  deriving stock Show

instance Aeson.FromJSON ACTypeSpecMapping where
  parseJSON = Aeson.withObject "ACTypeSpecMapping" $ \o -> do
    aCTypeSpecMappingHeaders    <- o .:  "headers" >>= listFromJSON
    aCTypeSpecMappingCName      <- o .:  "cname"
    aCTypeSpecMappingIdentifier <- o .:? "hsname"
    aCTypeSpecMappingRep        <- o .:? "representation"
    return ACTypeSpecMapping{
        headers    = aCTypeSpecMappingHeaders
      , cName      = aCTypeSpecMappingCName
      , identifier = aCTypeSpecMappingIdentifier
      , rep        = aCTypeSpecMappingRep
      }

instance Aeson.ToJSON ACTypeSpecMapping where
  toJSON mapping = Aeson.Object . KM.fromList $ catMaybes [
      Just ("headers" .= listToJSON mapping.headers)
    , Just ("cname"   .= mapping.cName)
    , ("hsname"         .=) <$> mapping.identifier
    , ("representation" .=) <$> mapping.rep
    ]

--------------------------------------------------------------------------------

newtype ACTypeRep = ACTypeRep {
      unACTypeRep :: CTypeRep
    }
  deriving stock Show

instance Aeson.FromJSON ACTypeRep where
  parseJSON = fmap ACTypeRep . aux
    where
      aux :: Aeson.Value -> Aeson.Parser CTypeRep
      aux = Aeson.withText "ACTypeRep" $ \t ->
        case Map.lookup t cTypeRepFromText of
          Just cTypeRep -> return cTypeRep
          Nothing ->
            Aeson.parseFail $ "unknown C representation: " ++ Text.unpack t

instance Aeson.ToJSON ACTypeRep where
  toJSON = Aeson.String . cTypeRepText . unACTypeRep

cTypeRepText :: CTypeRep -> Text
cTypeRepText = \case
    CTypeRepDefault -> "default"
    CTypeRepOpaque  -> "opaque"
    CTypeRepAlias   -> "alias"

cTypeRepFromText :: Map Text CTypeRep
cTypeRepFromText = Map.fromList [
      (cTypeRepText cTypeRep, cTypeRep)
    | cTypeRep <- [minBound..]
    ]

--------------------------------------------------------------------------------

data AHsTypeSpecMapping = AHsTypeSpecMapping {
      identifier :: Hs.Identifier
    , rep        :: Maybe AHsTypeRep
    , instances  :: [AOInstanceSpecMapping]
    }
  deriving stock Show

instance Aeson.FromJSON AHsTypeSpecMapping where
  parseJSON = Aeson.withObject "AHsTypeSpecMapping" $ \o -> do
    aHsTypeSpecMappingIdentifier <- o .:  "hsname"
    aHsTypeSpecMappingRep        <- o .:? "representation"
    aHsTypeSpecMappingInstances  <- o .:? "instances" .!= []
    return AHsTypeSpecMapping{
        identifier = aHsTypeSpecMappingIdentifier
      , rep        = aHsTypeSpecMappingRep
      , instances  = aHsTypeSpecMappingInstances
      }

instance Aeson.ToJSON AHsTypeSpecMapping where
  toJSON mapping = Aeson.Object . KM.fromList $ catMaybes [
      Just ("hsname" .= mapping.identifier)
    , ("representation" .=) <$> mapping.rep
    , ("instances" .=) <$> omitWhenNull mapping.instances
    ]

--------------------------------------------------------------------------------

newtype AHsTypeRep = AHsTypeRep {
      unAHsTypeRep :: HsTypeRep
    }
  deriving stock Show

instance Aeson.FromJSON AHsTypeRep where
  parseJSON = fmap AHsTypeRep . parseHsTypeRep
    where
      parseHsTypeRep :: Aeson.Value -> Aeson.Parser HsTypeRep
      parseHsTypeRep = \case
        Aeson.Object o | KM.size o == 1 && KM.member "record" o ->
          HsTypeRepRecord
            <$> Aeson.explicitParseField parseHsRecordRep o "record"
        Aeson.Object o | KM.size o == 1 && KM.member "newtype" o ->
          HsTypeRepNewtype
            <$> Aeson.explicitParseField parseHsNewtypeRep o "newtype"
        v -> Aeson.withText "AHsTypeRep" parseHsTypeRepText v

      parseHsTypeRepText :: Text -> Aeson.Parser HsTypeRep
      parseHsTypeRepText t = case t of
        "record"  -> return (HsTypeRepRecord def)
        "newtype" -> return (HsTypeRepNewtype def)
        "opaque"  -> return HsTypeRepOpaque
        "alias"   -> return HsTypeRepAlias
        _         ->
          Aeson.parseFail $ "unknown Haskell representation: " ++ Text.unpack t

      parseHsRecordRep :: Aeson.Value -> Aeson.Parser HsRecordRep
      parseHsRecordRep = Aeson.withObject "HsRecordRep" $ \o -> do
        hsRecordRepConstructor <- o .:? "constructor"
        hsRecordRepFields      <- o .:? "fields"
        return HsRecordRep{
            constructor = hsRecordRepConstructor
          , fields      = hsRecordRepFields
          }

      parseHsNewtypeRep :: Aeson.Value -> Aeson.Parser HsNewtypeRep
      parseHsNewtypeRep = Aeson.withObject "HsNewtypeRep" $ \o -> do
        hsNewtypeRepConstructor <- o .:? "constructor"
        fields <- o .:? "fields"
        hsNewtypeRepField <- case fields of
          Nothing          -> return Nothing
          Just [fieldName] -> return (Just fieldName)
          Just []          ->
            Aeson.parseFail "newtype representation with no fields"
          Just{}           ->
            Aeson.parseFail "newtype representation with more than one field"
        hsNewtypeRepFFIType <- fmap unAFFIType <$> (o .:? "ffitype")
        return HsNewtypeRep{
            constructor = hsNewtypeRepConstructor
          , field       = hsNewtypeRepField
          , ffiType     = hsNewtypeRepFFIType
          }

instance Aeson.ToJSON AHsTypeRep where
  toJSON = aux . unAHsTypeRep
    where
      aux :: HsTypeRep -> Aeson.Value
      aux = \case
        HsTypeRepRecord x
          | x == def  -> Aeson.String "record"
          | otherwise ->
                Aeson.Object . KM.singleton "record"
              . Aeson.Object . KM.fromList
              $ catMaybes [
                    ("constructor" .=) <$> x.constructor
                  , ("fields"      .=) <$> x.fields
                  ]
        HsTypeRepNewtype x
          | x == def  -> Aeson.String "newtype"
          | otherwise ->
                Aeson.Object . KM.singleton "newtype"
              . Aeson.Object . KM.fromList
              $ catMaybes [
                    ("constructor" .=) <$> x.constructor
                  , ("fields"      .=) <$> fmap (: []) x.field
                  , ("ffitype"     .=) <$> (AFFIType <$> x.ffiType)
                  ]
        HsTypeRepOpaque -> Aeson.String "opaque"
        HsTypeRepAlias  -> Aeson.String "alias"

--------------------------------------------------------------------------------

newtype AFFIType = AFFIType {
      unAFFIType :: FFIType
    }
  deriving stock Show

instance Aeson.FromJSON AFFIType where
  parseJSON = fmap AFFIType . aux
    where
      aux :: Aeson.Value -> Aeson.Parser FFIType
      aux = Aeson.withText "AFFIType" $ \t ->
        case Map.lookup t ffiTypeFromText of
          Just ffitype -> return ffitype
          Nothing -> Aeson.parseFail $ "unknown ffitype: " ++ Text.unpack t

instance Aeson.ToJSON AFFIType where
  toJSON = Aeson.String . ffiTypeText . unAFFIType

ffiTypeText :: FFIType -> Text
ffiTypeText = \case
    Basic basic  -> basicForeignTypeText basic
    Builtin builtin -> builtinForeignTypeText builtin

basicForeignTypeText :: BFT.BasicForeignType -> Text
basicForeignTypeText = \case
    BFT.Char -> "Char"
    BFT.Int -> "Int"
    BFT.Double -> "Double"
    BFT.Float -> "Float"
    BFT.Bool -> "Bool"
    BFT.Int8 -> "Int8"
    BFT.Int16 -> "Int16"
    BFT.Int32 -> "Int32"
    BFT.Int64 -> "Int64"
    BFT.Word -> "Word"
    BFT.Word8 -> "Word8"
    BFT.Word16 -> "Word16"
    BFT.Word32 -> "Word32"
    BFT.Word64 -> "Word64"
    BFT.Ptr -> "Ptr"
    BFT.FunPtr -> "FunPtr"
    BFT.StablePtr -> "StablePtr"

builtinForeignTypeText :: BFT.BuiltinForeignType -> Text
builtinForeignTypeText = \case
    BFT.IntPtr -> "IntPtr"
    BFT.WordPtr -> "WordPtr"
    BFT.ConstPtr -> "ConstPtr"
    BFT.CChar -> "CChar"
    BFT.CSChar -> "CSChar"
    BFT.CUChar -> "CUChar"
    BFT.CShort -> "CShort"
    BFT.CUShort -> "CUShort"
    BFT.CInt -> "CInt"
    BFT.CUInt -> "CUInt"
    BFT.CLong -> "CLong"
    BFT.CULong -> "CULong"
    BFT.CPtrdiff -> "CPtrdiff"
    BFT.CSize -> "CSize"
    BFT.CWchar -> "CWchar"
    BFT.CSigAtomic -> "CSigAtomic"
    BFT.CLLong -> "CLLong"
    BFT.CULLong -> "CULLong"
    BFT.CBool -> "CBool"
    BFT.CIntPtr -> "CIntPtr"
    BFT.CUIntPtr -> "CUIntPtr"
    BFT.CIntMax -> "CIntMax"
    BFT.CUIntMax -> "CUIntMax"
    BFT.CClock -> "CClock"
    BFT.CTime -> "CTime"
    BFT.CUSeconds -> "CUSeconds"
    BFT.CSUSeconds -> "CSUSeconds"
    BFT.CFloat -> "CFloat"
    BFT.CDouble -> "CDouble"

ffiTypeFromText :: Map Text FFIType
ffiTypeFromText =
       (Basic `Map.map` basicForeignTypeFromText)
    <> (Builtin `Map.map` builtinForeignTypeFromText)

basicForeignTypeFromText :: Map Text BFT.BasicForeignType
basicForeignTypeFromText = Map.fromList [
      (basicForeignTypeText basic, basic)
    | basic <- [minBound..]
    ]

builtinForeignTypeFromText :: Map Text BFT.BuiltinForeignType
builtinForeignTypeFromText = Map.fromList [
      (builtinForeignTypeText builtin, builtin)
    | builtin <- [minBound..]
    ]

--------------------------------------------------------------------------------

type AOInstanceSpecMapping = AOmittable Hs.TypeClass AInstanceSpecMapping

data AInstanceSpecMapping = AInstanceSpecMapping {
      clss        :: Hs.TypeClass
    , strategy    :: Maybe AStrategySpec
    , constraints :: [AConstraintSpec]
    }
  deriving stock Show

instance Aeson.FromJSON AInstanceSpecMapping where
  parseJSON = \case
    s@Aeson.String{} -> do
      aInstanceSpecMappingClass <- Aeson.parseJSON s
      let aInstanceSpecMappingStrategy    = Nothing
          aInstanceSpecMappingConstraints = []
      return AInstanceSpecMapping{
          clss        = aInstanceSpecMappingClass
        , strategy    = aInstanceSpecMappingStrategy
        , constraints = aInstanceSpecMappingConstraints
        }
    Aeson.Object o -> do
      aInstanceSpecMappingClass       <- o .:  "class"
      aInstanceSpecMappingStrategy    <- o .:? "strategy"
      aInstanceSpecMappingConstraints <- o .:? "constraints" .!= []
      return AInstanceSpecMapping{
          clss        = aInstanceSpecMappingClass
        , strategy    = aInstanceSpecMappingStrategy
        , constraints = aInstanceSpecMappingConstraints
        }
    v -> Aeson.parseFail $
      "expected AInstanceSpecMapping String or Object, but encountered "
        ++ typeOf v

instance Aeson.ToJSON AInstanceSpecMapping where
  toJSON mapping
    | isNothing mapping.strategy
        && null mapping.constraints =
          Aeson.toJSON mapping.clss
    | otherwise = Aeson.Object . KM.fromList $ catMaybes [
          Just ("class" .= mapping.clss)
        , ("strategy"    .=) <$> mapping.strategy
        , ("constraints" .=) <$> omitWhenNull mapping.constraints
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
      let constraintSpecRef = Hs.ExtRef{
              moduleName = extRefModule
            , ident      = extRefIdentifier
            }
      return $ AConstraintSpec ConstraintSpec{
          clss = constraintSpecClass
        , ref  = constraintSpecRef
        }

instance Aeson.ToJSON AConstraintSpec where
  toJSON (AConstraintSpec spec) = Aeson.object [
        "class"    .= spec.clss
      , "hsmodule" .= spec.ref.moduleName
      , "hsname"   .= spec.ref.ident
      ]

--------------------------------------------------------------------------------

fromABindingSpec ::
     FilePath
  -> ABindingSpec
  -> ([BindingSpecReadMsg], UnresolvedBindingSpec)
fromABindingSpec path spec =
    let bindingSpecTarget = unABindingSpecTarget spec.target
        bindingSpecModule = spec.hsModule
        (cTypeErrs, hsIds, bindingSpecCTypes) =
          mkCTypeMap path spec.cTypes
        (hsTypeErrs, bindingSpecHsTypes) =
          mkHsTypeMap path hsIds spec.hsTypes
    in  (cTypeErrs ++ hsTypeErrs, BindingSpec{
        target     = bindingSpecTarget
      , moduleName = bindingSpecModule
      , cTypes     = bindingSpecCTypes
      , hsTypes    = bindingSpecHsTypes
      })

mkCTypeMap ::
     FilePath
  -> [AOCTypeSpecMapping]
  -> ( [BindingSpecReadMsg]
     , Set Hs.Identifier
     , Map DeclId [(Set HashIncludeArg, Omittable CTypeSpec)]
     )
mkCTypeMap path =
    fin . foldr auxInsert (Set.empty, [], Map.empty, Set.empty, Map.empty)
  where
    fin ::
         ( Set Text
         , [HashIncludeArgMsg]
         , Map DeclId (Set HashIncludeArg)
         , Set Hs.Identifier
         , Map DeclId [(Set HashIncludeArg, Omittable CTypeSpec)]
         )
      -> ( [BindingSpecReadMsg]
         , Set Hs.Identifier
         , Map DeclId [(Set HashIncludeArg, Omittable CTypeSpec)]
         )
    fin (invalids, msgs, conflicts, hsIds, cTypeMap) =
      let invalidErrs = BindingSpecReadInvalidCName path <$> Set.toList invalids
          argErrs = BindingSpecReadHashIncludeArg path <$> msgs
          conflictErrs = [
              BindingSpecReadCTypeConflict path cDeclId header
            | (cDeclId, headers) <- Map.toList conflicts
            , header <- Set.toList headers
            ]
      in  (invalidErrs ++ argErrs ++ conflictErrs, hsIds, cTypeMap)

    auxInsert ::
         AOCTypeSpecMapping
      -> ( Set Text
         , [HashIncludeArgMsg]
         , Map DeclId (Set HashIncludeArg)
         , Set Hs.Identifier
         , Map DeclId [(Set HashIncludeArg, Omittable CTypeSpec)]
         )
      -> ( Set Text
         , [HashIncludeArgMsg]
         , Map DeclId (Set HashIncludeArg)
         , Set Hs.Identifier
         , Map DeclId [(Set HashIncludeArg, Omittable CTypeSpec)]
         )
    auxInsert aoCTypeMapping (invalids, msgs, conflicts, hsIds, acc) =
      let (cname, headers, mHsId, oCTypeSpec) = case aoCTypeMapping of
            ARequire mapping ->
              let cTypeSpec = CTypeSpec {
                      hsIdent = mapping.identifier
                    , cRep   = unACTypeRep <$> mapping.rep
                    }
              in  ( mapping.cName
                  , mapping.headers
                  , mapping.identifier
                  , Require cTypeSpec
                  )
            AOmit mapping ->
              ( mapping.cName
              , mapping.headers
              , Nothing
              , Omit
              )
          (msgs', headers') = bimap ((msgs ++) . concat) Set.fromList $
            unzip (map hashIncludeArg headers)
          hsIds' = maybe hsIds (`Set.insert` hsIds) mHsId
          newV = [(headers', oCTypeSpec)]
      in  case parseDeclId cname of
            Nothing ->
              (Set.insert cname invalids, msgs', conflicts, hsIds', acc)
            Just cDeclId ->
              case Map.insertLookupWithKey (const (++)) cDeclId newV acc of
                (Nothing, acc') -> (invalids, msgs', conflicts, hsIds', acc')
                (Just oldV, acc') ->
                  let conflicts' = auxDup cDeclId newV oldV conflicts
                  in  (invalids, msgs', conflicts', hsIds', acc')

    auxDup ::
         DeclId
      -> [(Set HashIncludeArg, a)]
      -> [(Set HashIncludeArg, a)]
      -> Map DeclId (Set HashIncludeArg)
      -> Map DeclId (Set HashIncludeArg)
    auxDup cDeclId newV oldV =
      case Set.intersection (mconcat (fst <$> newV)) (mconcat (fst <$> oldV)) of
        commonHeaders
          | Set.null commonHeaders -> id
          | otherwise -> Map.insertWith Set.union cDeclId commonHeaders

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
    auxInsert mapping (conflicts, acc) =
      let hsId       = mapping.identifier
          hsTypeSpec = HsTypeSpec {
              hsRep     = unAHsTypeRep <$> mapping.rep
            , instances = mkInstanceMap mapping.instances
            }
      in  case Map.insertLookupWithKey (\_ n _ -> n) hsId hsTypeSpec acc of
            (Nothing, acc') -> (conflicts,                 acc')
            (Just{},  acc') -> (Set.insert hsId conflicts, acc')

-- duplicates ignored, last value retained
mkInstanceMap ::
     [AOInstanceSpecMapping]
  -> Map Hs.TypeClass (Omittable InstanceSpec)
mkInstanceMap xs = Map.fromList . flip map xs $ \case
    ARequire mapping ->
      let inst = InstanceSpec {
              strategy    = unAStrategySpec <$> mapping.strategy
            , constraints = [
                  constr
                | AConstraintSpec constr <- mapping.constraints
                ]
            }
      in  (mapping.clss, Require inst)
    AOmit hsTypeClass -> (hsTypeClass, Omit)

--------------------------------------------------------------------------------

toABindingSpec ::
     (DeclId -> DeclId -> Ordering)
  -> UnresolvedBindingSpec
  -> ABindingSpec
toABindingSpec compareCDeclId spec = ABindingSpec {
      version  = mkAVersion currentBindingSpecVersion
    , target   = ABindingSpecTarget spec.target
    , hsModule = spec.moduleName
    , cTypes   = toAOCTypes compareCDeclId spec.cTypes
    , hsTypes  = toAHsTypes spec.hsTypes
    }

toAOCTypes ::
     (DeclId -> DeclId -> Ordering)
  -> Map DeclId [(Set HashIncludeArg, Omittable CTypeSpec)]
  -> [AOCTypeSpecMapping]
toAOCTypes compareCDeclId cTypeMap = map snd $ List.sortBy aux [
      (cDeclId,) $ case oCTypeSpec of
        Require spec -> ARequire ACTypeSpecMapping {
            headers    = map (.path) (Set.toAscList headers)
          , cName      = renderDeclId cDeclId
          , identifier = spec.hsIdent
          , rep        = ACTypeRep <$> spec.cRep
          }
        Omit -> AOmit AKCTypeSpecMapping {
            headers = map (.path) (Set.toAscList headers)
          , cName   = renderDeclId cDeclId
          }
    | (cDeclId, xs) <- Map.toAscList cTypeMap
    , (headers, oCTypeSpec) <- xs
    ]
  where
    aux ::
         (DeclId, AOCTypeSpecMapping)
      -> (DeclId, AOCTypeSpecMapping)
      -> Ordering
    aux (cDeclIdL, xL) (cDeclIdR, xR) =
      case compareCDeclId cDeclIdL cDeclIdR of
        LT -> LT
        GT -> GT
        EQ -> Ord.comparing headersOf xL xR

    headersOf :: AOCTypeSpecMapping -> [FilePath]
    headersOf = \case
      ARequire x -> x.headers
      AOmit    x -> x.headers

toAHsTypes :: Map Hs.Identifier HsTypeSpec -> [AHsTypeSpecMapping]
toAHsTypes hsTypeMap = [
      AHsTypeSpecMapping {
          identifier = hsIdentifier
        , rep        = AHsTypeRep <$> spec.hsRep
        , instances  = toAOInstances spec.instances
        }
    | (hsIdentifier, spec) <- Map.toAscList hsTypeMap
    ]

toAOInstances ::
     Map Hs.TypeClass (Omittable InstanceSpec)
  -> [AOInstanceSpecMapping]
toAOInstances instMap = [
      case oInstSpec of
        Require spec -> ARequire AInstanceSpecMapping {
            clss        = hsTypeClass
          , strategy    = AStrategySpec <$> spec.strategy
          , constraints = map AConstraintSpec spec.constraints
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
