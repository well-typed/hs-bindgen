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
-- > import HsBindgen.BindingSpec.Private.V1 (V1)
-- > import HsBindgen.BindingSpec.Private.V1 qualified as V1
module HsBindgen.BindingSpec.Private.V1 (
    -- * Version
    currentBindingSpecVersion
    -- * Types
  , BindingSpec(..)
  , UnresolvedBindingSpec
  , ResolvedBindingSpec
  , CTypeSpec(..)
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
  , getCTypes
  , lookupCTypeSpec
  , lookupHsTypeSpec
    -- ** YAML/JSON
  , readFile
  , encode
    -- ** Header resolution
  , resolve
    -- ** Merging
  , MergedBindingSpecs
  , merge
  , lookupMergedBindingSpecs
    -- ** Aeson representation
  , V1
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
import Text.Read (readMaybe)

import Clang.Args
import Clang.Paths

import HsBindgen.Runtime.BaseForeignType qualified as BFT

import HsBindgen.BindingSpec.Private.Common
import HsBindgen.BindingSpec.Private.Version
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
      -- | Binding specification module
      --
      -- Each binding specification is specific to a Haskell module.
      --
      -- The module name is optional in prescriptive binding specifications.  If
      -- one is specified, it must match the current module.  If not specified,
      -- the current module is used.
      --
      -- The module name is required in external binding specifications.
      moduleName :: Hs.ModuleName

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

-- | Binding specification for a C type
data CTypeSpec = CTypeSpec {
      -- | Haskell identifier
      hsIdent :: Maybe Hs.Identifier
    }
  deriving stock (Show, Eq, Ord, Generic)

instance Default CTypeSpec where
  def = CTypeSpec{
      hsIdent = Nothing
    }

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
  def = HsTypeSpec{
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

  | -- | Empty data representation
    --
    -- A type but no constructor is generated using @data@.
    HsTypeRepEmptyData

  | -- | Type alias representation
    --
    -- A type is generated using @type@.
    HsTypeRepTypeAlias
  deriving stock (Show, Eq, Ord, Generic)

-- | Haskell record representation
data HsRecordRep = HsRecordRep {
      constructor :: Maybe Hs.Identifier   -- ^ Constructor name
    , fields      :: Maybe [Hs.Identifier] -- ^ Field names
    }
  deriving stock (Show, Eq, Ord, Generic)

instance Default HsRecordRep where
  def = HsRecordRep{
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
      -- function around the foreign import declaration that unwraps and rewraps
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
  def = HsNewtypeRep{
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
  def = InstanceSpec{
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

-- | Construct an empty binding specification for the given module
empty :: Hs.ModuleName -> BindingSpec header
empty hsModuleName = BindingSpec{
      moduleName = hsModuleName
    , cTypes     = Map.empty
    , hsTypes    = Map.empty
    }

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
  -> Maybe Hs.ModuleName
  -> FilePath
  -> IO (Maybe UnresolvedBindingSpec)
readFile tracer cmpt mHsModuleName path = readVersion tracer path >>= \case
    Nothing -> return Nothing
    Just (aVersion, value)
      | isCompatBindingSpecVersions
          cmpt
          aVersion.bindingSpec
          currentBindingSpecVersion -> do
            traceWith tracer $ BindingSpecReadParseVersion path aVersion
            case Aeson.fromJSON value of
              Aeson.Success arep ->
                case fromABindingSpec mHsModuleName path arep of
                  Right (errs, spec) -> do
                    mapM_ (traceWith tracer) errs
                    return (Just spec)
                  Left err -> do
                    traceWith tracer err
                    return Nothing
              Aeson.Error err -> do
                traceWith tracer $ BindingSpecReadAesonError path err
                return Nothing
      | otherwise -> do
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

encodeJson' :: ARep V1 UnresolvedBindingSpec -> ByteString
encodeJson' = BSL.toStrict . Aeson.encode

encodeYaml' :: ARep V1 UnresolvedBindingSpec -> ByteString
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
      -- AInstanceSpec:1, AConstraintSpec:1
      "class"                 ->  4
      -- AInstanceSpec:2
      "strategy"              ->  5
      -- AInstanceSpec:3
      "constraints"           ->  6
      -- ABindingSpec:2, AConstraintSpec:2
      "hsmodule"              ->  7
      -- ABindingSpec:3
      "ctypes"                ->  8
      -- ABindingSpec:4
      "hstypes"               ->  9
      -- ACTypeSpec:1
      "headers"               -> 10
      -- ACTypeSpec:2
      "cname"                 -> 11
      -- ACTypeSpec:3, AHsTypeSpec:1, AConstraintSpec:3
      "hsname"                -> 12
      -- ACTypeSpec:4, AHsTypeSpec:2
      "representation"        -> 13
      -- AHsTypeSpec:3
      "instances"             -> 14
      -- AHsTypeRep:1
      "record"                -> 15
      -- AHsTypeRep:2
      "newtype"               -> 16
      -- HsRecordRep:1, HsNewtypeRep:1
      "constructor"           -> 17
      -- HsRecordRep:2, HsNewtypeRep:2
      "fields"                -> 18
      -- HsNewtypeRep:3
      "ffitype"               -> 19
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
        moduleName = uSpec.moduleName
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
      map :: Map DeclId [(Set SourcePath, ResolvedBindingSpec)]
    }
  deriving stock (Show)

-- | Merge (external) binding specifications
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
      =<< Map.lookup cDeclId specs.map
    (hsModuleName, oCTypeSpec) <- lookupCTypeSpec cDeclId headers spec
    let mHsTypeSpec = case oCTypeSpec of
          Require cTypeSpec -> do
            hsIdentifier <- cTypeSpec.hsIdent
            lookupHsTypeSpec hsIdentifier spec
          Omit -> Nothing
    return (hsModuleName, oCTypeSpec, mHsTypeSpec)

{-------------------------------------------------------------------------------
  Aeson representation
-------------------------------------------------------------------------------}

-- | Aeson representation version
type V1 :: ARepV
data V1 a

-- | Convert from the Aeson representation /for this version/
fromARep' :: ARepIso V1 a => ARep V1 a -> a
fromARep' = fromARep

-- | Convert to the Aeson representation /for this version/
toARep' :: ARepIso V1 a => a -> ARep V1 a
toARep' = toARep

--------------------------------------------------------------------------------

data instance ARep V1 UnresolvedBindingSpec = ABindingSpec {
      version  :: AVersion
    , hsModule :: Maybe Hs.ModuleName
    , cTypes   :: [AOCTypeSpec]
    , hsTypes  :: [ARep V1 HsTypeSpec]
    }
  deriving stock (Show)

instance Aeson.FromJSON (ARep V1 UnresolvedBindingSpec) where
  parseJSON = Aeson.withObject "BindingSpec" $ \o -> do
    aBindingSpecVersion  <- o .:  "version"
    aBindingSpecHsModule <- o .:? "hsmodule"
    aBindingSpecCTypes   <- o .:? "ctypes"  .!= []
    aBindingSpecHsTypes  <- o .:? "hstypes" .!= []
    return ABindingSpec{
        version  = aBindingSpecVersion
      , hsModule = fromARep' <$> aBindingSpecHsModule
      , cTypes   = aBindingSpecCTypes
      , hsTypes  = aBindingSpecHsTypes
      }

instance Aeson.ToJSON (ARep V1 UnresolvedBindingSpec) where
  toJSON spec = Aeson.Object . KM.fromList $ catMaybes [
      Just ("version" .= spec.version)
    , ("hsmodule" .=) . toARep' <$> spec.hsModule
    , ("ctypes"   .=) <$> omitWhenNull spec.cTypes
    , ("hstypes"  .=) <$> omitWhenNull spec.hsTypes
    ]

fromABindingSpec ::
     Maybe Hs.ModuleName
  -> FilePath
  -> ARep V1 UnresolvedBindingSpec
  -> Either BindingSpecReadMsg ([BindingSpecReadMsg], UnresolvedBindingSpec)
fromABindingSpec mHsModuleName path arep = do
    (moduleErrs, hsModuleName) <- case (arep.hsModule, mHsModuleName) of
      (Just bsModule, Just curModule)
        | bsModule == curModule -> return ([], bsModule)
        | otherwise -> return
            ([BindingSpecReadModuleMismatch path bsModule curModule], bsModule)
      (Just bsModule, Nothing) -> return ([], bsModule)
      (Nothing, Just curModule) -> return ([], curModule)
      (Nothing, Nothing) -> Left $ BindingSpecReadModuleNotSpecified path
    let (cTypeErrs, hsIds, bindingSpecCTypes) =
          fromAOCTypeSpecs path arep.cTypes
        (hsTypeErrs, bindingSpecHsTypes) =
          fromAHsTypeSpecs path hsIds arep.hsTypes
    return
      ( moduleErrs ++ cTypeErrs ++ hsTypeErrs
      , BindingSpec{
            moduleName = hsModuleName
          , cTypes     = bindingSpecCTypes
          , hsTypes    = bindingSpecHsTypes
          }
      )

toABindingSpec ::
     (DeclId -> DeclId -> Ordering)
  -> UnresolvedBindingSpec
  -> ARep V1 UnresolvedBindingSpec
toABindingSpec compareCDeclId spec = ABindingSpec{
      version  = mkAVersion currentBindingSpecVersion
    , hsModule = Just spec.moduleName
    , cTypes   = toAOCTypeSpecs compareCDeclId spec.cTypes
    , hsTypes  = toAHsTypeSpecs spec.hsTypes
    }

--------------------------------------------------------------------------------

newtype instance ARep V1 Hs.ModuleName = AModuleName Hs.ModuleName
  deriving stock (Show)

instance ARepIso V1 Hs.ModuleName

instance Aeson.FromJSON (ARep V1 Hs.ModuleName) where
  parseJSON = Aeson.withText "ModuleName" $ return . AModuleName . Hs.ModuleName

instance Aeson.ToJSON (ARep V1 Hs.ModuleName) where
  toJSON (AModuleName moduleName) = Aeson.String moduleName.text

--------------------------------------------------------------------------------

data instance ARep V1 CTypeSpec = ACTypeSpec {
      headers :: [FilePath]
    , cName   :: Text
    , hsIdent :: Maybe Hs.Identifier
    }
  deriving stock (Show)

instance Aeson.FromJSON (ARep V1 CTypeSpec) where
  parseJSON = Aeson.withObject "CTypeSpec" $ \o -> do
    aCTypeSpecHeaders <- o .:  "headers" >>= listFromJSON
    aCTypeSpecCName   <- o .:  "cname"
    aCTypeSpecHsIdent <- o .:? "hsname"
    return ACTypeSpec{
        headers = aCTypeSpecHeaders
      , cName   = aCTypeSpecCName
      , hsIdent = fromARep' <$> aCTypeSpecHsIdent
      }

instance Aeson.ToJSON (ARep V1 CTypeSpec) where
  toJSON arep = Aeson.Object . KM.fromList $ catMaybes [
      Just ("headers" .= listToJSON arep.headers)
    , Just ("cname"   .= arep.cName)
    , ("hsname" .=) . toARep' <$> arep.hsIdent
    ]

instance ARepKV V1 CTypeSpec where
  data ARepK V1 CTypeSpec = AKCTypeSpec {
      headers :: [FilePath]
    , cName   :: Text
    }

  fromARepKV arep =
    ( AKCTypeSpec{
          headers = arep.headers
        , cName   = arep.cName
        }
    , CTypeSpec{
          hsIdent = arep.hsIdent
        }
    )

  toARepKV k v = ACTypeSpec{
      headers = k.headers
    , cName   = k.cName
    , hsIdent = v.hsIdent
    }

deriving stock instance Show (ARepK V1 CTypeSpec)

instance Aeson.FromJSON (ARepK V1 CTypeSpec) where
  parseJSON = Aeson.withObject "AKCTypeSpec" $ \o -> do
    akCTypeSpecHeaders <- o .: "headers" >>= listFromJSON
    akCTypeSpecCName   <- o .: "cname"
    return AKCTypeSpec{
        headers = akCTypeSpecHeaders
      , cName   = akCTypeSpecCName
      }

instance Aeson.ToJSON (ARepK V1 CTypeSpec) where
  toJSON key = Aeson.Object $ KM.fromList [
      "headers" .= listToJSON key.headers
    , "cname"   .= key.cName
    ]

type AOCTypeSpec = AOmittable (ARepK V1 CTypeSpec) (ARep V1 CTypeSpec)

fromAOCTypeSpecs ::
     FilePath
  -> [AOCTypeSpec]
  -> ( [BindingSpecReadMsg]
     , Set Hs.Identifier
     , Map DeclId [(Set HashIncludeArg, Omittable CTypeSpec)]
     )
fromAOCTypeSpecs path =
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
         AOCTypeSpec
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
            ARequire arep ->
              let (k, cTypeSpec) = fromARepKV arep
              in  (k.cName, k.headers, cTypeSpec.hsIdent, Require cTypeSpec)
            AOmit k -> (k.cName, k.headers, Nothing, Omit)
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

toAOCTypeSpecs ::
     (DeclId -> DeclId -> Ordering)
  -> Map DeclId [(Set HashIncludeArg, Omittable CTypeSpec)]
  -> [AOCTypeSpec]
toAOCTypeSpecs compareCDeclId cTypeMap = map snd $ List.sortBy aux [
      (cDeclId,) $ case oCTypeSpec of
        Require spec -> ARequire ACTypeSpec{
            headers = map (.path) (Set.toAscList headers)
          , cName   = renderDeclId cDeclId
          , hsIdent = spec.hsIdent
          }
        Omit -> AOmit AKCTypeSpec{
            headers = map (.path) (Set.toAscList headers)
          , cName   = renderDeclId cDeclId
          }
    | (cDeclId, xs) <- Map.toAscList cTypeMap
    , (headers, oCTypeSpec) <- xs
    ]
  where
    aux :: (DeclId, AOCTypeSpec) -> (DeclId, AOCTypeSpec) -> Ordering
    aux (cDeclIdL, xL) (cDeclIdR, xR) =
      case compareCDeclId cDeclIdL cDeclIdR of
        LT -> LT
        GT -> GT
        EQ -> Ord.comparing headersOf xL xR

    headersOf :: AOCTypeSpec -> [FilePath]
    headersOf = \case
      ARequire x -> x.headers
      AOmit    x -> x.headers

--------------------------------------------------------------------------------

newtype instance ARep V1 Hs.Identifier = AHsIdentifier Hs.Identifier
  deriving stock (Show)

instance ARepIso V1 Hs.Identifier

instance Aeson.FromJSON (ARep V1 Hs.Identifier) where
  parseJSON = Aeson.withText "HsIdentifier" $
    return . AHsIdentifier . Hs.Identifier

instance Aeson.ToJSON (ARep V1 Hs.Identifier) where
  toJSON (AHsIdentifier hsIdent) = Aeson.String (hsIdent.text)

--------------------------------------------------------------------------------

data instance ARep V1 HsTypeSpec = AHsTypeSpec {
      hsIdent   :: Hs.Identifier
    , hsRep     :: Maybe HsTypeRep
    , instances :: [AOInstanceSpec]
    }
  deriving stock (Show)

instance Aeson.FromJSON (ARep V1 HsTypeSpec) where
  parseJSON = Aeson.withObject "HsTypeSpec" $ \o -> do
    aHsTypeSpecHsIdent   <- o .:  "hsname"
    aHsTypeSpecHsRep     <- o .:? "representation"
    aHsTypeSpecInstances <- o .:? "instances" .!= []
    return AHsTypeSpec{
        hsIdent   = fromARep' aHsTypeSpecHsIdent
      , hsRep     = fromARep' <$> aHsTypeSpecHsRep
      , instances = aHsTypeSpecInstances
      }

instance Aeson.ToJSON (ARep V1 HsTypeSpec) where
  toJSON arep = Aeson.Object . KM.fromList $ catMaybes [
      Just ("hsname" .= toARep' arep.hsIdent)
    , ("representation" .=) . toARep' <$> arep.hsRep
    , ("instances" .=) <$> omitWhenNull arep.instances
    ]

instance ARepKV V1 HsTypeSpec where
  newtype ARepK V1 HsTypeSpec = AKHsTypeSpec { unwrap :: Hs.Identifier }

  fromARepKV arep =
    ( AKHsTypeSpec arep.hsIdent
    , HsTypeSpec{
          hsRep     = arep.hsRep
        , instances = fromAOInstanceSpecs arep.instances
        }
    )

  toARepKV k v = AHsTypeSpec{
      hsIdent   = k.unwrap
    , hsRep     = v.hsRep
    , instances = toAOInstanceSpecs v.instances
    }

deriving stock instance Show (ARepK V1 HsTypeSpec)

instance Aeson.FromJSON (ARepK V1 HsTypeSpec) where
  parseJSON = fmap (AKHsTypeSpec . fromARep') . Aeson.parseJSON

instance Aeson.ToJSON (ARepK V1 HsTypeSpec) where
  toJSON = Aeson.toJSON . toARep' . (.unwrap)

fromAHsTypeSpecs ::
     FilePath
  -> Set Hs.Identifier
  -> [ARep V1 HsTypeSpec]
  -> ([BindingSpecReadMsg], Map Hs.Identifier HsTypeSpec)
fromAHsTypeSpecs path hsIds = fin . foldr auxInsert (Set.empty, Map.empty)
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
         ARep V1 HsTypeSpec
      -> (Set Hs.Identifier, Map Hs.Identifier HsTypeSpec)
      -> (Set Hs.Identifier, Map Hs.Identifier HsTypeSpec)
    auxInsert arep (conflicts, acc) =
      let (k, hsTypeSpec) = fromARepKV arep
      in  case Map.insertLookupWithKey (\_ n _ -> n) k.unwrap hsTypeSpec acc of
            (Nothing, acc') -> (conflicts,                     acc')
            (Just{},  acc') -> (Set.insert k.unwrap conflicts, acc')

toAHsTypeSpecs ::
     Map Hs.Identifier HsTypeSpec
  -> [ARep V1 HsTypeSpec]
toAHsTypeSpecs hsTypeMap = [
      toARepKV (AKHsTypeSpec hsIdentifier) spec
    | (hsIdentifier, spec) <- Map.toAscList hsTypeMap
    ]

--------------------------------------------------------------------------------

newtype instance ARep V1 HsTypeRep = AHsTypeRep HsTypeRep
  deriving stock (Show)

instance ARepIso V1 HsTypeRep

instance Aeson.FromJSON (ARep V1 HsTypeRep) where
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
        v -> Aeson.withText "HsTypeRep" parseHsTypeRepText v

      parseHsTypeRepText :: Text -> Aeson.Parser HsTypeRep
      parseHsTypeRepText t = case t of
        "record"    -> return (HsTypeRepRecord def)
        "newtype"   -> return (HsTypeRepNewtype def)
        "emptydata" -> return HsTypeRepEmptyData
        "typealias" -> return HsTypeRepTypeAlias
        _           ->
          Aeson.parseFail $ "unknown Haskell representation: " ++ Text.unpack t

      parseHsRecordRep :: Aeson.Value -> Aeson.Parser HsRecordRep
      parseHsRecordRep = Aeson.withObject "HsRecordRep" $ \o -> do
        hsRecordRepConstructor <- o .:? "constructor"
        hsRecordRepFields      <- o .:? "fields"
        return HsRecordRep{
            constructor = fromARep' <$> hsRecordRepConstructor
          , fields      = map fromARep' <$> hsRecordRepFields
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
        hsNewtypeRepFFIType <- fmap fromARep' <$> (o .:? "ffitype")
        return HsNewtypeRep{
            constructor = fromARep' <$> hsNewtypeRepConstructor
          , field       = fromARep' <$> hsNewtypeRepField
          , ffiType     = hsNewtypeRepFFIType
          }

instance Aeson.ToJSON (ARep V1 HsTypeRep) where
  toJSON (AHsTypeRep hsTypeRep) = case hsTypeRep of
    HsTypeRepRecord x
      | x == def  -> Aeson.String "record"
      | otherwise ->
            Aeson.Object . KM.singleton "record"
          . Aeson.Object . KM.fromList
          $ catMaybes [
                ("constructor" .=) . toARep' <$> x.constructor
              , ("fields"      .=) . map toARep' <$> x.fields
              ]
    HsTypeRepNewtype x
      | x == def  -> Aeson.String "newtype"
      | otherwise ->
            Aeson.Object . KM.singleton "newtype"
          . Aeson.Object . KM.fromList
          $ catMaybes [
                ("constructor" .=) . toARep' <$> x.constructor
              , ("fields"      .=) . (: []) . toARep' <$> x.field
              , ("ffitype"     .=) . toARep' <$> x.ffiType
              ]
    HsTypeRepEmptyData -> Aeson.String "emptydata"
    HsTypeRepTypeAlias -> Aeson.String "typealias"

--------------------------------------------------------------------------------

newtype instance ARep V1 FFIType = AFFIType FFIType
  deriving stock (Show)

instance ARepIso V1 FFIType

instance Aeson.FromJSON (ARep V1 FFIType) where
  parseJSON = Aeson.withText "FFIType" $ \t ->
    case Map.lookup t ffiTypeFromText of
      Just ffitype -> return (AFFIType ffitype)
      Nothing -> Aeson.parseFail $ "unknown ffitype: " ++ Text.unpack t

instance Aeson.ToJSON (ARep V1 FFIType) where
  toJSON (AFFIType ffiType) = Aeson.String (ffiTypeText ffiType)

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

data instance ARep V1 InstanceSpec = AInstanceSpec {
      clss        :: Hs.TypeClass
    , strategy    :: Maybe StrategySpec
    , constraints :: [ConstraintSpec]
    }
  deriving stock (Show)

instance Aeson.FromJSON (ARep V1 InstanceSpec) where
  parseJSON = \case
    s@Aeson.String{} -> do
      aInstanceSpecClass <- Aeson.parseJSON s
      let aInstanceSpecStrategy    = Nothing
          aInstanceSpecConstraints = []
      return AInstanceSpec{
          clss        = fromARep' aInstanceSpecClass
        , strategy    = aInstanceSpecStrategy
        , constraints = aInstanceSpecConstraints
        }
    Aeson.Object o -> do
      aInstanceSpecClass       <- o .:  "class"
      aInstanceSpecStrategy    <- o .:? "strategy"
      aInstanceSpecConstraints <- o .:? "constraints" .!= []
      return AInstanceSpec{
          clss        = fromARep' aInstanceSpecClass
        , strategy    = fromARep' <$> aInstanceSpecStrategy
        , constraints = fromARep' <$> aInstanceSpecConstraints
        }
    v -> Aeson.parseFail $
      "expected InstanceSpec String or Object, but encountered " ++ typeOf v

instance Aeson.ToJSON (ARep V1 InstanceSpec) where
  toJSON arep
    | isNothing arep.strategy && null arep.constraints =
        Aeson.toJSON (toARep' arep.clss)
    | otherwise = Aeson.Object . KM.fromList $ catMaybes [
          Just ("class" .= toARep' arep.clss)
        , ("strategy"    .=) . toARep' <$> arep.strategy
        , ("constraints" .=) . fmap toARep' <$> omitWhenNull arep.constraints
        ]

instance ARepKV V1 InstanceSpec where
  newtype ARepK V1 InstanceSpec = AKInstanceSpec { unwrap :: Hs.TypeClass }

  fromARepKV arep =
    ( AKInstanceSpec arep.clss
    , InstanceSpec{
          strategy    = arep.strategy
        , constraints = arep.constraints
        }
    )

  toARepKV k v = AInstanceSpec{
      clss        = k.unwrap
    , strategy    = v.strategy
    , constraints = v.constraints
    }

deriving stock instance Show (ARepK V1 InstanceSpec)

instance Aeson.FromJSON (ARepK V1 InstanceSpec) where
  parseJSON = fmap (AKInstanceSpec . fromARep') . Aeson.parseJSON

instance Aeson.ToJSON (ARepK V1 InstanceSpec) where
  toJSON = Aeson.toJSON . toARep' . (.unwrap)

type AOInstanceSpec = AOmittable (ARepK V1 InstanceSpec) (ARep V1 InstanceSpec)

-- duplicates ignored, last value retained
fromAOInstanceSpecs ::
     [AOInstanceSpec]
  -> Map Hs.TypeClass (Omittable InstanceSpec)
fromAOInstanceSpecs xs = Map.fromList . flip map xs $ \case
    ARequire arep -> bimap (.unwrap) Require (fromARepKV arep)
    AOmit    k    -> (k.unwrap, Omit)

toAOInstanceSpecs ::
     Map Hs.TypeClass (Omittable InstanceSpec)
  -> [AOInstanceSpec]
toAOInstanceSpecs instMap = [
      case oInstSpec of
        Require spec -> ARequire $ toARepKV (AKInstanceSpec hsTypeClass) spec
        Omit         -> AOmit (AKInstanceSpec hsTypeClass)
    | (hsTypeClass, oInstSpec) <- Map.toAscList instMap
    ]

--------------------------------------------------------------------------------

newtype instance ARep V1 Hs.TypeClass = ATypeClass Hs.TypeClass
  deriving stock (Show)

instance ARepIso V1 Hs.TypeClass

instance Aeson.FromJSON (ARep V1 Hs.TypeClass) where
  parseJSON = Aeson.withText "TypeClass" $ \t ->
    let s = Text.unpack t
    in  case readMaybe s of
          Just clss -> return (ATypeClass clss)
          Nothing   -> Aeson.parseFail $ "unknown type class: " ++ s

instance Aeson.ToJSON (ARep V1 Hs.TypeClass) where
  toJSON (ATypeClass clss) = Aeson.String $ Text.pack (show clss)

--------------------------------------------------------------------------------

newtype instance ARep V1 StrategySpec = AStrategySpec StrategySpec
  deriving stock (Show)

instance ARepIso V1 StrategySpec

instance Aeson.FromJSON (ARep V1 StrategySpec) where
  parseJSON = Aeson.withText "StrategySpec" $ \t ->
    case Map.lookup t strategySpecFromText of
      Just strategy -> return (AStrategySpec strategy)
      Nothing -> Aeson.parseFail $ "unknown strategy: " ++ Text.unpack t

instance Aeson.ToJSON (ARep V1 StrategySpec) where
  toJSON (AStrategySpec spec) = Aeson.String (strategySpecText spec)

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

newtype instance ARep V1 ConstraintSpec = AConstraintSpec ConstraintSpec
  deriving stock (Show)

instance ARepIso V1 ConstraintSpec

instance Aeson.FromJSON (ARep V1 ConstraintSpec) where
  parseJSON = Aeson.withObject "ConstraintSpec" $ \o -> do
      constraintSpecClass <- o .: "class"
      extRefModule        <- o .: "hsmodule"
      extRefIdentifier    <- o .: "hsname"
      let constraintSpecRef = Hs.ExtRef{
              moduleName = fromARep' extRefModule
            , ident      = fromARep' extRefIdentifier
            }
      return $ AConstraintSpec ConstraintSpec{
          clss = fromARep' constraintSpecClass
        , ref  = constraintSpecRef
        }

instance Aeson.ToJSON (ARep V1 ConstraintSpec) where
  toJSON (AConstraintSpec spec) = Aeson.object [
        "class"    .= toARep' spec.clss
      , "hsmodule" .= toARep' spec.ref.moduleName
      , "hsname"   .= toARep' spec.ref.ident
      ]

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- 'List.lookup' using a predicate
lookupBy :: (a -> Bool) -> [(a, b)] -> Maybe b
lookupBy p = fmap snd . List.find (p . fst)
