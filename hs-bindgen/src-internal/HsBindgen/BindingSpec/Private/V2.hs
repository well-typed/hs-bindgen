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
-- > import HsBindgen.BindingSpec.Private.V2 qualified as BindingSpec
--
-- When distinguishing separate versions:
--
-- > import HsBindgen.BindingSpec.Private.V2 (V2)
-- > import HsBindgen.BindingSpec.Private.V2 qualified as V2
module HsBindgen.BindingSpec.Private.V2 (
    -- * Version
    currentBindingSpecVersion
    -- * Types
  , BindingSpec(..)
  , UnresolvedBindingSpec
  , ResolvedBindingSpec
  , CTypeSpec(..)
  , CEnumSpec(..)
  , HsTypeSpec(..)
  , HsTypeRep(..)
  , HsRecordRep(..)
  , HsNewtypeRep(..)
  , hsSpecFFIType
    -- ** Instances
  , InstanceSpec(..)
    -- * API
  , empty
  , getCTypes
  , lookupCTypeSpec
  , lookupHsTypeSpec
    -- ** YAML/JSON
  , parseValue
  , encode
    -- ** Header resolution
  , resolve
    -- ** Merging
  , MergedBindingSpecs
  , merge
  , lookupMergedBindingSpecs
    -- ** Aeson representation
  , V2
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
import Text.SimplePrettyPrint qualified as PP

import Clang.Args
import Clang.Paths

import HsBindgen.BindingSpec.Private.Common
import HsBindgen.BindingSpec.Private.V1 qualified as V1
import HsBindgen.BindingSpec.Private.Version
import HsBindgen.Config.MangleCandidate (MangleCandidate (..))
import HsBindgen.Config.MangleCandidate qualified as MangleCandidate
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst
import HsBindgen.IR.C qualified as C
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
currentBindingSpecVersion = $$(constBindingSpecVersion 2 0)

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
      -- A C type is identified using a 'C.DeclId' and a set of headers that
      -- provide the type.  For a given 'C.DeclId', the sets of headers are
      -- disjoint.  The type of this map is therefore equivalent to
      -- @'Map' 'C.DeclId' ('Map' header ('Omittable' t'CTypeSpec'))@, but this
      -- type is used as an optimization.
    , cTypes :: Map C.DeclId [(Set header, Omittable CTypeSpec)]

      -- | Haskell type specifications
    , hsTypes :: Map (Hs.Name Hs.NsTypeConstr) HsTypeSpec
    }
  deriving stock (Eq, Generic, Show)

-- | Binding specification with unresolved headers
--
-- The headers are as specified in a C include directive, relative to a
-- directory in the C include search path.
type UnresolvedBindingSpec = BindingSpec C.HashIncludeArg

-- | Binding specification with resolved headers
--
-- The resolved header is the canonical on-disk path in the current environment.
type ResolvedBindingSpec = BindingSpec (C.HashIncludeArg, RealPath)

--------------------------------------------------------------------------------

-- | Binding specification for a C type
data CTypeSpec = CTypeSpec {
      hsName :: Maybe (Hs.Name Hs.NsTypeConstr)
    , enum   :: Maybe CEnumSpec -- ^ Specified for C @enum@ types
    }
  deriving stock (Show, Eq, Ord, Generic)

--------------------------------------------------------------------------------

-- | C @enum@ specification
data CEnumSpec =
    CEnumOpen   -- ^ C @enum@ may have values other than those declared
  | CEnumClosed -- ^ C @enum@ may only have declared vlaues
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

instance Default CEnumSpec where
  def = CEnumOpen

--------------------------------------------------------------------------------

-- | Binding specification for a Haskell type
data HsTypeSpec = HsTypeSpec {
      -- | Haskell type representation
      hsRep :: Maybe HsTypeRep

       -- | Instance specification
    , instances :: Map Inst.TypeClass (Omittable InstanceSpec)
    }
  deriving stock (Show, Eq, Ord, Generic)

instance Default HsTypeSpec where
  def = HsTypeSpec{
      hsRep     = Nothing
    , instances = Map.empty
    }

hsSpecFFIType :: HsTypeSpec -> Maybe Hs.ExtRef
hsSpecFFIType hsSpec = do
    oInst <- Map.lookup Inst.HasFFIType hsSpec.instances
    case oInst of
      Require inst -> (.ref) <$> List.find Inst.isFFIType inst.associated
      Omit         -> Nothing

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
      constructor :: Maybe (Hs.Name Hs.NsConstr)
    , fields      :: Maybe [Hs.Name Hs.NsVar]
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
      constructor :: Maybe (Hs.Name Hs.NsConstr)

      -- | Field name
    , field :: Maybe (Hs.Name Hs.NsVar)
    }
  deriving stock (Show, Eq, Ord, Generic)

instance Default HsNewtypeRep where
  def = HsNewtypeRep{
        constructor = Nothing
      , field       = Nothing
      }

{-------------------------------------------------------------------------------
  Types: Instances
-------------------------------------------------------------------------------}

-- | Instance specification
data InstanceSpec = InstanceSpec {
      -- | Strategy used to generate/derive the instance
      --
      -- A 'Nothing' value indicates that @hs-bindgen@ defaults should be used.
      strategy :: Maybe Inst.Strategy

      -- | Instance constraints
      --
      -- If specified, /all/ constraints must be listed.
    , constraints :: [Inst.Constraint]

      -- | Associated types
    , associated :: [Inst.AssociatedType]
    }
  deriving stock (Show, Eq, Ord, Generic)

instance Default InstanceSpec where
  def = InstanceSpec{
      strategy    = Nothing
    , constraints = []
    , associated  = []
    }

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
getCTypes :: ResolvedBindingSpec -> Map C.DeclId [Set RealPath]
getCTypes spec = map (Set.map snd . fst) <$> spec.cTypes

-- | Lookup a C type in a 'ResolvedBindingSpec'
lookupCTypeSpec ::
     C.DeclId
  -> Set RealPath
  -> ResolvedBindingSpec
  -> Maybe (Hs.ModuleName, Omittable CTypeSpec)
lookupCTypeSpec cDeclId headers spec = do
    ps <- Map.lookup cDeclId spec.cTypes
    oCTypeSpec <- lookupBy (not . Set.disjoint headers . Set.map snd) ps
    return (spec.moduleName, oCTypeSpec)

-- | Lookup a Haskell type in a 'ResolvedBindingSpec'
lookupHsTypeSpec ::
     Hs.Name Hs.NsTypeConstr
  -> ResolvedBindingSpec
  -> Maybe HsTypeSpec
lookupHsTypeSpec hsName spec = Map.lookup hsName spec.hsTypes

{-------------------------------------------------------------------------------
  API: YAML/JSON
-------------------------------------------------------------------------------}

parseValue :: ParseSpecFunction UnresolvedBindingSpec
parseValue tracer cmpt mHsModuleName path aVersion value
  | isCompatBindingSpecVersions
      cmpt
      aVersion.bindingSpec
      currentBindingSpecVersion
  = do
      traceWith tracer $ withCallStack $
        BindingSpecReadCompatibleVersion path currentBindingSpecVersion aVersion.bindingSpec
      case Aeson.fromJSON value of
        Aeson.Success arep ->
          case fromABindingSpec mHsModuleName path arep of
            Right (errs, spec) -> do
              mapM_ (traceWith tracer . withCallStack) errs
              return (Right spec)
            Left err -> do
              traceWith tracer (withCallStack err)
              return (Left OtherError)
        Aeson.Error err -> do
          traceWith tracer $ withCallStack $ BindingSpecReadAesonError path err
          return (Left OtherError)
  | otherwise = do
      traceWith tracer $ withCallStack $
        BindingSpecReadIncompatibleVersion path currentBindingSpecVersion aVersion.bindingSpec
      mV1 <- V1.parseValue tracer cmpt mHsModuleName path aVersion value
      forM mV1 $ \v1 -> do
        traceWith tracer $ withCallStack $
          BindingSpecReadConvertVersion path V1.currentBindingSpecVersion currentBindingSpecVersion
        pure (fromV1 v1)

-- | Encode a binding specification
encode ::
     (C.DeclId -> C.DeclId -> Ordering)
  -> Format
  -> UnresolvedBindingSpec
  -> ByteString
encode compareCDeclId = \case
    FormatJSON -> encodeJson' . toABindingSpec compareCDeclId
    FormatYAML -> encodeYaml' . toABindingSpec compareCDeclId

encodeJson' :: ARep V2 UnresolvedBindingSpec -> ByteString
encodeJson' = BSL.toStrict . Aeson.encode

encodeYaml' :: ARep V2 UnresolvedBindingSpec -> ByteString
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
      -- AInstanceSpec:1, AConstraint:1
      "class"                 ->  4
      -- AInstanceSpec:2
      "strategy"              ->  5
      -- AInstanceSpec:3
      "constraints"           ->  6
      -- AAssociatedType:1
      "type"                  ->  7
      -- ABindingSpec:2, AConstraint:2, AAssociatedType:2
      "hsmodule"              ->  8
      -- ABindingSpec:3
      "ctypes"                ->  9
      -- ABindingSpec:4
      "hstypes"               -> 10
      -- ACTypeSpec:1
      "headers"               -> 11
      -- ACTypeSpec:2
      "cname"                 -> 12
      -- ACTypeSpec:3, AHsTypeSpec:1, AConstraint:3, AAssociatedType:3
      "hsname"                -> 13
      -- ACTypeSpec:4
      "enum"                  -> 14
      -- AHsTypeSpec:2
      "representation"        -> 15
      -- AHsTypeSpec:3
      "instances"             -> 16
      -- AHsTypeRep:1
      "record"                -> 17
      -- AHsTypeRep:2
      "newtype"               -> 18
      -- HsRecordRep:1, HsNewtypeRep:1
      "constructor"           -> 19
      -- HsRecordRep:2, HsNewtypeRep:2
      "fields"                -> 20
      -- AInstanceSpec:4
      "associated"            -> 21
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

    let lookup' :: C.HashIncludeArg -> Maybe (C.HashIncludeArg, RealPath)
        lookup' uHeader = (uHeader,) <$> Map.lookup uHeader headerMap

        resolveSet ::
             Set C.HashIncludeArg
          -> Maybe (Set (C.HashIncludeArg, RealPath))
        resolveSet uHeaders =
          case mapMaybe lookup' (Set.toList uHeaders) of
            []       -> Nothing
            rHeaders -> Just (Set.fromList rHeaders)

        resolveType ::
             C.DeclId
          -> (Set C.HashIncludeArg, a)
          -> IO (Maybe (Set (C.HashIncludeArg, RealPath), a))
        resolveType cDeclId (uHeaders, x) = case resolveSet uHeaders of
          Just rHeaders -> return $ Just (rHeaders, x)
          Nothing       -> do
            traceWith tracer $ withCallStack $ BindingSpecResolveTypeDropped cDeclId
            return Nothing

        resolveTypes ::
             C.DeclId
          -> [(Set C.HashIncludeArg, a)]
          -> IO
               ( Maybe
                   (C.DeclId, [(Set (C.HashIncludeArg, RealPath), a)])
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
    allHeaders :: Set C.HashIncludeArg
    allHeaders = mconcat $ fst <$> concat (Map.elems uSpec.cTypes)

{-------------------------------------------------------------------------------
  API: Merging
-------------------------------------------------------------------------------}

-- | Merged (external) binding specifications
--
-- While a t'HsBindgen.BindingSpec.Private.V2.BindingSpec' is specific to a
-- Haskell module, this type supports binding specifications across multiple
-- Haskell modules. It is a performance optimization for resolving external
-- binding specifications.
newtype MergedBindingSpecs = MergedBindingSpecs {
      map :: Map C.DeclId [(Set RealPath, ResolvedBindingSpec)]
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
    mkTypeErrs :: Set C.DeclId -> [BindingSpecMergeMsg]
    mkTypeErrs = fmap BindingSpecMergeConflict . Set.toList

    mergeSpec ::
         ( Set C.DeclId
         , ( Map C.DeclId (Set C.HashIncludeArg)
           , Map C.DeclId [(Set RealPath, ResolvedBindingSpec)]
           )
         )
      -> ResolvedBindingSpec
      -> ( Set C.DeclId
         , ( Map C.DeclId (Set C.HashIncludeArg)
           , Map C.DeclId [(Set RealPath, ResolvedBindingSpec)]
           )
         )
    mergeSpec ctx spec =
        foldl' (mergeType spec) ctx
      . map (fmap (map fst))
      $ Map.toList spec.cTypes

    mergeType ::
         ResolvedBindingSpec
      -> ( Set C.DeclId
         , ( Map C.DeclId (Set C.HashIncludeArg)
           , Map C.DeclId [(Set RealPath, ResolvedBindingSpec)]
           )
         )
      -> (C.DeclId, [Set (C.HashIncludeArg, RealPath)])
      -> ( Set C.DeclId
         , ( Map C.DeclId (Set C.HashIncludeArg)
           , Map C.DeclId [(Set RealPath, ResolvedBindingSpec)]
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

-- | Lookup type specs in t'MergedBindingSpecs'
lookupMergedBindingSpecs ::
     C.DeclId
  -> Set RealPath
  -> MergedBindingSpecs
  -> Maybe (Hs.ModuleName, Omittable CTypeSpec, Maybe HsTypeSpec)
lookupMergedBindingSpecs cDeclId headers specs = do
    spec <- lookupBy (not . Set.disjoint headers)
      =<< Map.lookup cDeclId specs.map
    (hsModuleName, oCTypeSpec) <- lookupCTypeSpec cDeclId headers spec
    let mHsTypeSpec = case oCTypeSpec of
          Require cTypeSpec -> do
            hsName <- cTypeSpec.hsName
            lookupHsTypeSpec hsName spec
          Omit -> Nothing
    return (hsModuleName, oCTypeSpec, mHsTypeSpec)

{-------------------------------------------------------------------------------
  Aeson representation
-------------------------------------------------------------------------------}

-- | Aeson representation version
type V2 :: ARepV
data V2 a

-- | Convert from the Aeson representation /for this version/
fromARep' :: ARepIso V2 a => ARep V2 a -> a
fromARep' = fromARep

-- | Convert to the Aeson representation /for this version/
toARep' :: ARepIso V2 a => a -> ARep V2 a
toARep' = toARep

--------------------------------------------------------------------------------

data instance ARep V2 UnresolvedBindingSpec = ABindingSpec {
      version  :: AVersion
    , hsModule :: Maybe Hs.ModuleName
    , cTypes   :: [AOCTypeSpec]
    , hsTypes  :: [ARep V2 HsTypeSpec]
    }
  deriving stock (Show)

instance Aeson.FromJSON (ARep V2 UnresolvedBindingSpec) where
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

instance Aeson.ToJSON (ARep V2 UnresolvedBindingSpec) where
  toJSON spec = Aeson.Object . KM.fromList $ catMaybes [
      Just ("version" .= spec.version)
    , ("hsmodule" .=) . toARep' <$> spec.hsModule
    , ("ctypes"   .=) <$> omitWhenNull spec.cTypes
    , ("hstypes"  .=) <$> omitWhenNull spec.hsTypes
    ]

fromABindingSpec ::
     Maybe Hs.ModuleName
  -> FilePath
  -> ARep V2 UnresolvedBindingSpec
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
    let (cTypeErrs, hsNms, bindingSpecCTypes) =
          fromAOCTypeSpecs path arep.cTypes
        (hsTypeErrs, bindingSpecHsTypes) =
          fromAHsTypeSpecs path hsNms arep.hsTypes
    return
      ( moduleErrs ++ cTypeErrs ++ hsTypeErrs
      , BindingSpec{
            moduleName = hsModuleName
          , cTypes     = bindingSpecCTypes
          , hsTypes    = bindingSpecHsTypes
          }
      )

toABindingSpec ::
     (C.DeclId -> C.DeclId -> Ordering)
  -> UnresolvedBindingSpec
  -> ARep V2 UnresolvedBindingSpec
toABindingSpec compareCDeclId spec = ABindingSpec{
      version  = mkAVersion currentBindingSpecVersion
    , hsModule = Just spec.moduleName
    , cTypes   = toAOCTypeSpecs compareCDeclId spec.cTypes
    , hsTypes  = toAHsTypeSpecs spec.hsTypes
    }

--------------------------------------------------------------------------------

newtype instance ARep V2 Hs.ModuleName = AModuleName Hs.ModuleName
  deriving stock (Show)

instance ARepIso V2 Hs.ModuleName

instance Aeson.FromJSON (ARep V2 Hs.ModuleName) where
  parseJSON = Aeson.withText "ModuleName" $ return . AModuleName . Hs.ModuleName

instance Aeson.ToJSON (ARep V2 Hs.ModuleName) where
  toJSON (AModuleName moduleName) = Aeson.String moduleName.text

--------------------------------------------------------------------------------

data instance ARep V2 CTypeSpec = ACTypeSpec {
      headers :: [FilePath]
    , cName   :: Text
    , hsName  :: Maybe (Hs.Name Hs.NsTypeConstr)
    , enum    :: Maybe CEnumSpec
    }
  deriving stock (Show)

instance Aeson.FromJSON (ARep V2 CTypeSpec) where
  parseJSON = Aeson.withObject "CTypeSpec" $ \o -> do
    aCTypeSpecHeaders <- o .:  "headers" >>= listFromJSON
    aCTypeSpecCName   <- o .:  "cname"
    aCTypeSpecHsName  <- o .:? "hsname"
    aCTypeSpecEnum    <- o .:? "enum"
    return ACTypeSpec{
        headers = aCTypeSpecHeaders
      , cName   = aCTypeSpecCName
      , hsName  = fromARep' <$> aCTypeSpecHsName
      , enum    = fromARep' <$> aCTypeSpecEnum
      }

instance Aeson.ToJSON (ARep V2 CTypeSpec) where
  toJSON arep = Aeson.Object . KM.fromList $ catMaybes [
      Just ("headers" .= listToJSON arep.headers)
    , Just ("cname"   .= arep.cName)
    , ("hsname" .=) . toARep' <$> arep.hsName
    , ("enum"   .=) . toARep' <$> arep.enum
    ]

instance ARepKV V2 CTypeSpec where
  data ARepK V2 CTypeSpec = AKCTypeSpec {
      headers :: [FilePath]
    , cName   :: Text
    }

  fromARepKV arep =
    ( AKCTypeSpec{
          headers = arep.headers
        , cName   = arep.cName
        }
    , CTypeSpec{
          hsName = arep.hsName
        , enum   = arep.enum
        }
    )

  toARepKV k v = ACTypeSpec{
      headers = k.headers
    , cName   = k.cName
    , hsName  = v.hsName
    , enum    = v.enum
    }

deriving stock instance Show (ARepK V2 CTypeSpec)

instance Aeson.FromJSON (ARepK V2 CTypeSpec) where
  parseJSON = Aeson.withObject "AKCTypeSpec" $ \o -> do
    akCTypeSpecHeaders <- o .: "headers" >>= listFromJSON
    akCTypeSpecCName   <- o .: "cname"
    return AKCTypeSpec{
        headers = akCTypeSpecHeaders
      , cName   = akCTypeSpecCName
      }

instance Aeson.ToJSON (ARepK V2 CTypeSpec) where
  toJSON key = Aeson.Object $ KM.fromList [
      "headers" .= listToJSON key.headers
    , "cname"   .= key.cName
    ]

type AOCTypeSpec = AOmittable (ARepK V2 CTypeSpec) (ARep V2 CTypeSpec)

fromAOCTypeSpecs ::
     FilePath
  -> [AOCTypeSpec]
  -> ( [BindingSpecReadMsg]
     , Set (Hs.Name Hs.NsTypeConstr)
     , Map C.DeclId [(Set C.HashIncludeArg, Omittable CTypeSpec)]
     )
fromAOCTypeSpecs path =
    fin . foldr auxInsert (Set.empty, [], Map.empty, Set.empty, Map.empty)
  where
    fin ::
         ( Set Text
         , [C.HashIncludeArgMsg]
         , Map C.DeclId (Set C.HashIncludeArg)
         , Set (Hs.Name Hs.NsTypeConstr)
         , Map C.DeclId [(Set C.HashIncludeArg, Omittable CTypeSpec)]
         )
      -> ( [BindingSpecReadMsg]
         , Set (Hs.Name Hs.NsTypeConstr)
         , Map C.DeclId [(Set C.HashIncludeArg, Omittable CTypeSpec)]
         )
    fin (invalids, msgs, conflicts, hsNms, cTypeMap) =
      let invalidErrs = BindingSpecReadInvalidCName path <$> Set.toList invalids
          argErrs = BindingSpecReadHashIncludeArg path <$> msgs
          conflictErrs = [
              BindingSpecReadCTypeConflict path cDeclId header
            | (cDeclId, headers) <- Map.toList conflicts
            , header <- Set.toList headers
            ]
      in  (invalidErrs ++ argErrs ++ conflictErrs, hsNms, cTypeMap)

    auxInsert ::
         AOCTypeSpec
      -> ( Set Text
         , [C.HashIncludeArgMsg]
         , Map C.DeclId (Set C.HashIncludeArg)
         , Set (Hs.Name Hs.NsTypeConstr)
         , Map C.DeclId [(Set C.HashIncludeArg, Omittable CTypeSpec)]
         )
      -> ( Set Text
         , [C.HashIncludeArgMsg]
         , Map C.DeclId (Set C.HashIncludeArg)
         , Set (Hs.Name Hs.NsTypeConstr)
         , Map C.DeclId [(Set C.HashIncludeArg, Omittable CTypeSpec)]
         )
    auxInsert aoCTypeMapping (invalids, msgs, conflicts, hsNms, acc) =
      let (cname, headers, mHsNm, oCTypeSpec) = case aoCTypeMapping of
            ARequire arep ->
              let (k, cTypeSpec) = fromARepKV arep
              in  (k.cName, k.headers, cTypeSpec.hsName, Require cTypeSpec)
            AOmit k -> (k.cName, k.headers, Nothing, Omit)
          (msgs', headers') = bimap ((msgs ++) . concat) Set.fromList $
            unzip (map C.hashIncludeArg headers)
          hsNms' = maybe hsNms (`Set.insert` hsNms) mHsNm
          newV = [(headers', oCTypeSpec)]
      in  case C.parseDeclId cname of
            Nothing ->
              (Set.insert cname invalids, msgs', conflicts, hsNms', acc)
            Just cDeclId ->
              case Map.insertLookupWithKey (const (++)) cDeclId newV acc of
                (Nothing, acc') -> (invalids, msgs', conflicts, hsNms', acc')
                (Just oldV, acc') ->
                  let conflicts' = auxDup cDeclId newV oldV conflicts
                  in  (invalids, msgs', conflicts', hsNms', acc')

    auxDup ::
         C.DeclId
      -> [(Set C.HashIncludeArg, a)]
      -> [(Set C.HashIncludeArg, a)]
      -> Map C.DeclId (Set C.HashIncludeArg)
      -> Map C.DeclId (Set C.HashIncludeArg)
    auxDup cDeclId newV oldV =
      case Set.intersection (mconcat (fst <$> newV)) (mconcat (fst <$> oldV)) of
        commonHeaders
          | Set.null commonHeaders -> id
          | otherwise -> Map.insertWith Set.union cDeclId commonHeaders

toAOCTypeSpecs ::
     (C.DeclId -> C.DeclId -> Ordering)
  -> Map C.DeclId [(Set C.HashIncludeArg, Omittable CTypeSpec)]
  -> [AOCTypeSpec]
toAOCTypeSpecs compareCDeclId cTypeMap = map snd $ List.sortBy aux [
      (cDeclId,) $ case oCTypeSpec of
        Require spec -> ARequire ACTypeSpec{
            headers = map (.path) (Set.toAscList headers)
          , cName   = C.renderDeclId cDeclId
          , hsName  = spec.hsName
          , enum    = spec.enum
          }
        Omit -> AOmit AKCTypeSpec{
            headers = map (.path) (Set.toAscList headers)
          , cName   = C.renderDeclId cDeclId
          }
    | (cDeclId, xs) <- Map.toAscList cTypeMap
    , (headers, oCTypeSpec) <- xs
    ]
  where
    aux :: (C.DeclId, AOCTypeSpec) -> (C.DeclId, AOCTypeSpec) -> Ordering
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

newtype instance ARep V2 CEnumSpec = ACEnumSpec CEnumSpec
  deriving stock (Show)

instance ARepIso V2 CEnumSpec

instance Aeson.FromJSON (ARep V2 CEnumSpec) where
  parseJSON = Aeson.withText "CEnumSpec" $ \t ->
    case Map.lookup t cEnumSpecFromText of
      Just cEnumSpec -> return (ACEnumSpec cEnumSpec)
      Nothing        -> Aeson.parseFail $
        "unknown C enum specification: " ++ Text.unpack t

instance Aeson.ToJSON (ARep V2 CEnumSpec) where
  toJSON (ACEnumSpec cEnumSpec) = Aeson.String (cEnumSpecToText cEnumSpec)

cEnumSpecToText :: CEnumSpec -> Text
cEnumSpecToText = \case
    CEnumOpen   -> "open"
    CEnumClosed -> "closed"

cEnumSpecFromText :: Map Text CEnumSpec
cEnumSpecFromText = Map.fromList [
      (cEnumSpecToText cEnumSpec, cEnumSpec)
    | cEnumSpec <- [minBound..]
    ]

--------------------------------------------------------------------------------

newtype instance ARep V2 (Hs.Name ns) =
    AHsName (Hs.Name ns)
  deriving stock (Show)

instance ARepIso V2 (Hs.Name ns)

instance Hs.SingNamespace ns => Aeson.FromJSON (ARep V2 (Hs.Name ns)) where
  parseJSON = case Hs.singNamespace @ns of
    Hs.SNsTypeConstr ->
      Aeson.withText "Hs.Name Hs.NsTypeConstr" $ \text ->
        case parseHsName (Proxy @Hs.NsTypeConstr) text of
          Left err ->
            Aeson.parseFail $
              "failed parsing type constructor name: " <> show (prettyForTrace err)
          Right hsName ->
            pure $ AHsName hsName
    Hs.SNsConstr ->
      Aeson.withText "Hs.Name Hs.NsConstr" $ \text ->
        case parseHsName (Proxy @Hs.NsConstr) text of
          Left err ->
            Aeson.parseFail $
              "failed parsing data constructor name: " <> show (prettyForTrace err)
          Right hsName ->
            pure $ AHsName hsName
    Hs.SNsVar ->
      Aeson.withText "Hs.Name Hs.NsVar" $ \text ->
        case parseHsName (Proxy @Hs.NsVar) text of
          Left err ->
            Aeson.parseFail $
              "failed parsing variable name: " <> show (prettyForTrace err)
          Right hsName ->
            pure $ AHsName hsName

instance Aeson.ToJSON (ARep V2 (Hs.Name ns)) where
  toJSON (AHsName hsName) = Aeson.String (hsName.text)

--------------------------------------------------------------------------------

data instance ARep V2 HsTypeSpec = AHsTypeSpec {
      hsName    :: Hs.Name Hs.NsTypeConstr
    , hsRep     :: Maybe HsTypeRep
    , instances :: [AOInstanceSpec]
    }
  deriving stock (Show)

instance Aeson.FromJSON (ARep V2 HsTypeSpec) where
  parseJSON = Aeson.withObject "HsTypeSpec" $ \o -> do
    aHsTypeSpecHsName    <- o .:  "hsname"
    aHsTypeSpecHsRep     <- o .:? "representation"
    aHsTypeSpecInstances <- o .:? "instances" .!= []
    return AHsTypeSpec{
        hsName    = fromARep' aHsTypeSpecHsName
      , hsRep     = fromARep' <$> aHsTypeSpecHsRep
      , instances = aHsTypeSpecInstances
      }

instance Aeson.ToJSON (ARep V2 HsTypeSpec) where
  toJSON arep = Aeson.Object . KM.fromList $ catMaybes [
      Just ("hsname" .= toARep' arep.hsName)
    , ("representation" .=) . toARep' <$> arep.hsRep
    , ("instances" .=) <$> omitWhenNull arep.instances
    ]

instance ARepKV V2 HsTypeSpec where
  newtype ARepK V2 HsTypeSpec = AKHsTypeSpec { unwrap :: Hs.Name Hs.NsTypeConstr }

  fromARepKV arep =
    ( AKHsTypeSpec arep.hsName
    , HsTypeSpec{
          hsRep     = arep.hsRep
        , instances = fromAOInstanceSpecs arep.instances
        }
    )

  toARepKV k v = AHsTypeSpec{
      hsName    = k.unwrap
    , hsRep     = v.hsRep
    , instances = toAOInstanceSpecs v.instances
    }

deriving stock instance Show (ARepK V2 HsTypeSpec)

instance Aeson.FromJSON (ARepK V2 HsTypeSpec) where
  parseJSON = fmap (AKHsTypeSpec . fromARep') . Aeson.parseJSON

instance Aeson.ToJSON (ARepK V2 HsTypeSpec) where
  toJSON = Aeson.toJSON . toARep' . (.unwrap)

fromAHsTypeSpecs ::
     FilePath
  -> Set (Hs.Name Hs.NsTypeConstr)
  -> [ARep V2 HsTypeSpec]
  -> ([BindingSpecReadMsg], Map (Hs.Name Hs.NsTypeConstr) HsTypeSpec)
fromAHsTypeSpecs path hsNms = fin . foldr auxInsert (Set.empty, Map.empty)
  where
    fin ::
         ( Set (Hs.Name Hs.NsTypeConstr)
         , Map (Hs.Name Hs.NsTypeConstr) HsTypeSpec )
      -> ([BindingSpecReadMsg], Map (Hs.Name Hs.NsTypeConstr) HsTypeSpec)
    fin (conflicts, hsTypeMap) =
      let conflictErrs = BindingSpecReadHsTypeConflict path <$>
            Set.toList conflicts
          noRefErrs = BindingSpecReadHsTypeNameNoRef path <$>
            Set.toList (Map.keysSet hsTypeMap `Set.difference` hsNms)
      in  (conflictErrs ++ noRefErrs, hsTypeMap)

    auxInsert ::
         ARep V2 HsTypeSpec
      -> (Set (Hs.Name Hs.NsTypeConstr), Map (Hs.Name Hs.NsTypeConstr) HsTypeSpec)
      -> (Set (Hs.Name Hs.NsTypeConstr), Map (Hs.Name Hs.NsTypeConstr) HsTypeSpec)
    auxInsert arep (conflicts, acc) =
      let (k, hsTypeSpec) = fromARepKV arep
      in  case Map.insertLookupWithKey (\_ n _ -> n) k.unwrap hsTypeSpec acc of
            (Nothing, acc') -> (conflicts,                     acc')
            (Just{},  acc') -> (Set.insert k.unwrap conflicts, acc')

toAHsTypeSpecs ::
     Map (Hs.Name Hs.NsTypeConstr) HsTypeSpec
  -> [ARep V2 HsTypeSpec]
toAHsTypeSpecs hsTypeMap = [
      toARepKV (AKHsTypeSpec hsName) spec
    | (hsName, spec) <- Map.toAscList hsTypeMap
    ]

--------------------------------------------------------------------------------

newtype instance ARep V2 HsTypeRep = AHsTypeRep HsTypeRep
  deriving stock (Show)

instance ARepIso V2 HsTypeRep

instance Aeson.FromJSON (ARep V2 HsTypeRep) where
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
        return HsNewtypeRep{
            constructor = fromARep' <$> hsNewtypeRepConstructor
          , field       = fromARep' <$> hsNewtypeRepField
          }

instance Aeson.ToJSON (ARep V2 HsTypeRep) where
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
              ]
    HsTypeRepEmptyData -> Aeson.String "emptydata"
    HsTypeRepTypeAlias -> Aeson.String "typealias"

--------------------------------------------------------------------------------

data instance ARep V2 InstanceSpec = AInstanceSpec {
      clss        :: Inst.TypeClass
    , strategy    :: Maybe Inst.Strategy
    , constraints :: [Inst.Constraint]
    , associated  :: [Inst.AssociatedType]
    }
  deriving stock (Show)

instance Aeson.FromJSON (ARep V2 InstanceSpec) where
  parseJSON = \case
    s@Aeson.String{} -> do
      aInstanceSpecClass <- Aeson.parseJSON s
      let aInstanceSpecStrategy    = Nothing
          aInstanceSpecConstraints = []
          aAssociated              = []
      return AInstanceSpec{
          clss        = fromARep' aInstanceSpecClass
        , strategy    = aInstanceSpecStrategy
        , constraints = aInstanceSpecConstraints
        , associated  = aAssociated
        }
    Aeson.Object o -> do
      aInstanceSpecClass       <- o .:  "class"
      aInstanceSpecStrategy    <- o .:? "strategy"
      aInstanceSpecConstraints <- o .:? "constraints" .!= []
      aInstanceSpecAssociated  <- o .:? "associated" .!= []
      return AInstanceSpec{
          clss        = fromARep' aInstanceSpecClass
        , strategy    = fromARep' <$> aInstanceSpecStrategy
        , constraints = fromARep' <$> aInstanceSpecConstraints
        , associated  = fromARep' <$> aInstanceSpecAssociated
        }
    v -> Aeson.parseFail $
      "expected InstanceSpec String or Object, but encountered " ++ typeOf v

instance Aeson.ToJSON (ARep V2 InstanceSpec) where
  toJSON arep
    | isNothing arep.strategy && null arep.constraints && null arep.associated =
        Aeson.toJSON (toARep' arep.clss)
    | otherwise = Aeson.Object . KM.fromList $ catMaybes [
          Just ("class" .= toARep' arep.clss)
        , ("strategy"    .=) . toARep' <$> arep.strategy
        , ("constraints" .=) . fmap toARep' <$> omitWhenNull arep.constraints
        , ("associated"  .=) . fmap toARep' <$> omitWhenNull arep.associated
        ]

instance ARepKV V2 InstanceSpec where
  newtype ARepK V2 InstanceSpec = AKInstanceSpec { unwrap :: Inst.TypeClass }

  fromARepKV arep =
    ( AKInstanceSpec arep.clss
    , InstanceSpec{
          strategy    = arep.strategy
        , constraints = arep.constraints
        , associated  = arep.associated
        }
    )

  toARepKV k v = AInstanceSpec{
      clss        = k.unwrap
    , strategy    = v.strategy
    , constraints = v.constraints
    , associated  = v.associated
    }

deriving stock instance Show (ARepK V2 InstanceSpec)

instance Aeson.FromJSON (ARepK V2 InstanceSpec) where
  parseJSON = fmap (AKInstanceSpec . fromARep') . Aeson.parseJSON

instance Aeson.ToJSON (ARepK V2 InstanceSpec) where
  toJSON = Aeson.toJSON . toARep' . (.unwrap)

type AOInstanceSpec = AOmittable (ARepK V2 InstanceSpec) (ARep V2 InstanceSpec)

-- duplicates ignored, last value retained
fromAOInstanceSpecs ::
     [AOInstanceSpec]
  -> Map Inst.TypeClass (Omittable InstanceSpec)
fromAOInstanceSpecs xs = Map.fromList . flip map xs $ \case
    ARequire arep -> bimap (.unwrap) Require (fromARepKV arep)
    AOmit    k    -> (k.unwrap, Omit)

toAOInstanceSpecs ::
     Map Inst.TypeClass (Omittable InstanceSpec)
  -> [AOInstanceSpec]
toAOInstanceSpecs instMap = [
      case oInstSpec of
        Require spec -> ARequire $ toARepKV (AKInstanceSpec hsTypeClass) spec
        Omit         -> AOmit (AKInstanceSpec hsTypeClass)
    | (hsTypeClass, oInstSpec) <- Map.toAscList instMap
    ]

--------------------------------------------------------------------------------

newtype instance ARep V2 Inst.TypeClass = ATypeClass Inst.TypeClass
  deriving stock (Show)

instance ARepIso V2 Inst.TypeClass

instance Aeson.FromJSON (ARep V2 Inst.TypeClass) where
  parseJSON = Aeson.withText "TypeClass" $ \t ->
    let s = Text.unpack t
    in  case readMaybe s of
          Just clss -> return (ATypeClass clss)
          Nothing   -> Aeson.parseFail $ "unknown type class: " ++ s

instance Aeson.ToJSON (ARep V2 Inst.TypeClass) where
  toJSON (ATypeClass clss) = Aeson.String $ Text.pack (show clss)

--------------------------------------------------------------------------------

newtype instance ARep V2 Inst.Strategy = AStrategy Inst.Strategy
  deriving stock (Show)

instance ARepIso V2 Inst.Strategy

instance Aeson.FromJSON (ARep V2 Inst.Strategy) where
  parseJSON = Aeson.withText "Strategy" $ \t ->
    case Map.lookup t strategyFromText of
      Just strat -> return (AStrategy strat)
      Nothing    -> Aeson.parseFail $ "unknown strategy: " ++ Text.unpack t

instance Aeson.ToJSON (ARep V2 Inst.Strategy) where
  toJSON (AStrategy strat) = Aeson.String (strategyToText strat)

strategyToText :: Inst.Strategy -> Text
strategyToText = \case
    Inst.HsBindgen -> "hs-bindgen"
    Inst.Newtype   -> "newtype"
    Inst.Stock     -> "stock"

strategyFromText :: Map Text Inst.Strategy
strategyFromText = Map.fromList [
      (strategyToText strat, strat)
    | strat <- [minBound..]
    ]

--------------------------------------------------------------------------------

newtype instance ARep V2 Inst.Constraint = AConstraint Inst.Constraint
  deriving stock (Show)

instance ARepIso V2 Inst.Constraint

instance Aeson.FromJSON (ARep V2 Inst.Constraint) where
  parseJSON = Aeson.withObject "Constraint" $ \o -> do
      constraintClass <- o .: "class"
      extRefModule    <- o .: "hsmodule"
      extRefName      <- o .: "hsname"
      let hsName :: Hs.Name Hs.NsTypeConstr
          hsName = fromARep' extRefName
          -- TODO https://github.com/well-typed/hs-bindgen/issues/423
          --
          -- At the moment, we only handle external references to types types.
          -- Later we may support external references to variables (e.g., in
          -- macros).
          constraintRef = Hs.ExtRef{
              moduleName = fromARep' extRefModule
            , name       = hsName
            }
      return $ AConstraint Inst.Constraint{
          clss = fromARep' constraintClass
        , ref  = constraintRef
        }

instance Aeson.ToJSON (ARep V2 Inst.Constraint) where
  toJSON (AConstraint constraint) = Aeson.object [
        "class"    .= toARep' constraint.clss
      , "hsmodule" .= toARep' constraint.ref.moduleName
        -- TODO https://github.com/well-typed/hs-bindgen/issues/423
        --
        -- At the moment, we only handle external references to types types.
        -- Later we may support external references to variables (e.g., in
        -- macros).
      , "hsname"   .= toARep' constraint.ref.name
      ]

--------------------------------------------------------------------------------

newtype instance ARep V2 Inst.AssociatedType = AAssociatedType Inst.AssociatedType
  deriving stock (Show)

instance ARepIso V2 Inst.AssociatedType

instance Aeson.FromJSON (ARep V2 Inst.AssociatedType) where
  parseJSON = Aeson.withObject "AssociatedType" $ \o -> do
      name            <- o .: "type"
      extRefModule    <- o .: "hsmodule"
      extRefName      <- o .: "hsname"
      let hsName :: Hs.Name Hs.NsTypeConstr
          hsName = fromARep' extRefName
          ref = Hs.ExtRef{
              moduleName = fromARep' extRefModule
            , name       = hsName
            }
      return $ AAssociatedType Inst.AssociatedTypeSyn{
          name = fromARep' name
        , ref  = ref
        }

instance Aeson.ToJSON (ARep V2 Inst.AssociatedType) where
  toJSON (AAssociatedType typ) = Aeson.object [
        "type"     .= toARep' typ.name
      , "hsmodule" .= toARep' typ.ref.moduleName
      , "hsname"   .= toARep' typ.ref.name
      ]

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- 'List.lookup' using a predicate
lookupBy :: (a -> Bool) -> [(a, b)] -> Maybe b
lookupBy p = fmap snd . List.find (p . fst)

data MangleCandidateError =
    MangleCandidateCouldNotMangleError Text
  | MangleCandidateParseError          MangleCandidate.ParseCandidateError

instance PrettyForTrace MangleCandidateError where
  prettyForTrace = \case
    MangleCandidateCouldNotMangleError x -> PP.hsep [
        "Could not mangle:"
      , PP.text x
      ]
    MangleCandidateParseError err -> PP.hsep [
        "Name does not adhere to naming rules:"
      , prettyForTrace err
      ]

parseHsName :: forall ns.
     Hs.SingNamespace ns
  => Proxy ns
  -> Text
  -> Either MangleCandidateError (Hs.Name ns)
parseHsName _ hsNameCandidate =
    case MangleCandidate.parseCandidate mangleCandidateConfig hsNameCandidate of
      Nothing ->
        Left $ MangleCandidateCouldNotMangleError hsNameCandidate
      Just (Left  err) ->
        Left $ MangleCandidateParseError err
      Just (Right hsName) ->
        Right $ hsName
  where
    mangleCandidateConfig :: MangleCandidate Maybe
    mangleCandidateConfig =
      MangleCandidate.mangleCandidateDefault
        -- Names in binding specs are supplied by the user and are expected to
        -- be valid Haskell identifiers. We therefore do not check reserved
        -- names: the user is trusted to avoid them, and checking would reject
        -- names like @type@ that are legitimately used in external packages.
        & #reservedNames .~ mempty

{-------------------------------------------------------------------------------
  Conversion V1 -> V2
-------------------------------------------------------------------------------}

class FromV1 a b where
  fromV1 :: a -> b

instance FromV1 (V1.BindingSpec header) (BindingSpec header) where
  fromV1 (V1.BindingSpec moduleName cTypes hsTypes) = BindingSpec {
      moduleName = moduleName
    , cTypes     = Map.map (fmap (second (fmap fromV1))) cTypes
    , hsTypes    = Map.map fromV1 hsTypes
    }

instance FromV1 V1.CTypeSpec CTypeSpec where
  fromV1 (V1.CTypeSpec hsName enum) = CTypeSpec {
      hsName = hsName
    , enum   = fmap fromV1 enum
    }

instance FromV1 V1.CEnumSpec CEnumSpec where
  fromV1 = \case
    V1.CEnumOpen   -> CEnumOpen
    V1.CEnumClosed -> CEnumClosed

instance FromV1 V1.HsTypeSpec HsTypeSpec where
  fromV1 spec@(V1.HsTypeSpec hsRep instances) = HsTypeSpec {
        hsRep     = fmap fromV1 hsRep
      , instances = Map.mapWithKey go instances
      }
    where
      go :: Inst.TypeClass -> Omittable V1.InstanceSpec -> Omittable InstanceSpec
      go cls = fmap (fromV1_InstanceSpec cls (V1.hsSpecFFIType spec))

instance FromV1 V1.HsTypeRep HsTypeRep where
  fromV1 = \case
    V1.HsTypeRepRecord    rep -> HsTypeRepRecord    (fromV1 rep)
    V1.HsTypeRepNewtype   rep -> HsTypeRepNewtype   (fromV1 rep)
    V1.HsTypeRepEmptyData     -> HsTypeRepEmptyData
    V1.HsTypeRepTypeAlias     -> HsTypeRepTypeAlias

instance FromV1 V1.HsRecordRep HsRecordRep where
  fromV1 (V1.HsRecordRep constructor fields) = HsRecordRep {
      constructor = constructor
    , fields      = fields
    }

instance FromV1 V1.HsNewtypeRep HsNewtypeRep where
  fromV1 (V1.HsNewtypeRep constructor field _ffiType) = HsNewtypeRep {
      constructor = constructor
    , field       = field
    }

-- | Move the V1 @ffitype@ field into the V2 @HasFFIType@ class instance as a V2
-- @FFIType@ associated type synonym.
fromV1_InstanceSpec ::
     Inst.TypeClass
  -> Maybe V1.HsFFIType
  -> V1.InstanceSpec
  -> InstanceSpec
fromV1_InstanceSpec cls ffiTypeMay (V1.InstanceSpec strategy constraints) =
    InstanceSpec {
        strategy    = strategy
      , constraints = constraints
      , associated  =
          if cls == Inst.HasFFIType
          then toList (mkFFIType <$> ffiTypeMay)
          else []
      }
  where
    mkFFIType :: V1.HsFFIType -> Inst.AssociatedType
    mkFFIType ffiType = Inst.FFIType ffiType.unwrap
