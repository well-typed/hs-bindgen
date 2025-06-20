-- | Binding specification
--
-- Intended for qualified import.
--
-- > import HsBindgen.BindingSpec qualified as BindingSpec
module HsBindgen.BindingSpec (
    -- * Types
    BindingSpec(..)
  , UnresolvedBindingSpec
  , ResolvedBindingSpec
  , CSpelling(..)
  , Omittable(..)
  , TypeSpec(..)
  , defaultTypeSpec
    -- ** Instances
  , InstanceSpec(..)
  , StrategySpec(..)
  , ConstraintSpec(..)
    -- ** Exceptions
  , ReadBindingSpecException(..)
  , WriteBindingSpecException(..)
  , MergeBindingSpecException(..)
  , BindingSpecException(..)
  , BindingSpecExceptions(..)
    -- * API
  , empty
  , load
  , lookupTypeSpec
    -- ** YAML/JSON
  , readFile
  , readFileJson
  , readFileYaml
  , encodeJson
  , encodeYaml
  , writeFile
  , writeFileJson
  , writeFileYaml
    -- ** Header resolution
  , resolve
    -- ** Merging
  , merge
  ) where

import Control.Applicative (asum)
import Control.Exception (Exception(..))
import Control.Monad ((<=<))
import Control.Tracer (Tracer)
import Data.Aeson ((.=), (.:), (.:?), (.!=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as Aeson
import Data.ByteString qualified as BSS
import Data.ByteString.Lazy qualified as BSL
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy(Proxy))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Typeable (Typeable, typeRep)
import Data.Yaml qualified as Yaml
import Data.Yaml.Internal qualified
import Data.Yaml.Pretty qualified
import Prelude hiding (readFile, writeFile)

import Clang.Args
import Clang.Paths
import HsBindgen.Clang.Args (ExtraClangArgsLog)
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Language.Haskell
import HsBindgen.Orphans ()
import HsBindgen.Resolve
import HsBindgen.Util.Tracer (TraceWithCallStack)

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Binding specification
--
-- The @header@ type parameter determines the representation of header paths.
-- See 'UnresolvedBindingSpec' and 'ResolvedBindingSpec'.
newtype BindingSpec header = BindingSpec {
      -- | Type specifications
      --
      -- C types are identified using a 'CSpelling' and a set of headers where
      -- the type may be (transitively) declared.  For a given 'CSpelling', the
      -- corresponding sets of headers are disjoint.  The type is therefore
      -- equivalent to @'Map' 'CSpelling' ('Map' header 'Omittable Type')@, but
      -- this type is used as an optimization.  In most cases, each 'CSpelling'
      -- is mapped to a singleton list with a singleton set of headers.
      bindingSpecTypes :: Map CSpelling [(Set header, Omittable TypeSpec)]
    }
  deriving stock (Eq, Generic, Show)

-- | Binding specification with unresolved headers
type UnresolvedBindingSpec = BindingSpec CHeaderIncludePath

-- | Binding specification with resolved headers
type ResolvedBindingSpec = BindingSpec SourcePath

--------------------------------------------------------------------------------

-- | C spelling
--
-- A value must specify @struct@, @union@, or @enum@ when required.
--
-- Examples: @int8_t@, @struct tm@
newtype CSpelling = CSpelling { getCSpelling :: Text }
  -- 'Show' instance valid due to 'IsString' instance
  deriving newtype (Eq, IsString, Ord, Show)

deriving newtype instance Aeson.FromJSON CSpelling

deriving newtype instance Aeson.ToJSON CSpelling

--------------------------------------------------------------------------------

-- | Wrapper for types that may be omitted
--
-- This type is isomorphic with 'Maybe'.
--
-- In general, the following conventions are followed:
--
-- * If something is specified, it is required.  It is an error if @hs-bindgen@
--   is unable to satisfy the requirement.
-- * If something is omitted, then @hs-bindgen@ does /not/ generate the
--   corresponding code.  Use of something that is omitted is an error.
-- * If nothing is specified, @hs-bindgen@ generates code using defaults.  This
--   case is /not/ represented by 'Omittable'.
data Omittable a =
    Require a
  | Omit
  deriving stock (Eq, Generic, Show)

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
  deriving stock (Eq, Generic, Show)

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
  deriving stock (Eq, Generic, Show)

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
  deriving stock (Eq, Generic, Show)

{-------------------------------------------------------------------------------
  Types: Exceptions
-------------------------------------------------------------------------------}

-- | Failed to load binding specification file
data ReadBindingSpecException =
    -- | Unknown file extension
    ReadBindingSpecUnknownExtension FilePath
  | -- | Aeson parsing error
    ReadBindingSpecAesonError FilePath String
  | -- | YAML parsing error
    ReadBindingSpecYamlError FilePath Yaml.ParseException
  | -- | YAML parsing warnings (which should be treated like errors)
    ReadBindingSpecYamlWarning FilePath [Data.Yaml.Internal.Warning]
    -- | Multiple entries for the same C name and header in the same file
  | ReadBindingSpecConflict FilePath (Set (CSpelling, CHeaderIncludePath))
  deriving stock (Show)

instance Exception ReadBindingSpecException where
  displayException = \case
    ReadBindingSpecUnknownExtension path -> "unknown extension: " ++ path
    ReadBindingSpecAesonError path err ->
      "error parsing JSON: " ++ path ++ ": " ++ err
    ReadBindingSpecYamlError path err -> unlines [
        "error parsing YAML: " ++ path
      , Yaml.prettyPrintParseException err
      ]
    ReadBindingSpecYamlWarning path warnings ->
      unlines $
          ("duplicate keys in YAML file: " ++ path)
        : [ "  " ++ Aeson.formatPath jsonPath
          | Data.Yaml.Internal.DuplicateKey jsonPath <- warnings
          ]
    ReadBindingSpecConflict path conflicts ->
      unlines $
          ( "multiple entries for same C name and header: "
              ++ path
          )
        : [ "  " ++ Text.unpack (getCSpelling cspelling)
              ++ ' ' : getCHeaderIncludePath header
          | (cspelling, header) <- Set.toAscList conflicts
          ]

--------------------------------------------------------------------------------

-- | Failed to write binding specification file
newtype WriteBindingSpecException =
    -- | Unknown file extension
    WriteBindingSpecUnknownExtension FilePath
  deriving stock (Show)

instance Exception WriteBindingSpecException where
  displayException = \case
    WriteBindingSpecUnknownExtension path -> "unknown extension: " ++ path

--------------------------------------------------------------------------------

-- | Failed to merge binding specifications
newtype MergeBindingSpecException =
    -- | Multiple binding specifications for the same C name and header
    MergeBindingSpecConflict (Set CSpelling)
  deriving stock (Show)

instance Exception MergeBindingSpecException where
  displayException = \case
    MergeBindingSpecConflict cspellings->
      unlines $
          "conflicting binding specifications for same C name and header:"
        : [ "  " ++ Text.unpack (getCSpelling cspelling)
          | cspelling <- Set.toAscList cspellings
          ]

--------------------------------------------------------------------------------

-- | Failed loading, resolving, or merging binding specifications
data BindingSpecException =
    ReadBindingSpecException  ReadBindingSpecException
  | MergeBindingSpecException MergeBindingSpecException
  deriving stock (Show)

instance Exception BindingSpecException where
  displayException = \case
    ReadBindingSpecException  e -> displayException e
    MergeBindingSpecException e -> displayException e

--------------------------------------------------------------------------------

-- | Failed loading, resolving, or merging binding specifications
newtype BindingSpecExceptions = BindingSpecExceptions [BindingSpecException]
  deriving stock (Show)

instance Exception BindingSpecExceptions where
  displayException (BindingSpecExceptions es) =
    unlines $ map displayException es

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Empty binding specification
empty :: BindingSpec header
empty = BindingSpec {
      bindingSpecTypes = Map.empty
    }

-- | Load, resolve, and merge binding specifications
--
-- The format is determined by filename extension.
load ::
     Tracer IO (TraceWithCallStack ExtraClangArgsLog)
  -> ClangArgs
  -> [FilePath]
  -> IO (Set ResolveHeaderException, ResolvedBindingSpec)
load tracer args paths = throwExceptions =<< do
    (errs, uspec) <-
      first (map ReadBindingSpecException) . partitionEithers
        <$> mapM readFile paths
    (resolveErrs, specs) <-
      first Set.unions . unzip <$> mapM (resolve tracer args) uspec
    return $ case first MergeBindingSpecException (merge specs) of
      Right spec
        | null errs -> Right (resolveErrs, spec)
        | otherwise -> Left $ BindingSpecExceptions errs
      Left mergeErr -> Left $ BindingSpecExceptions (errs ++ [mergeErr])
  where
    throwExceptions :: Either BindingSpecExceptions x -> IO x
    throwExceptions = either throwIO return

-- | Lookup the 'TypeSpec' associated with a C name spelling where there is at
-- least one header in common with the specified set
lookupTypeSpec ::
     CSpelling
  -> Set SourcePath
  -> ResolvedBindingSpec
  -> Maybe (Omittable TypeSpec)
lookupTypeSpec cspelling headers =
    fmap snd . List.find (not . Set.disjoint headers . fst)
      <=< Map.lookup cspelling . bindingSpecTypes

{-------------------------------------------------------------------------------
  API: YAML/JSON
-------------------------------------------------------------------------------}

-- | Read a binding specification from a file
--
-- The format is determined by the filename extension.
readFile ::
     FilePath
  -> IO (Either ReadBindingSpecException UnresolvedBindingSpec)
readFile path
    | ".yaml" `List.isSuffixOf` path = readFileYaml path
    | ".json" `List.isSuffixOf` path = readFileJson path
    | otherwise = return $ Left (ReadBindingSpecUnknownExtension path)

-- | Read a binding specification from a JSON file
readFileJson ::
     FilePath
  -> IO (Either ReadBindingSpecException UnresolvedBindingSpec)
readFileJson path = do
    ees <- Aeson.eitherDecodeFileStrict' path
    return $ case ees of
      Right spec -> fromABindingSpec path spec
      Left err   -> Left (ReadBindingSpecAesonError path err)

-- | Read a binding specification from a YAML file
readFileYaml ::
     FilePath
  -> IO (Either ReadBindingSpecException UnresolvedBindingSpec)
readFileYaml path = do
    eews <- Yaml.decodeFileWithWarnings path
    return $ case eews of
      Right ([], spec) -> fromABindingSpec path spec
      Right (warnings, _) -> Left (ReadBindingSpecYamlWarning path warnings)
      Left err -> Left (ReadBindingSpecYamlError path err)

-- | Encode a binding specification as JSON
encodeJson :: UnresolvedBindingSpec -> BSL.ByteString
encodeJson = encodeJson' . toABindingSpec

-- | Encode a binding specification as YAML
encodeYaml :: UnresolvedBindingSpec -> BSS.ByteString
encodeYaml = encodeYaml' . toABindingSpec

-- | Write a binding specification to a file
--
-- The format is determined by the filename extension.
writeFile ::
     FilePath
  -> UnresolvedBindingSpec
  -> IO (Either WriteBindingSpecException ())
writeFile path spec
    | ".yaml" `List.isSuffixOf` path = Right <$> writeFileYaml path spec
    | ".json" `List.isSuffixOf` path = Right <$> writeFileJson path spec
    | otherwise = return $ Left (WriteBindingSpecUnknownExtension path)

-- | Write a binding specification to a JSON file
writeFileJson :: FilePath -> UnresolvedBindingSpec -> IO ()
writeFileJson path = BSL.writeFile path . encodeJson' . toABindingSpec

-- | Write a binding specification to a YAML file
writeFileYaml :: FilePath -> UnresolvedBindingSpec -> IO ()
writeFileYaml path = BSS.writeFile path . encodeYaml' . toABindingSpec

{-------------------------------------------------------------------------------
  API: Header resolution
-------------------------------------------------------------------------------}

-- | Resolve headers in a binding specification
resolve ::
     Tracer IO (TraceWithCallStack ExtraClangArgsLog)
  -> ClangArgs
  -> UnresolvedBindingSpec
  -> IO (Set ResolveHeaderException, ResolvedBindingSpec)
resolve tracer args uSpec = do
    let types = bindingSpecTypes uSpec
        cPaths = Set.toAscList . mconcat $ fst <$> mconcat (Map.elems types)
    (errs, headerMap) <- bimap Set.fromList Map.fromList . partitionEithers
      <$> mapM
            (\cPath -> fmap (cPath,) <$> resolveHeader' tracer args cPath)
            cPaths
    let resolveSet :: Set CHeaderIncludePath -> Set SourcePath
        resolveSet =
            Set.fromList
          . mapMaybe (`Map.lookup` headerMap)
          . Set.toList
        resolve1 :: (Set CHeaderIncludePath, a) -> Maybe (Set SourcePath, a)
        resolve1 (sU, x) = case resolveSet sU of
          sR
            | Set.null sR -> Nothing
            | otherwise   -> Just (sR, x)
        resolve' :: [(Set CHeaderIncludePath, a)] -> Maybe [(Set SourcePath, a)]
        resolve' lU = case mapMaybe resolve1 lU of
          lR
            | null lR   -> Nothing
            | otherwise -> Just lR
        rSpec = BindingSpec {
            bindingSpecTypes = Map.mapMaybe resolve' types
          }
    return (errs, rSpec)

{-------------------------------------------------------------------------------
  API: Merging
-------------------------------------------------------------------------------}

-- | Merge binding specifications
merge ::
     [ResolvedBindingSpec]
  -> Either MergeBindingSpecException ResolvedBindingSpec
merge = \case
    []   -> Right empty
    x:xs -> do
      bindingSpecTypes <- mergeTypes Set.empty (bindingSpecTypes x) $
        concatMap (Map.toList . bindingSpecTypes) xs
      return BindingSpec{..}
  where
    mergeTypes ::
         Set CSpelling
      -> Map CSpelling [(Set SourcePath, a)]
      -> [(CSpelling, [(Set SourcePath, a)])]
      -> Either MergeBindingSpecException (Map CSpelling [(Set SourcePath, a)])
    mergeTypes dupSet acc = \case
      []
        | Set.null dupSet -> Right acc
        | otherwise       -> Left $ MergeBindingSpecConflict dupSet
      (cspelling, rs):ps ->
        case Map.insertLookupWithKey (const (++)) cspelling rs acc of
          (Nothing, acc') -> mergeTypes dupSet acc' ps
          (Just ls, acc') ->
            let lHeaders = Set.unions $ fst <$> ls
                rHeaders = Set.unions $ fst <$> rs
                iHeaders = Set.intersection lHeaders rHeaders
            in  if Set.null iHeaders
                  then mergeTypes dupSet acc' ps
                  else mergeTypes (Set.insert cspelling dupSet) acc' ps

{-------------------------------------------------------------------------------
  Auxiliary: Specification files
-------------------------------------------------------------------------------}

data AOmittable a = ARequire a | AOmit a
  deriving stock Show

instance Aeson.FromJSON a => Aeson.FromJSON (AOmittable a) where
  parseJSON = \case
    Aeson.Object o | KM.size o == 1 && KM.member "omit" o ->
      AOmit <$> o .: "omit"
    v -> ARequire <$> Aeson.parseJSON v

instance Aeson.ToJSON a => Aeson.ToJSON (AOmittable a) where
  toJSON = \case
    ARequire x -> Aeson.toJSON x
    AOmit    x -> Aeson.object ["omit" .= x]

--------------------------------------------------------------------------------

newtype ABindingSpec = ABindingSpec {
      aBindingSpecTypes :: [AOmittable ATypeSpecMapping]
    }
  deriving stock Show

instance Aeson.FromJSON ABindingSpec where
  parseJSON = Aeson.withObject "ABindingSpec" $ \o -> do
    aBindingSpecTypes <- o .: "types"
    return ABindingSpec{..}

instance Aeson.ToJSON ABindingSpec where
  toJSON ABindingSpec{..} = Aeson.object [
    "types" .= aBindingSpecTypes
    ]

--------------------------------------------------------------------------------

data ATypeSpecMapping = ATypeSpecMapping {
      aTypeSpecMappingHeaders    :: [CHeaderIncludePath]
    , aTypeSpecMappingCName      :: CSpelling
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
  -> Either ReadBindingSpecException UnresolvedBindingSpec
fromABindingSpec path ABindingSpec{..} = do
    bindingSpecTypes <- mkTypeMap aBindingSpecTypes
    return BindingSpec{..}
  where
    mkTypeMap ::
         [AOmittable ATypeSpecMapping]
      -> Either
           ReadBindingSpecException
           (Map CSpelling [(Set CHeaderIncludePath, Omittable TypeSpec)])
    mkTypeMap = mkTypeMapErr . foldr mkTypeMapInsert (Map.empty, Map.empty)

    mkTypeMapErr ::
         (Map CSpelling (Set CHeaderIncludePath), a)
      -> Either ReadBindingSpecException a
    mkTypeMapErr (dupMap, x)
      | Map.null dupMap = Right x
      | otherwise = Left . ReadBindingSpecConflict path $ Set.fromList [
            (cspelling, header)
          | (cspelling, headers) <- Map.toList dupMap
          , header <- Set.toList headers
          ]

    mkTypeMapInsert ::
         AOmittable ATypeSpecMapping
      -> ( Map CSpelling (Set CHeaderIncludePath)
         , Map CSpelling [(Set CHeaderIncludePath, Omittable TypeSpec)]
         )
      -> ( Map CSpelling (Set CHeaderIncludePath)
         , Map CSpelling [(Set CHeaderIncludePath, Omittable TypeSpec)]
         )
    mkTypeMapInsert aoTypeMapping (dupMap, accMap) =
      let (cspelling, headers, oTypeSpec) = case aoTypeMapping of
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
          newV = [(Set.fromList headers, oTypeSpec)]
          x = Map.insertLookupWithKey (const (++)) cspelling newV accMap
      in  case x of
            (Nothing,   accMap') -> (dupMap, accMap')
            (Just oldV, accMap') ->
              (mkTypeMapDup cspelling newV oldV dupMap, accMap')

    mkTypeMapDup ::
         CSpelling
      -> [(Set CHeaderIncludePath, a)]
      -> [(Set CHeaderIncludePath, a)]
      -> Map CSpelling (Set CHeaderIncludePath)
      -> Map CSpelling (Set CHeaderIncludePath)
    mkTypeMapDup cspelling newV oldV dupMap =
      let commonHeaders =
            Set.intersection (mconcat (fst <$> newV)) (mconcat (fst <$> oldV))
      in  if Set.null commonHeaders
            then dupMap
            else Map.insertWith Set.union cspelling commonHeaders dupMap

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
    aBindingSpecTypes :: [AOmittable ATypeSpecMapping]
    aBindingSpecTypes = [
        case oType of
          Require TypeSpec{..} -> ARequire ATypeSpecMapping {
              aTypeSpecMappingHeaders    = Set.toAscList headers
            , aTypeSpecMappingCName      = cspelling
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
              aTypeSpecMappingHeaders    = Set.toAscList headers
            , aTypeSpecMappingCName      = cspelling
            , aTypeSpecMappingModule     = Nothing
            , aTypeSpecMappingIdentifier = Nothing
            , aTypeSpecMappingInstances  = []
            }
      | (cspelling, xs) <- Map.toAscList bindingSpecTypes
      , (headers, oType) <- xs
      ]

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
      "omit"        -> 0  -- Omittable:1
      "types"       -> 1  -- ABindingSpec:1
      "class"       -> 2  -- AInstanceSpecMapping:1, AConstraintSpec:1
      "headers"     -> 3  -- ATypeSpecMapping:1
      "cname"       -> 4  -- ATypeSpecMapping:2
      "module"      -> 5  -- ATypeSpecMapping:3, AConstraintSpec:2
      "identifier"  -> 6  -- ATypeSpecMapping:4, AConstraintSpec:3
      "instances"   -> 7  -- ATypeSpecMapping:5
      "strategy"    -> 8  -- AInstanceSpecMapping:2
      "constraints" -> 9  -- AInstanceSpecMapping:3
      key -> panicPure $ "Unknown key: " ++ show key

{-------------------------------------------------------------------------------
  Auxiliary: Aeson helpers
-------------------------------------------------------------------------------}

-- | Omit empty lists, for use with 'objectWithOptionalFields' and '(.=?)'
omitWhenNull :: [a] -> Maybe [a]
omitWhenNull xs
    | null xs   = Nothing
    | otherwise = Just xs

-- | Convert list to JSON, with special case for the singleton list
--
-- This results in format that is somewhat more friendly for human consumption.
-- It can however not be used for lists-of-lists.
--
-- See also 'listFromJSON'.
listToJSON :: Aeson.ToJSON a => [a] -> Aeson.Value
listToJSON [x] = Aeson.toJSON x
listToJSON xs  = Aeson.toJSON xs

-- | Inverse to 'listToJSON'
listFromJSON :: forall a.
     (Aeson.FromJSON a, Typeable a)
  => Aeson.Value
  -> Aeson.Parser [a]
listFromJSON value = asum [
      Aeson.withArray (show (typeRep (Proxy @[a]))) parseList value
    , parseSingleton
    ]
  where
    parseList :: Aeson.Array -> Aeson.Parser [a]
    parseList = mapM Aeson.parseJSON . toList

    parseSingleton :: Aeson.Parser [a]
    parseSingleton = List.singleton <$> Aeson.parseJSON value

-- | 'Aeson.Value' constructor name, for use in error messages
typeOf :: Aeson.Value -> String
typeOf = \case
    Aeson.Object{} -> "Object"
    Aeson.Array{}  -> "Array"
    Aeson.String{} -> "String"
    Aeson.Number{} -> "Number"
    Aeson.Bool{}   -> "Bool"
    Aeson.Null     -> "Null"
