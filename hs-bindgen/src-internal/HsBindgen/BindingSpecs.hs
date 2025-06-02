-- | Binding specifications
--
-- Intended for qualified import.
--
-- > import HsBindgen.BindingSpecs qualified as BindingSpecs
module HsBindgen.BindingSpecs (
    -- * Types
    BindingSpecs(..)
  , UnresolvedBindingSpecs
  , ResolvedBindingSpecs
  , Omittable(..)
  , TypeSpec(..)
  , defaultTypeSpec
    -- ** Instances
  , InstanceSpec(..)
  , StrategySpec(..)
  , ConstraintSpec(..)
    -- ** Exceptions
  , ReadBindingSpecsException(..)
  , WriteBindingSpecsException(..)
  , MergeBindingSpecsException(..)
  , BindingSpecsException(..)
  , BindingSpecsExceptions(..)
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
import Clang.CNameSpelling
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

-- | Binding specifications
--
-- The @header@ type parameter determines the representation of header paths.
-- See 'UnresolvedBindingSpecs' and 'ResolvedBindingSpecs'.
newtype BindingSpecs header = BindingSpecs {
      -- | Type specifications
      --
      -- C types are identified using a 'CNameSpelling' and a set of headers
      -- where the type may be (transitively) declared.  For a given
      -- 'CNameSpelling', the corresponding sets of headers are disjoint.  The
      -- type is therefore equivalent to
      -- @'Map' 'CNameSpelling' ('Map' header 'Omittable Type')@, but this type
      -- is used as an optimization.  In most cases, each 'CNameSpelling' is
      -- mapped to a singleton list with a singleton set of headers.
      bindingSpecsTypes :: Map CNameSpelling [(Set header, Omittable TypeSpec)]
    }
  deriving stock (Eq, Generic, Show)

-- | Binding specifications with unresolved headers
type UnresolvedBindingSpecs = BindingSpecs CHeaderIncludePath

-- | Binding specifications with resolved headers
type ResolvedBindingSpecs = BindingSpecs SourcePath

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

-- | Binding specifications for a C type
data TypeSpec = TypeSpec {
      -- | Haskell module
      typeSpecModule :: Maybe HsModuleName

    , -- | Haskell identifier
      typeSpecIdentifier :: Maybe HsIdentifier

    , -- | Instance specifications
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

-- | Instance specifications
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

-- | Failed to load binding specifications file
data ReadBindingSpecsException =
    -- | Unknown file extension
    ReadBindingSpecsUnknownExtension FilePath
  | -- | Aeson parsing error
    ReadBindingSpecsAesonError FilePath String
  | -- | YAML parsing error
    ReadBindingSpecsYamlError FilePath Yaml.ParseException
  | -- | YAML parsing warnings (which should be treated like errors)
    ReadBindingSpecsYamlWarning FilePath [Data.Yaml.Internal.Warning]
    -- | Multiple specifications for the same C name and header in the same file
  | ReadBindingSpecsConflict
      FilePath
      (Set (CNameSpelling, CHeaderIncludePath))
  deriving stock (Show)

instance Exception ReadBindingSpecsException where
  displayException = \case
    ReadBindingSpecsUnknownExtension path -> "unknown extension: " ++ path
    ReadBindingSpecsAesonError path err ->
      "error parsing JSON: " ++ path ++ ": " ++ err
    ReadBindingSpecsYamlError path err -> unlines [
        "error parsing YAML: " ++ path
      , Yaml.prettyPrintParseException err
      ]
    ReadBindingSpecsYamlWarning path warnings ->
      unlines $
          ("duplicate keys in YAML file: " ++ path)
        : [ "  " ++ Aeson.formatPath jsonPath
          | Data.Yaml.Internal.DuplicateKey jsonPath <- warnings
          ]
    ReadBindingSpecsConflict path conflicts ->
      unlines $
          ( "multiple specifications for same C name and header: "
              ++ path
          )
        : [ "  " ++ Text.unpack (getCNameSpelling cname)
              ++ ' ' : getCHeaderIncludePath header
          | (cname, header) <- Set.toAscList conflicts
          ]

--------------------------------------------------------------------------------

-- | Failed to write binding specifications file
newtype WriteBindingSpecsException =
    -- | Unknown file extension
    WriteBindingSpecsUnknownExtension FilePath
  deriving stock (Show)

instance Exception WriteBindingSpecsException where
  displayException = \case
    WriteBindingSpecsUnknownExtension path -> "unknown extension: " ++ path

--------------------------------------------------------------------------------

-- | Failed to merge binding specifications
newtype MergeBindingSpecsException =
    -- | Multiple binding specifications for the same C name and header
    MergeBindingSpecsConflict (Set CNameSpelling)
  deriving stock (Show)

instance Exception MergeBindingSpecsException where
  displayException = \case
    MergeBindingSpecsConflict cnames ->
      unlines $
          "conflicting binding specifications for same C name and header:"
        : [ "  " ++ Text.unpack (getCNameSpelling cname)
          | cname <- Set.toAscList cnames
          ]

--------------------------------------------------------------------------------

-- | Failed loading, resolving, or merging binding specifications
data BindingSpecsException =
    ReadBindingSpecsException  ReadBindingSpecsException
  | MergeBindingSpecsException MergeBindingSpecsException
  deriving stock (Show)

instance Exception BindingSpecsException where
  displayException = \case
    ReadBindingSpecsException  e -> displayException e
    MergeBindingSpecsException e -> displayException e

--------------------------------------------------------------------------------

-- | Failed loading, resolving, or merging binding specifications
newtype BindingSpecsExceptions = BindingSpecsExceptions [BindingSpecsException]
  deriving stock (Show)

instance Exception BindingSpecsExceptions where
  displayException (BindingSpecsExceptions es) =
    unlines $ map displayException es

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Empty binding specifications
empty :: BindingSpecs header
empty = BindingSpecs {
      bindingSpecsTypes = Map.empty
    }

-- | Load, resolve, and merge binding specifications
--
-- The format is determined by filename extension.
load ::
     Tracer IO (TraceWithCallStack ExtraClangArgsLog)
  -> ClangArgs
  -> [FilePath]
  -> IO
       ( Either
           BindingSpecsExceptions
           (Set ResolveHeaderException, ResolvedBindingSpecs)
       )
load tracer args paths = do
    (errs, uspecs) <-
      first (map ReadBindingSpecsException) . partitionEithers
        <$> mapM readFile paths
    (resolveErrs, specss) <-
      first Set.unions . unzip <$> mapM (resolve tracer args) uspecs
    return $ case first MergeBindingSpecsException (merge specss) of
      Right specs
        | null errs -> Right (resolveErrs, specs)
        | otherwise -> Left $ BindingSpecsExceptions errs
      Left mergeErr -> Left $ BindingSpecsExceptions (errs ++ [mergeErr])

-- | Lookup the 'TypeSpec' associated with a C name spelling where there is at
-- least one header in common with the specified set
lookupTypeSpec ::
     CNameSpelling
  -> Set SourcePath
  -> ResolvedBindingSpecs
  -> Maybe (Omittable TypeSpec)
lookupTypeSpec cname headers =
    fmap snd . List.find (not . Set.disjoint headers . fst)
      <=< Map.lookup cname . bindingSpecsTypes

{-------------------------------------------------------------------------------
  API: YAML/JSON
-------------------------------------------------------------------------------}

-- | Read binding specifications from a file
-- The format is determined by the filename extension.
readFile ::
     FilePath
  -> IO (Either ReadBindingSpecsException UnresolvedBindingSpecs)
readFile path
    | ".yaml" `List.isSuffixOf` path = readFileYaml path
    | ".json" `List.isSuffixOf` path = readFileJson path
    | otherwise = return $ Left (ReadBindingSpecsUnknownExtension path)

-- | Read binding specifications from a JSON file
readFileJson ::
     FilePath
  -> IO (Either ReadBindingSpecsException UnresolvedBindingSpecs)
readFileJson path = do
    ees <- Aeson.eitherDecodeFileStrict' path
    return $ case ees of
      Right specs -> fromABindingSpecs path specs
      Left err    -> Left (ReadBindingSpecsAesonError path err)

-- | Read binding specifications from a YAML file
readFileYaml ::
     FilePath
  -> IO (Either ReadBindingSpecsException UnresolvedBindingSpecs)
readFileYaml path = do
    eews <- Yaml.decodeFileWithWarnings path
    return $ case eews of
      Right ([], specs) -> fromABindingSpecs path specs
      Right (warnings, _) -> Left (ReadBindingSpecsYamlWarning path warnings)
      Left err -> Left (ReadBindingSpecsYamlError path err)

-- | Encode binding specifications as JSON
encodeJson :: UnresolvedBindingSpecs -> BSL.ByteString
encodeJson = encodeJson' . toABindingSpecs

-- | Encode binding specifications as YAML
encodeYaml :: UnresolvedBindingSpecs -> BSS.ByteString
encodeYaml = encodeYaml' . toABindingSpecs

-- | Write binding specifications to a file
--
-- The format is determined by the filename extension.
writeFile ::
     FilePath
  -> UnresolvedBindingSpecs
  -> IO (Either WriteBindingSpecsException ())
writeFile path specs
    | ".yaml" `List.isSuffixOf` path = Right <$> writeFileYaml path specs
    | ".json" `List.isSuffixOf` path = Right <$> writeFileJson path specs
    | otherwise = return $ Left (WriteBindingSpecsUnknownExtension path)

-- | Write binding specifications to a JSON file
writeFileJson :: FilePath -> UnresolvedBindingSpecs -> IO ()
writeFileJson path = BSL.writeFile path . encodeJson' . toABindingSpecs

-- | Write binding specifications to a YAML file
writeFileYaml :: FilePath -> UnresolvedBindingSpecs -> IO ()
writeFileYaml path = BSS.writeFile path . encodeYaml' . toABindingSpecs

{-------------------------------------------------------------------------------
  API: Header resolution
-------------------------------------------------------------------------------}

-- | Resolve headers in binding specifications
resolve ::
     Tracer IO (TraceWithCallStack ExtraClangArgsLog)
  -> ClangArgs
  -> UnresolvedBindingSpecs
  -> IO (Set ResolveHeaderException, ResolvedBindingSpecs)
resolve tracer args uSpecs = do
    let types = bindingSpecsTypes uSpecs
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
        rSpecs = BindingSpecs {
            bindingSpecsTypes = Map.mapMaybe resolve' types
          }
    return (errs, rSpecs)

{-------------------------------------------------------------------------------
  API: Merging
-------------------------------------------------------------------------------}

-- | Merge binding specifications
merge ::
     [ResolvedBindingSpecs]
  -> Either MergeBindingSpecsException ResolvedBindingSpecs
merge = \case
    []   -> Right empty
    x:xs -> do
      bindingSpecsTypes <- mergeTypes Set.empty (bindingSpecsTypes x) $
        concatMap (Map.toList . bindingSpecsTypes) xs
      return BindingSpecs{..}
  where
    mergeTypes ::
         Set CNameSpelling
      -> Map CNameSpelling [(Set SourcePath, a)]
      -> [(CNameSpelling, [(Set SourcePath, a)])]
      -> Either
           MergeBindingSpecsException
           (Map CNameSpelling [(Set SourcePath, a)])
    mergeTypes dupSet acc = \case
      []
        | Set.null dupSet -> Right acc
        | otherwise       -> Left $ MergeBindingSpecsConflict dupSet
      (cname, rs):ps ->
        case Map.insertLookupWithKey (const (++)) cname rs acc of
          (Nothing, acc') -> mergeTypes dupSet acc' ps
          (Just ls, acc') ->
            let lHeaders = Set.unions $ fst <$> ls
                rHeaders = Set.unions $ fst <$> rs
                iHeaders = Set.intersection lHeaders rHeaders
            in  if Set.null iHeaders
                  then mergeTypes dupSet acc' ps
                  else mergeTypes (Set.insert cname dupSet) acc' ps

{-------------------------------------------------------------------------------
  Auxiliary: Specifications files
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

newtype ABindingSpecs = ABindingSpecs {
      aBindingSpecsTypes :: [AOmittable ATypeSpecMapping]
    }
  deriving stock Show

instance Aeson.FromJSON ABindingSpecs where
  parseJSON = Aeson.withObject "ABindingSpecs" $ \o -> do
    aBindingSpecsTypes <- o .: "types"
    return ABindingSpecs{..}

instance Aeson.ToJSON ABindingSpecs where
  toJSON ABindingSpecs{..} = Aeson.object [
    "types" .= aBindingSpecsTypes
    ]

--------------------------------------------------------------------------------

data ATypeSpecMapping = ATypeSpecMapping {
      aTypeSpecMappingHeaders    :: [CHeaderIncludePath]
    , aTypeSpecMappingCName      :: CNameSpelling
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
  toJSON ATypeSpecMapping{..} = objectWithOptionalFields [
      "headers"    .=! listToJSON aTypeSpecMappingHeaders
    , "cname"      .=! aTypeSpecMappingCName
    , "module"     .=? aTypeSpecMappingModule
    , "identifier" .=? aTypeSpecMappingIdentifier
    , "instances"  .=? omitWhenNull aTypeSpecMappingInstances
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
    | otherwise = objectWithOptionalFields [
          "class"       .=! aInstanceSpecMappingClass
        , "strategy"    .=? aInstanceSpecMappingStrategy
        , "constraints" .=? omitWhenNull aInstanceSpecMappingConstraints
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

fromABindingSpecs ::
     FilePath
  -> ABindingSpecs
  -> Either ReadBindingSpecsException UnresolvedBindingSpecs
fromABindingSpecs path ABindingSpecs{..} = do
    bindingSpecsTypes <- mkTypeMap aBindingSpecsTypes
    return BindingSpecs{..}
  where
    mkTypeMap ::
         [AOmittable ATypeSpecMapping]
      -> Either
           ReadBindingSpecsException
           (Map CNameSpelling [(Set CHeaderIncludePath, Omittable TypeSpec)])
    mkTypeMap = mkTypeMapErr . foldr mkTypeMapInsert (Map.empty, Map.empty)

    mkTypeMapErr ::
         (Map CNameSpelling (Set CHeaderIncludePath), a)
      -> Either ReadBindingSpecsException a
    mkTypeMapErr (dupMap, x)
      | Map.null dupMap = Right x
      | otherwise = Left . ReadBindingSpecsConflict path $ Set.fromList [
            (cname, header)
          | (cname, headers) <- Map.toList dupMap
          , header <- Set.toList headers
          ]

    mkTypeMapInsert ::
         AOmittable ATypeSpecMapping
      -> ( Map CNameSpelling (Set CHeaderIncludePath)
         , Map CNameSpelling [(Set CHeaderIncludePath, Omittable TypeSpec)]
         )
      -> ( Map CNameSpelling (Set CHeaderIncludePath)
         , Map CNameSpelling [(Set CHeaderIncludePath, Omittable TypeSpec)]
         )
    mkTypeMapInsert aoTypeMapping (dupMap, accMap) =
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
          newV = [(Set.fromList headers, oTypeSpec)]
          x = Map.insertLookupWithKey (const (++)) cname newV accMap
      in  case x of
            (Nothing,   accMap') -> (dupMap, accMap')
            (Just oldV, accMap') ->
              (mkTypeMapDup cname newV oldV dupMap, accMap')

    mkTypeMapDup ::
         CNameSpelling
      -> [(Set CHeaderIncludePath, a)]
      -> [(Set CHeaderIncludePath, a)]
      -> Map CNameSpelling (Set CHeaderIncludePath)
      -> Map CNameSpelling (Set CHeaderIncludePath)
    mkTypeMapDup cname newV oldV dupMap =
      let commonHeaders =
            Set.intersection (mconcat (fst <$> newV)) (mconcat (fst <$> oldV))
      in  if Set.null commonHeaders
            then dupMap
            else Map.insertWith Set.union cname commonHeaders dupMap

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

toABindingSpecs :: UnresolvedBindingSpecs -> ABindingSpecs
toABindingSpecs BindingSpecs{..} = ABindingSpecs{..}
  where
    aBindingSpecsTypes :: [AOmittable ATypeSpecMapping]
    aBindingSpecsTypes = [
        case oType of
          Require TypeSpec{..} -> ARequire ATypeSpecMapping {
              aTypeSpecMappingHeaders    = Set.toAscList headers
            , aTypeSpecMappingCName      = cname
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
            , aTypeSpecMappingCName      = cname
            , aTypeSpecMappingModule     = Nothing
            , aTypeSpecMappingIdentifier = Nothing
            , aTypeSpecMappingInstances  = []
            }
      | (cname, xs) <- Map.toAscList bindingSpecsTypes
      , (headers, oType) <- xs
      ]

encodeJson' :: ABindingSpecs -> BSL.ByteString
encodeJson' = Aeson.encode

encodeYaml' :: ABindingSpecs -> BSS.ByteString
encodeYaml' = Data.Yaml.Pretty.encodePretty yamlConfig
  where
    yamlConfig :: Data.Yaml.Pretty.Config
    yamlConfig =
          Data.Yaml.Pretty.setConfCompare (compare `on` keyPosition)
        $ Data.Yaml.Pretty.defConfig

    keyPosition :: Text -> Int
    keyPosition = \case
      "omit"        -> 0  -- Omittable:1
      "types"       -> 1  -- ABindingSpecs:1
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

-- | Create an object 'Aeson.Value', supporting optional fields
objectWithOptionalFields :: [Maybe Aeson.Pair] -> Aeson.Value
objectWithOptionalFields = Aeson.Object . KM.fromList . catMaybes

-- | Construct a required field, for use with 'objectWithOptionalFields'
(.=!) :: (Aeson.KeyValue e kv, Aeson.ToJSON v) => Aeson.Key -> v -> Maybe kv
key .=! x = Just (key .= x)

-- | Construct an optional field, for use with 'objectWithOptionalFields'
(.=?) ::
     (Aeson.KeyValue e kv, Aeson.ToJSON v)
  => Aeson.Key
  -> Maybe v
  -> Maybe kv
key .=? mX = (key .=) <$> mX

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
