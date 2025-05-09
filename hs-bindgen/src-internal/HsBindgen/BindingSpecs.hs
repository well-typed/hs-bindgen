module HsBindgen.BindingSpecs (
    -- * Public API
    BindingSpecs(..)
  , emptyBindingSpecs
  , loadBindingSpecs
    -- * Types
  , IBindingSpecs(..)
  , TypeSpec(..)
  , Omittable(..)
  , ExtType(..)
    -- ** Haskell references
  , HsRef(..)
  , HsModuleName(..)
  , HsIdentifier(..)
    -- ** Instances
  , HsTypeClass(..)
  , DeriveStrategy(..)
    -- ** Exceptions
  , LoadBindingSpecsException(..)
  , MergeBindingSpecsException(..)
  , BindingSpecsException(..)
  , BindingSpecsExceptions(..)
  , GetExtTypeException(..)
  , WriteBindingSpecsException(..)
    -- * API
  , emptyIBindingSpecs
  , loadBindingSpecs'
  , lookupBindingSpecsType
  , lookupTypeSpec
  , getExtType
    -- ** Specification files
  , readBindingSpecsFile
  , readBindingSpecsJson
  , readBindingSpecsYaml
  , encodeBindingSpecsJson
  , encodeBindingSpecsYaml
  , writeBindingSpecsFile
  , writeBindingSpecsJson
  , writeBindingSpecsYaml
    -- ** Header resolution
  , resolveBindingSpecs
    -- ** Merging
  , mergeBindingSpecs
  ) where

import Control.Applicative (asum)
import Control.Exception (Exception(displayException))
import Control.Monad ((<=<))
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
import Data.Yaml.Pretty qualified as Yaml.Pretty
import Text.Read (readMaybe)

import Clang.Args
import Clang.CNameSpelling
import Clang.Paths
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Orphans ()
import HsBindgen.Resolve

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

-- | Binding specifications
--
-- Binding specifications serve two purposes:
--
-- * They can be used to configure how to generate bindings.
-- * They can specify existing (\"external\") bindings to use when generating
--   bindings.
newtype BindingSpecs = BindingSpecs (IBindingSpecs SourcePath)
  deriving (Eq, Show)

-- | Empty binding specifications
emptyBindingSpecs :: BindingSpecs
emptyBindingSpecs = BindingSpecs emptyIBindingSpecs

-- | Load, resolve, and merge binding specifications, throwing an
-- 'HsBindgenException' on error
--
-- The format is determined by filename extension.
loadBindingSpecs ::
     ClangArgs
  -> [FilePath]
  -> IO (Set ResolveHeaderException, BindingSpecs)
loadBindingSpecs args =
    either (throwIO . HsBindgenException) (return . fmap BindingSpecs)
      <=< loadBindingSpecs' args

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Binding specifications (internal)
--
-- A C type is identified using a 'CNameSpelling' and a set of headers where the
-- type may be (transitively) declared.  The @header@ type parameter determines
-- the representation of the header path.  Binding specification files must
-- specify headers as they are used in C @#include@ directives, loaded to an
-- @'IBindingSpecs' 'CHeaderIncludePath'@.  Headers are resolved to get a
-- @'IBindingSpecs' 'SourcePath'@.
--
-- The 'BindingSpecs' wrapper is the (opaque) public type that hides the type
-- parameter.
newtype IBindingSpecs header = IBindingSpecs {
      -- | Type specifications
      --
      -- For a given 'CNameSpelling', the corresponding sets of headers are
      -- disjoint.  The type is therefore equivalent to
      -- @'Map' 'CNameSpelling' ('Map' header 'TypeSpec')@, but this type is
      -- used as an optimization.  In most cases, each 'CNameSpelling' is mapped
      -- to exactly one value with a set of few headers.
      --
      -- When using binding specifications to configure how bindings are
      -- generated, this map is used to specify customizations and omissions.
      -- Types that are not specified may be generated without customization.
      iBindingSpecsTypes :: Map CNameSpelling [(Set header, TypeSpec)]
    }
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- Binding specification for a C type
data TypeSpec = TypeSpec {
      -- | Haskell type
      --
      -- A @'Just' 'Require' 'HsRef'@ value specifies the Haskell module and
      -- identifier name.
      --
      -- A @'Just' 'Omit'@ value instructs @hs-bindgen@ to not generate the
      -- Haskell type at all.
      --
      -- A @'Nothing'@ value instructs @hs-bindgen@ to generate the Haskell
      -- type, using the name mangler to generate the identifier name.
      typeSpecHaskell :: Maybe (Omittable HsRef)

    , -- | Instance specifications
      --
      -- A @'Just' 'Require' 'DeriveStrategy'@ value specifies the strategy to
      -- use to derive/generate the instance.
      --
      -- A @'Just' 'Omit'@ value instructs @hs-bindgen@ to not derive/generate
      -- the instance.
      --
      -- A @'Nothing'@ value instructs @hs-bindgen@ to derive/generate the
      -- instance using @hs-bindgen@ defaults.
      --
      -- When using binding specifications to configure how bindings are
      -- generated, this map is used to specify customizations and omissions.
      -- Instances that are not specified may be derived/generated without
      -- customization.
      typeSpecInstances :: Map HsTypeClass (Maybe (Omittable DeriveStrategy))
    }
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | Wrapper for types that may be omitted
--
-- In general, the following conventions are followed:
--
-- * If a value is specified, it is required.  It is an error if @hs-bindgen@ is
--   unable to satisfy the requirement.
-- * If @omit@ is specified, then @hs-bindgen@ does /not/ generate the
--   corresponding code.  For example, a user may omit a @Show@ instance so that
--   they can write their own.
-- * If nothing is specified, @hs-bindgen@ generates code using defaults.  This
--   case is /not/ represented by 'Omittable'.
data Omittable a =
    Require a
  | Omit
  deriving stock (Eq, Ord, Show)

instance Aeson.FromJSON a => Aeson.FromJSON (Omittable a) where
  parseJSON = \case
    "omit" -> return Omit
    v      -> Require <$> Aeson.parseJSON v

instance Aeson.ToJSON a => Aeson.ToJSON (Omittable a) where
  toJSON = \case
    Require x -> Aeson.toJSON x
    Omit      -> "omit"

--------------------------------------------------------------------------------

-- | External binding type
data ExtType = ExtType {
      extTypeHaskell   :: HsRef
    , extTypeInstances :: Set HsTypeClass
    }
  deriving (Eq, Generic, Ord, Show)

{-------------------------------------------------------------------------------
  Types: Haskell references
-------------------------------------------------------------------------------}

-- | Reference to a Haskell identifier in a specific Haskell module
data HsRef = HsRef {
      hsRefModule     :: HsModuleName
    , hsRefIdentifier :: HsIdentifier
    }
  deriving stock (Eq, Generic, Ord, Show)

instance Aeson.FromJSON HsRef where
  parseJSON = Aeson.withObject "HsRef" $ \o -> do
    hsRefModule     <- o .: "module"
    hsRefIdentifier <- o .: "identifier"
    return HsRef{..}

instance Aeson.ToJSON HsRef where
  toJSON HsRef{..} = Aeson.object [
        "module"     .= hsRefModule
      , "identifier" .= hsRefIdentifier
      ]

--------------------------------------------------------------------------------

-- | Haskell module name
--
-- Example: @HsBindgen.Runtime.LibC@
newtype HsModuleName = HsModuleName { getHsModuleName :: Text }
  deriving stock (Generic)
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, Eq, IsString, Ord, Show)

--------------------------------------------------------------------------------

-- | Haskell identifier
--
-- Example: @CTm@
--
-- This type is different from 'HsBindgen.Hs.AST.HsName' in that it does not
-- include a 'HsBindgen.Hs.AST.Namespace'.
newtype HsIdentifier = HsIdentifier { getHsIdentifier :: Text }
  deriving stock (Generic)
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, Eq, IsString, Ord, Show)

{------------------------------------------------------------------------------
  Types: Instances
-------------------------------------------------------------------------------}

-- | Type class
data HsTypeClass =
    -- Haskell98 derivable classes
    -- <https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/deriving.html>
    Eq
  | Ord
  | Enum
  | Ix
  | Bounded
  | Read
  | Show

    -- Classes we can only derive through newtype deriving
  | Bits
  | FiniteBits
  | Floating
  | Fractional
  | Integral
  | Num
  | Real
  | RealFloat
  | RealFrac

    -- Classes we can generate when all components have instances
  | StaticSize
  | ReadRaw
  | WriteRaw
  | Storable
  deriving stock (Eq, Generic, Ord, Read, Show)

instance Aeson.FromJSON HsTypeClass where
  parseJSON = Aeson.withText "HsTypeClass" $ \t ->
    let s = Text.unpack t
    in  case readMaybe s of
          Just clss -> return clss
          Nothing   -> Aeson.parseFail $ "unknown type class: " ++ s

instance Aeson.ToJSON HsTypeClass where
  toJSON = Aeson.String . Text.pack . show

--------------------------------------------------------------------------------

-- | Strategy to derive a type class
data DeriveStrategy =
    -- | Derive using the @newtype@ strategy
    DeriveNewtype

  | -- | Derive using the @stock@ strategy
    DeriveStock

  | -- | Derive using the @via AsCEnum@ strategy
    DeriveViaAsCEnum

  | -- | Derive using the @via AsSequentialCEnum@ strategy
    DeriveViaAsSequentialCEnum
  deriving (Bounded, Enum, Eq, Ord, Show)

instance Aeson.FromJSON DeriveStrategy where
  parseJSON = Aeson.withText "DeriveStrategy" $ \t ->
    case Map.lookup t deriveStrategyFromText of
      Just strategy -> return strategy
      Nothing       -> Aeson.parseFail $ "unknown strategy: " ++ (Text.unpack t)

instance Aeson.ToJSON DeriveStrategy where
  toJSON = Aeson.String . deriveStrategyText

deriveStrategyText :: DeriveStrategy -> Text
deriveStrategyText = \case
    DeriveNewtype              -> "newtype"
    DeriveStock                -> "stock"
    DeriveViaAsCEnum           -> "via AsCEnum"
    DeriveViaAsSequentialCEnum -> "via AsSequentialCEnum"

deriveStrategyFromText :: Map Text DeriveStrategy
deriveStrategyFromText = Map.fromList [
      (deriveStrategyText strategy, strategy)
    | strategy <- [minBound..]
    ]

{-------------------------------------------------------------------------------
  Types: Exceptions
-------------------------------------------------------------------------------}

-- | Failed to load binding specifications file
data LoadBindingSpecsException =
    -- | Unknown file extension
    LoadBindingSpecsUnknownExtension FilePath
  | -- | Aeson parsing error
    LoadBindingSpecsAesonError FilePath String
  | -- | YAML parsing error
    LoadBindingSpecsYamlError FilePath Yaml.ParseException
  | -- | YAML parsing warnings (which should be treated like errors)
    LoadBindingSpecsYamlWarning FilePath [Data.Yaml.Internal.Warning]
    -- | Multiple specifications for the same C name and header in the same file
  | LoadBindingSpecsConflict
      FilePath
      (Set (CNameSpelling, CHeaderIncludePath))
  deriving stock (Show)

instance Exception LoadBindingSpecsException where
  displayException = \case
    LoadBindingSpecsUnknownExtension path -> "unknown extension: " ++ path
    LoadBindingSpecsAesonError path err ->
      "error parsing JSON: " ++ path ++ ": " ++ err
    LoadBindingSpecsYamlError path err -> unlines [
        "error parsing YAML: " ++ path
      , Yaml.prettyPrintParseException err
      ]
    LoadBindingSpecsYamlWarning path warnings ->
      unlines $
          ("duplicate keys in YAML file: " ++ path)
        : [ "  " ++ Aeson.formatPath jsonPath
          | Data.Yaml.Internal.DuplicateKey jsonPath <- warnings
          ]
    LoadBindingSpecsConflict path conflicts ->
      unlines $
          ( "multiple specifications for same C name and header: "
              ++ path
          )
        : [ "  " ++ Text.unpack (getCNameSpelling cname)
              ++ ' ' : getCHeaderIncludePath header
          | (cname, header) <- Set.toAscList conflicts
          ]

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

-- | Failed loading, resolving, or merging binding specifications
data BindingSpecsException =
    LoadBindingSpecsException  LoadBindingSpecsException
  | MergeBindingSpecsException MergeBindingSpecsException
  deriving stock (Show)

instance Exception BindingSpecsException where
  displayException = \case
    LoadBindingSpecsException  e -> displayException e
    MergeBindingSpecsException e -> displayException e

-- | Failed loading, resolving, or merging binding specifications
newtype BindingSpecsExceptions = BindingSpecsExceptions [BindingSpecsException]
  deriving stock (Show)

instance Exception BindingSpecsExceptions where
  displayException (BindingSpecsExceptions es) =
    unlines $ map displayException es

-- | External binding error
data GetExtTypeException =
    -- | No Haskell module and identifier specified
    GetExtTypeNoHaskell CNameSpelling
    -- | Type omitted
  | GetExtTypeOmit CNameSpelling
  deriving stock (Show)

instance Exception GetExtTypeException where
  displayException = \case
    GetExtTypeNoHaskell cname ->
      "no Haskell module and identifier specified: "
        ++ Text.unpack (getCNameSpelling cname)
    GetExtTypeOmit cname ->
      "type omitted: " ++ Text.unpack (getCNameSpelling cname)

-- | Failed to write binding specifications file
newtype WriteBindingSpecsException =
    -- | Unknown file extension
    WriteBindingSpecsUnknownExtension FilePath
  deriving stock (Show)

instance Exception WriteBindingSpecsException where
  displayException = \case
    WriteBindingSpecsUnknownExtension path -> "unknown extension: " ++ path

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Empty binding specifications (internal)
emptyIBindingSpecs :: IBindingSpecs header
emptyIBindingSpecs = IBindingSpecs {
      iBindingSpecsTypes = Map.empty
    }

-- | Load, resolve, and merge binding specifications
--
-- The format is determined by filename extension.
loadBindingSpecs' ::
     ClangArgs
  -> [FilePath]
  -> IO
       ( Either
           BindingSpecsExceptions
           (Set ResolveHeaderException, IBindingSpecs SourcePath)
       )
loadBindingSpecs' args paths = do
    (errs, uspecs) <-
      first (map LoadBindingSpecsException) . partitionEithers
        <$> mapM readBindingSpecsFile paths
    (resolveErrs, specss) <-
      first Set.unions . unzip <$> mapM (resolveBindingSpecs args) uspecs
    return $ case first MergeBindingSpecsException (mergeBindingSpecs specss) of
      Right specs
        | null errs -> Right (resolveErrs, specs)
        | otherwise -> Left $ BindingSpecsExceptions errs
      Left mergeErr -> Left $ BindingSpecsExceptions (errs ++ [mergeErr])

-- | Lookup a type C name spelling in binding specifications
lookupBindingSpecsType ::
     CNameSpelling
  -> IBindingSpecs SourcePath
  -> Maybe [(Set SourcePath, TypeSpec)]
lookupBindingSpecsType cname = Map.lookup cname . iBindingSpecsTypes

-- | Lookup a 'TypeSpec' associated with a set of header paths with at least one
-- in common with the specified set of header paths
--
-- This is purposefully separate from 'lookupBindingSpecsType' because we do not
-- even need to compute the set of header paths unless there is a match for the
-- C name spelling.
lookupTypeSpec ::
     Set SourcePath
  -> [(Set SourcePath, TypeSpec)]
  -> Maybe TypeSpec
lookupTypeSpec headers = aux
  where
    aux :: [(Set SourcePath, a)] -> Maybe a
    aux = \case
      (specHeaders, x):ps
        | Set.null (headers `Set.intersection` specHeaders) -> aux ps
        | otherwise                                         -> Just x
      []                                                    -> Nothing

getExtType :: CNameSpelling -> TypeSpec -> Either GetExtTypeException ExtType
getExtType cname TypeSpec{..} = do
    extTypeHaskell <- case typeSpecHaskell of
      Just (Require hsRef) -> return hsRef
      Just Omit            -> Left $ GetExtTypeOmit cname
      Nothing              -> Left $ GetExtTypeNoHaskell cname
    let extTypeInstances =
          Map.foldrWithKey auxInstance Set.empty typeSpecInstances
    return ExtType{..}
  where
    auxInstance ::
         HsTypeClass
      -> Maybe (Omittable DeriveStrategy)
      -> Set HsTypeClass
      -> Set HsTypeClass
    auxInstance clss = \case
      Just Omit  -> id
      _otherwise -> Set.insert clss

{-------------------------------------------------------------------------------
  API: Specification files
-------------------------------------------------------------------------------}

-- | Read binding specifications from a file
--
-- The format is determined by the filename extension.
readBindingSpecsFile ::
     FilePath
  -> IO (Either LoadBindingSpecsException (IBindingSpecs CHeaderIncludePath))
readBindingSpecsFile path
    | ".yaml" `List.isSuffixOf` path = readBindingSpecsYaml path
    | ".json" `List.isSuffixOf` path = readBindingSpecsJson path
    | otherwise = return $ Left (LoadBindingSpecsUnknownExtension path)

-- | Read binding specifications from a JSON file
readBindingSpecsJson ::
     FilePath
  -> IO (Either LoadBindingSpecsException (IBindingSpecs CHeaderIncludePath))
readBindingSpecsJson path = do
    ees <- Aeson.eitherDecodeFileStrict' path
    return $ case ees of
      Right specs -> fromABindingSpecs path specs
      Left err    -> Left (LoadBindingSpecsAesonError path err)

-- | Read binding specifications from a YAML file
readBindingSpecsYaml ::
     FilePath
  -> IO (Either LoadBindingSpecsException (IBindingSpecs CHeaderIncludePath))
readBindingSpecsYaml path = do
    eews <- Yaml.decodeFileWithWarnings path
    return $ case eews of
      Right ([], specs) -> fromABindingSpecs path specs
      Right (warnings, _) -> Left (LoadBindingSpecsYamlWarning path warnings)
      Left err -> Left (LoadBindingSpecsYamlError path err)

-- | Encode binding specifications as JSON
encodeBindingSpecsJson :: IBindingSpecs CHeaderIncludePath -> BSL.ByteString
encodeBindingSpecsJson = encodeJson . toABindingSpecs

-- | Encode binding specifications as YAML
encodeBindingSpecsYaml :: IBindingSpecs CHeaderIncludePath -> BSS.ByteString
encodeBindingSpecsYaml = encodeYaml . toABindingSpecs

-- | Write binding specifications to a file
--
-- The format is determined by the filename extension.
writeBindingSpecsFile ::
     FilePath
  -> IBindingSpecs CHeaderIncludePath
  -> IO (Either WriteBindingSpecsException ())
writeBindingSpecsFile path specs
    | ".yaml" `List.isSuffixOf` path =
        Right <$> writeBindingSpecsYaml path specs
    | ".json" `List.isSuffixOf` path =
        Right <$> writeBindingSpecsJson path specs
    | otherwise = return $ Left (WriteBindingSpecsUnknownExtension path)

-- | Write binding specifications to a JSON file
writeBindingSpecsJson :: FilePath -> IBindingSpecs CHeaderIncludePath -> IO ()
writeBindingSpecsJson path = BSL.writeFile path . encodeJson . toABindingSpecs

-- | Write binding specifications to a YAML file
writeBindingSpecsYaml :: FilePath -> IBindingSpecs CHeaderIncludePath -> IO ()
writeBindingSpecsYaml path = BSS.writeFile path . encodeYaml . toABindingSpecs

{-------------------------------------------------------------------------------
  API: Header resolution
-------------------------------------------------------------------------------}

-- | Resolve headers in binding specifications
resolveBindingSpecs ::
     ClangArgs
  -> IBindingSpecs CHeaderIncludePath
  -> IO (Set ResolveHeaderException, IBindingSpecs SourcePath)
resolveBindingSpecs args IBindingSpecs{..} = do
    let cPaths = Set.toAscList . mconcat $
          fst <$> mconcat (Map.elems iBindingSpecsTypes)
    (errs, headerMap) <- bimap Set.fromList Map.fromList . partitionEithers
      <$> mapM (\cPath -> fmap (cPath,) <$> resolveHeader' args cPath) cPaths
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
        resolve :: [(Set CHeaderIncludePath, a)] -> Maybe [(Set SourcePath, a)]
        resolve lU = case mapMaybe resolve1 lU of
          lR
            | null lR   -> Nothing
            | otherwise -> Just lR
        specs = IBindingSpecs {
            iBindingSpecsTypes = Map.mapMaybe resolve iBindingSpecsTypes
          }
    return (errs, specs)

{-------------------------------------------------------------------------------
  API: Merging
-------------------------------------------------------------------------------}

-- | Merge binding specifications
mergeBindingSpecs ::
     [IBindingSpecs SourcePath]
  -> Either MergeBindingSpecsException (IBindingSpecs SourcePath)
mergeBindingSpecs = \case
    []   -> Right emptyIBindingSpecs
    x:xs -> do
      iBindingSpecsTypes <- mergeTypes Set.empty (iBindingSpecsTypes x) $
        concatMap (Map.toList . iBindingSpecsTypes) xs
      return IBindingSpecs{..}
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
  Auxiliary functions: specification files
-------------------------------------------------------------------------------}

newtype ABindingSpecs = ABindingSpecs {
      aBindingsSpecsTypes :: [ATypeMapping]
    }
  deriving Show

instance Aeson.FromJSON ABindingSpecs where
  parseJSON = Aeson.withObject "ABindingSpecs" $ \o -> do
    aBindingsSpecsTypes <- o .: "types"
    return ABindingSpecs{..}

instance Aeson.ToJSON ABindingSpecs where
  toJSON ABindingSpecs{..} = Aeson.object [
      "types" .= aBindingsSpecsTypes
    ]

--------------------------------------------------------------------------------

data ATypeMapping = ATypeMapping {
      aTypeMappingHeaders   :: [CHeaderIncludePath]
    , aTypeMappingCName     :: CNameSpelling
    , aTypeMappingHaskell   :: Maybe (Omittable HsRef)
    , aTypeMappingInstances :: [AInstanceSpec]
    }
  deriving Show

instance Aeson.FromJSON ATypeMapping where
  parseJSON = Aeson.withObject "ATypeMapping" $ \o -> do
    aTypeMappingHeaders   <- o .:  "headers" >>= listFromJSON
    aTypeMappingCName     <- o .:  "cname"
    aTypeMappingHaskell   <- o .:? "haskell"
    aTypeMappingInstances <- o .:? "instances" .!= []
    return ATypeMapping{..}

instance Aeson.ToJSON ATypeMapping where
  toJSON ATypeMapping{..} = objectWithOptionalFields [
      "headers"   .=! listToJSON aTypeMappingHeaders
    , "cname"     .=! aTypeMappingCName
    , "haskell"   .=? aTypeMappingHaskell
    , "instances" .=? omitWhenNull aTypeMappingInstances
    ]

--------------------------------------------------------------------------------

data AInstanceSpec = AInstanceSpec {
      aInstanceSpecClass    :: HsTypeClass
    , aInstanceSpecStrategy :: Maybe (Omittable DeriveStrategy)
    }
  deriving Show

instance Aeson.FromJSON AInstanceSpec where
  parseJSON = \case
    s@Aeson.String{} -> do
      aInstanceSpecClass <- Aeson.parseJSON s
      let aInstanceSpecStrategy = Nothing
      return AInstanceSpec{..}
    Aeson.Object o -> do
      aInstanceSpecClass    <- o .:  "class"
      aInstanceSpecStrategy <- o .:? "strategy"
      return AInstanceSpec{..}
    v -> Aeson.parseFail $
      "expected AInstanceSpec String or Object, but encountered " ++ typeOf v

instance Aeson.ToJSON AInstanceSpec where
  toJSON AInstanceSpec{..}
      | isNothing aInstanceSpecStrategy = Aeson.toJSON aInstanceSpecClass
      | otherwise = objectWithOptionalFields [
            "class"    .=! aInstanceSpecClass
          , "strategy" .=? aInstanceSpecStrategy
          ]

--------------------------------------------------------------------------------

fromABindingSpecs ::
     FilePath
  -> ABindingSpecs
  -> Either LoadBindingSpecsException (IBindingSpecs CHeaderIncludePath)
fromABindingSpecs path ABindingSpecs{..} = do
    iBindingSpecsTypes <- mkTypeMap aBindingsSpecsTypes
    return IBindingSpecs{..}
  where
    mkTypeMap ::
         [ATypeMapping]
      -> Either
           LoadBindingSpecsException
           (Map CNameSpelling [(Set CHeaderIncludePath, TypeSpec)])
    mkTypeMap = mkTypeMapErr . foldr mkTypeMapInsert (Map.empty, Map.empty)

    mkTypeMapErr ::
         (Map CNameSpelling (Set CHeaderIncludePath), a)
      -> Either LoadBindingSpecsException a
    mkTypeMapErr (dupMap, x)
      | Map.null dupMap = Right x
      | otherwise = Left . LoadBindingSpecsConflict path $ Set.fromList [
            (cname, header)
          | (cname, headers) <- Map.toList dupMap
          , header <- Set.toList headers
          ]

    mkTypeMapInsert ::
         ATypeMapping
      -> ( Map CNameSpelling (Set CHeaderIncludePath)
         , Map CNameSpelling [(Set CHeaderIncludePath, TypeSpec)]
         )
      -> ( Map CNameSpelling (Set CHeaderIncludePath)
         , Map CNameSpelling [(Set CHeaderIncludePath, TypeSpec)]
         )
    mkTypeMapInsert ATypeMapping{..} (dupMap, accMap) =
      let typeSpec = TypeSpec {
              typeSpecHaskell   = aTypeMappingHaskell
            , typeSpecInstances = mkInstanceMap aTypeMappingInstances
            }
          newV = [(Set.fromList aTypeMappingHeaders, typeSpec)]
          x = Map.insertLookupWithKey (const (++)) aTypeMappingCName newV accMap
      in  case x of
            (Nothing,   accMap') -> (dupMap, accMap')
            (Just oldV, accMap') ->
              (mkTypeMapDup aTypeMappingCName newV oldV dupMap, accMap')

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
         [AInstanceSpec]
      -> Map HsTypeClass (Maybe (Omittable DeriveStrategy))
    mkInstanceMap xs = Map.fromList [
        (aInstanceSpecClass, aInstanceSpecStrategy)
      | AInstanceSpec{..} <- xs
      ]

toABindingSpecs :: IBindingSpecs CHeaderIncludePath -> ABindingSpecs
toABindingSpecs IBindingSpecs{..} = ABindingSpecs{..}
  where
    aBindingsSpecsTypes :: [ATypeMapping]
    aBindingsSpecsTypes = [
        ATypeMapping {
            aTypeMappingHeaders   = Set.toAscList headers
          , aTypeMappingCName     = cname
          , aTypeMappingHaskell   = typeSpecHaskell
          , aTypeMappingInstances = unInstanceMap typeSpecInstances
          }
      | (cname, xs) <- Map.toAscList iBindingSpecsTypes
      , (headers, TypeSpec{..}) <- xs
      ]

    unInstanceMap ::
         Map HsTypeClass (Maybe (Omittable DeriveStrategy))
      -> [AInstanceSpec]
    unInstanceMap instanceMap = [
        AInstanceSpec{..}
      | (aInstanceSpecClass, aInstanceSpecStrategy) <- Map.toAscList instanceMap
      ]

encodeJson :: ABindingSpecs -> BSL.ByteString
encodeJson = Aeson.encode

encodeYaml :: ABindingSpecs -> BSS.ByteString
encodeYaml = Yaml.Pretty.encodePretty yamlConfig
  where
    yamlConfig :: Yaml.Pretty.Config
    yamlConfig =
          Yaml.Pretty.setConfCompare (compare `on` keyPosition)
        $ Yaml.Pretty.defConfig

    keyPosition :: Text -> Int
    keyPosition = \case
      -- ABindingSpecs
      "types" -> 1
      -- ATypeMapping
      "headers"   -> 1
      "cname"     -> 2
      "haskell"   -> 3
      "instances" -> 4
      -- HsRef
      "module"     -> 1
      "identifier" -> 2
      -- InstanceSpec
      "class"    -> 1
      "strategy" -> 2
      -- Unknown
      key -> panicPure $ "Unknown key: " ++ show key

{-------------------------------------------------------------------------------
  Auxiliary functions: Aeson
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
