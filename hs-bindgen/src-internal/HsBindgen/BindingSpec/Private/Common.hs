-- | Binding specification code that is common across all versions
--
-- This /private/ module may only be used by "HsBindgen.BindingSpec" and
-- sub-modules.
--
-- Intended for qualified import when re-exporting.
--
-- > import HsBindgen.BindingSpec.Private.Common qualified as Common
--
-- Intended for unqualified import otherwise.
module HsBindgen.BindingSpec.Private.Common (
    -- * Trace messages
    BindingSpecReadMsg(..)
  , BindingSpecResolveMsg(..)
  , BindingSpecMergeMsg(..)
  , BindingSpecMsg(..)
    -- * Aeson representation
    -- $ARep
  , ARepIso(..)
  , ARepKV(..)
    -- * Omittable
  , Omittable(..)
  , AOmittable(..)
  , AOmittable'
    -- * File API
  , Format(..)
  , getFormat
  , readVersion
    -- * Aeson auxiliary functions
  , omitWhenNull
  , listToJSON
  , listFromJSON
  , typeOf
  ) where

import Control.Applicative (asum)
import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as Aeson
import Data.Coerce
import Data.List qualified as List
import Data.Typeable (Typeable, typeRep)
import Data.Yaml qualified as Yaml
import Data.Yaml.Internal qualified
import Text.SimplePrettyPrint ((><))
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.BindingSpec.Private.Version
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Resolve (ResolveHeaderMsg)
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

-- | Load binding specification file trace messages
data BindingSpecReadMsg =
    BindingSpecReadAesonError FilePath String
  | BindingSpecReadYamlError FilePath String
  | BindingSpecReadYamlWarning FilePath String
  | BindingSpecReadParseVersion FilePath AVersion
  | BindingSpecReadIncompatibleVersion FilePath AVersion
  | BindingSpecReadTargetNotSpecified FilePath
  | BindingSpecReadIncompatibleTarget FilePath
  | BindingSpecReadAnyTargetNotEnforced FilePath
  | BindingSpecReadModuleMismatch FilePath Hs.ModuleName Hs.ModuleName
  | BindingSpecReadModuleNotSpecified FilePath
  | BindingSpecReadInvalidCName FilePath Text
  | BindingSpecReadCTypeConflict FilePath DeclId HashIncludeArg
  | BindingSpecReadHsIdentifierNoRef FilePath Hs.Identifier
  | BindingSpecReadHsTypeConflict FilePath Hs.Identifier
  | BindingSpecReadHashIncludeArg FilePath HashIncludeArgMsg
  | BindingSpecReadConvertVersion FilePath BindingSpecVersion BindingSpecVersion
  deriving stock (Show)

instance IsTrace Level BindingSpecReadMsg where
  getDefaultLogLevel = \case
    BindingSpecReadAesonError{}           -> Error
    BindingSpecReadYamlError{}            -> Error
    BindingSpecReadYamlWarning{}          -> Error
    BindingSpecReadParseVersion{}         -> Debug
    BindingSpecReadIncompatibleVersion{}  -> Error
    BindingSpecReadTargetNotSpecified{}   -> Error
    BindingSpecReadIncompatibleTarget{}   -> Error
    BindingSpecReadAnyTargetNotEnforced{} -> Notice
    BindingSpecReadModuleMismatch{}       -> Warning
    BindingSpecReadModuleNotSpecified{}   -> Error
    BindingSpecReadInvalidCName{}         -> Error
    BindingSpecReadCTypeConflict{}        -> Error
    BindingSpecReadHsIdentifierNoRef{}    -> Error
    BindingSpecReadHsTypeConflict{}       -> Error
    BindingSpecReadHashIncludeArg _ x     -> getDefaultLogLevel x
    BindingSpecReadConvertVersion _ f t
      | f <= t                            -> Info
      | otherwise                         -> Notice
  getSource = \case
    BindingSpecReadHashIncludeArg _ x -> getSource x
    _otherwise                        -> HsBindgen
  getTraceId = const "binding-spec-read"

instance PrettyForTrace BindingSpecReadMsg where
  prettyForTrace = \case
    BindingSpecReadAesonError path msg -> PP.hcat [
        "error parsing JSON: "
      , PP.string path
      , ": "
      , PP.string msg
      ]
    BindingSpecReadYamlError path msg ->
      -- 'lines' is used because the error includes newlines
      -- TODO: Should this use PP.renderedLines instead?
      PP.hangs' ("error parsing YAML: " >< PP.string path) 2 $
        map PP.string $ lines msg
    BindingSpecReadYamlWarning path msg -> PP.hcat [
        "error parsing YAML: "
      , PP.string path
      , ": "
      , PP.string msg
      ]
    BindingSpecReadParseVersion path version ->
      PP.hangs' ("parsing binding specification: " >< PP.string path) 2 [
          "hs-bindgen version: " ><
            prettyForTraceHsBindgenVersion version.bindgen
        , "binding specification version: " ><
            prettyForTrace version.bindingSpec
        ]
    BindingSpecReadIncompatibleVersion path version ->
      PP.hangs' ("incompatible binding specification version: " >< PP.string path) 2 [
          "hs-bindgen version: " ><
            prettyForTraceHsBindgenVersion version.bindgen
        , "binding specification version: " ><
            prettyForTrace version.bindingSpec
        ]
    BindingSpecReadTargetNotSpecified path ->
      "target not specified in external binding specification: " >< PP.string path
    BindingSpecReadIncompatibleTarget path ->
      "incompatible binding specification target: " >< PP.string path
    BindingSpecReadAnyTargetNotEnforced path ->
      "'any' target of prescriptive binding specification not yet enforced: " ><
        PP.string path
    BindingSpecReadModuleMismatch path bsModule curModule ->
      PP.hangs'
        ("binding specification module mismatch: " >< PP.string path)
        2
        [ "binding specification module: " >< prettyForTrace bsModule
        , "current module: " >< prettyForTrace curModule
        ]
    BindingSpecReadModuleNotSpecified path ->
      "module not specified in external binding specification: " >< PP.string path
    BindingSpecReadInvalidCName path t -> PP.hcat [
        "invalid C name in "
      , PP.string path
      , ": "
      , PP.text t
      ]
    BindingSpecReadCTypeConflict path cDeclId header -> PP.hcat [
        "multiple entries in "
      , PP.string path
      , " for C type: "
      , prettyForTrace cDeclId
      , " ("
      , PP.string header.path
      , ")"
      ]
    BindingSpecReadHsIdentifierNoRef path hsIdentifier -> PP.hcat [
        "Haskell identifier in "
      , PP.string path
      , " not referenced by C type: "
      , PP.text hsIdentifier.text
      ]
    BindingSpecReadHsTypeConflict path hsIdentifier -> PP.hcat [
        "multiple entries in "
      , PP.string path
      , " for Haskell type: "
      , PP.text hsIdentifier.text
      ]
    BindingSpecReadHashIncludeArg path msg ->
      prettyForTrace msg >< " in " >< PP.string path
    BindingSpecReadConvertVersion path versionFrom versionTo -> PP.cat [
        "converting binding specification: "
      , PP.string path
      , " (from version "
      , prettyForTrace versionFrom
      , ", to version "
      , prettyForTrace versionTo
      , ")"
      ]

--------------------------------------------------------------------------------

-- | Resolve binding specification trace messages
data BindingSpecResolveMsg =
    BindingSpecResolveExternalHeader     ResolveHeaderMsg
  | BindingSpecResolvePrescriptiveHeader ResolveHeaderMsg
  | BindingSpecResolveTypeDropped        DeclId
  deriving stock (Show)

instance IsTrace Level BindingSpecResolveMsg where
  getDefaultLogLevel = \case
    BindingSpecResolveExternalHeader x
      -- Any warnings/errors that happen while resolving /external/ headers are
      -- 'Info' only: the only consequence is that those headers will then not
      -- match against anything (and we might generate separate warnings/errors
      -- for that anyway while resolving the binding specification).
      | lvl > Info -> Info
      | otherwise  -> lvl
      where
        lvl = getDefaultLogLevel x
    BindingSpecResolvePrescriptiveHeader x ->
      -- However, any errors that happen during /prescriptive/ binding specs
      -- truly are errors.
      getDefaultLogLevel x
    BindingSpecResolveTypeDropped{} -> Info
  getSource = \case
    BindingSpecResolveExternalHeader     x -> getSource x
    BindingSpecResolvePrescriptiveHeader x -> getSource x
    BindingSpecResolveTypeDropped{}        -> HsBindgen
  getTraceId = const "binding-spec-resolve"

instance PrettyForTrace BindingSpecResolveMsg where
  prettyForTrace = \case
    BindingSpecResolveExternalHeader x ->
      PP.hang
        "During resolution of external binding specification:"
        2
        (prettyForTrace x)
    BindingSpecResolvePrescriptiveHeader x ->
      PP.hang
        "During resolution of prescriptive binding specification:"
        2
        (prettyForTrace x)
    BindingSpecResolveTypeDropped cDeclId ->
      "Type dropped: " >< prettyForTrace cDeclId

--------------------------------------------------------------------------------

-- | Merge binding specification trace messages
newtype BindingSpecMergeMsg =
    BindingSpecMergeConflict DeclId
  deriving stock (Show)

instance IsTrace Level BindingSpecMergeMsg where
  getDefaultLogLevel = const Error
  getSource          = const HsBindgen
  getTraceId         = const "binding-spec-merge"

instance PrettyForTrace BindingSpecMergeMsg where
  prettyForTrace = \case
    BindingSpecMergeConflict cDeclId ->
      "conflicting binding specifications for C type: "
        >< prettyForTrace cDeclId

--------------------------------------------------------------------------------

-- | All binding specification trace messages
data BindingSpecMsg =
    BindingSpecReadMsg    BindingSpecReadMsg
  | BindingSpecResolveMsg BindingSpecResolveMsg
  | BindingSpecMergeMsg   BindingSpecMergeMsg
  deriving stock    (Show, Generic)
  deriving anyclass (IsTrace Level, PrettyForTrace)

{-------------------------------------------------------------------------------
  Aeson representation
-------------------------------------------------------------------------------}

-- $ARep
--
-- Each binding specification version uses a data family named @ARep@ to define
-- Aeson representations for that version, making the relationship between the
-- types explicit.
--
-- This abstraction makes code uniform.  Example:
--
-- > let inst = InstanceSpec {
-- >         strategy    = fromARep @ARep <$> arep.strategy
-- >       , constraints = fromARep @ARep <$> arep.constraints
-- >       }
-- > in ..
--
-- When a version depends on a previous version, instances for the previous
-- version can be accessed using qualified imports: @fromARep \@V1.ARep@.

-- | Aeson representation that is isomorphic to the type
--
-- @f@ is the data family for a specific binding specification version.  Type
-- @f a@ is the Aeson representation for type @a@.
class ARepIso (f :: Star -> Star) a where
  fromARep :: f a -> a
  toARep   :: a -> f a

  default fromARep :: Coercible (f a) a => f a -> a
  fromARep = coerce

  default toARep   :: Coercible a (f a) => a -> f a
  toARep = coerce

-- | Aeson representation that encodes a key in addition to the type
--
-- @f@ is the data family for a specific binding specification version.  Type
-- @f a@ is the Aeson representation for type @a@.  Type @ARepK f a@ is the key
-- that is encoded within the Aeson representation.
class ARepKV (f :: Star -> Star) a where
  data ARepK f a

  fromARepKV :: f a -> (ARepK f a, a)

  toARepKV :: ARepK f a -> a -> f a

{-------------------------------------------------------------------------------
  Omittable
-------------------------------------------------------------------------------}

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
  deriving stock (Eq, Ord, Generic, Show)

-- | Aeson representation of 'Omittable'
--
-- A value of type @r@ is required by specifying it as usual.
--
-- A value of type @o@ is omitted by wrapping it in an object as the value for
-- single key @omit@.
--
-- The wrapped types differ because omitted values may not require as much
-- information as required values.
data AOmittable o r = AOmit o | ARequire r
  deriving stock Show

instance
     (Aeson.FromJSON o, Aeson.FromJSON r)
  => Aeson.FromJSON (AOmittable o r)
  where
    parseJSON = \case
      Aeson.Object o | KM.size o == 1 && KM.member "omit" o ->
        AOmit <$> o .: "omit"
      v -> ARequire <$> Aeson.parseJSON v

instance (Aeson.ToJSON o, Aeson.ToJSON r) => Aeson.ToJSON (AOmittable o r) where
  toJSON = \case
    ARequire x -> Aeson.toJSON x
    AOmit    x -> Aeson.object ["omit" .= x]

-- | Aeson representation of 'Omittable' with same omitted and required type
type AOmittable' a = AOmittable a a

{-------------------------------------------------------------------------------
  File API
-------------------------------------------------------------------------------}

-- | Supported specification file formats
data Format =
    FormatJSON
  | FormatYAML

-- | Get format based on filename
--
-- YAML is used if the extension is unknown.
getFormat :: FilePath -> Format
getFormat path
    | ".json" `List.isSuffixOf` path = FormatJSON
    | otherwise                      = FormatYAML

-- | Function that reads a file and gets the 'AVersion', which determines how to
-- parse the corresponding 'Aeson.Value'
type ReadVersionFunction =
     Tracer BindingSpecReadMsg
  -> FilePath
  -> IO (Maybe (AVersion, Aeson.Value))

-- | Read a binding specification file, returning the 'BindingSpecVersion' and
-- 'Aeson.Value'
--
-- The format is determined by the filename extension.
readVersion :: ReadVersionFunction
readVersion tracer path = case getFormat path of
    FormatYAML -> readVersionYaml tracer path
    FormatJSON -> readVersionJson tracer path

-- | Read a binding specification JSON file, returning the 'BindingSpecVersion'
-- and 'Aeson.Value'
readVersionJson :: ReadVersionFunction
readVersionJson tracer path = Aeson.eitherDecodeFileStrict' path >>= \case
    Right value -> getAVersionM tracer path value
    Left err -> do
      traceWith tracer $ BindingSpecReadAesonError path err
      return Nothing

-- | Read a binding specification YAML file, returning the 'BindingSpecVersion'
-- and 'Aeson.Value'
readVersionYaml :: ReadVersionFunction
readVersionYaml tracer path = Yaml.decodeFileWithWarnings path >>= \case
    Right (warnings, value) -> do
      forM_ warnings $ \case
        Data.Yaml.Internal.DuplicateKey jsonPath -> do
          let msg = "duplicate key: " ++ Aeson.formatPath jsonPath
          traceWith tracer $ BindingSpecReadYamlWarning path msg
      getAVersionM tracer path value
    Left err -> do
      let msg = Yaml.prettyPrintParseException err
      traceWith tracer $ BindingSpecReadYamlError path msg
      return Nothing

getAVersionM ::
     MonadIO m
  => Tracer BindingSpecReadMsg
  -> FilePath
  -> Aeson.Value
  -> m (Maybe (AVersion, Aeson.Value))
getAVersionM tracer path value = case getAVersion value of
    Right aVersion -> return $ Just (aVersion, value)
    Left err -> do
      traceWith tracer $ BindingSpecReadAesonError path err
      return Nothing

{-------------------------------------------------------------------------------
  Aeson auxiliary functions
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
