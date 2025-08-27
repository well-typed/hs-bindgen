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
    -- * Omittable
  , Omittable(..)
  , AOmittable(..)
  ) where

import Data.Aeson ((.:), (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as Aeson
import Text.SimplePrettyPrint (hang, hangs', string, textToCtxDoc, (><))

import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Resolve (ResolveHeaderMsg)
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

-- | Load binding specification file trace messages
data BindingSpecReadMsg =
    -- | Aeson parsing error
    BindingSpecReadAesonError FilePath String
  | -- | YAML parsing error
    BindingSpecReadYamlError FilePath String
  | -- | YAML parsing warning (which should be treated as an error)
    BindingSpecReadYamlWarning FilePath String
  | -- | Invalid C name
    BindingSpecReadInvalidCName FilePath Text
  | -- | Multiple entries for the same C type
    BindingSpecReadConflict FilePath C.QualName HashIncludeArg
  | -- | @#include@ argument message
    BindingSpecReadHashIncludeArg FilePath HashIncludeArgMsg
  deriving stock (Show)

instance IsTrace Level BindingSpecReadMsg where
  getDefaultLogLevel = \case
    BindingSpecReadHashIncludeArg _ x -> getDefaultLogLevel x
    _otherwise                        -> Error
  getSource = \case
    BindingSpecReadHashIncludeArg _ x -> getSource x
    _otherwise                        -> HsBindgen
  getTraceId = const "binding-spec-read"

instance PrettyForTrace BindingSpecReadMsg where
  prettyForTrace = \case
    BindingSpecReadAesonError path msg ->
      "error parsing JSON: " >< string path >< ": " >< string msg
    BindingSpecReadYamlError path msg ->
      -- 'lines' is used because the error includes newlines
      hangs' ("error parsing YAML: " >< string path) 2 $ map string $ lines msg
    BindingSpecReadYamlWarning path msg ->
      "error parsing YAML: " >< string path >< ": " >< string msg
    BindingSpecReadInvalidCName path t ->
      "invalid C name in " >< string path >< ": " >< textToCtxDoc t
    BindingSpecReadConflict path cQualName header ->
      "multiple entries in " >< string path >< " for C type: "
        >< textToCtxDoc (C.qualNameText cQualName)
        >< " (" >< string (getHashIncludeArg header) >< ")"
    BindingSpecReadHashIncludeArg path msg ->
      prettyForTrace msg >< " in " >< string path

--------------------------------------------------------------------------------

-- | Resolve binding specification trace messages
data BindingSpecResolveMsg =
    BindingSpecResolveExternalHeader ResolveHeaderMsg
  | BindingSpecResolvePrescriptiveHeader ResolveHeaderMsg
  | BindingSpecResolveTypeDropped C.QualName
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
      hang
        "during resolution of external binding specification:"
        2
        (prettyForTrace x)
    BindingSpecResolvePrescriptiveHeader x ->
      hang
        "during resolution of prescriptive binding specification:"
        2
        (prettyForTrace x)
    BindingSpecResolveTypeDropped cQualName ->
      "type dropped: " >< textToCtxDoc (C.qualNameText cQualName)

--------------------------------------------------------------------------------

-- | Merge binding specification trace messages
data BindingSpecMergeMsg =
    -- | Multiple binding specifications for the same C type
    BindingSpecMergeConflict C.QualName
  deriving stock (Show)

instance IsTrace Level BindingSpecMergeMsg where
  getDefaultLogLevel = const Error
  getSource          = const HsBindgen
  getTraceId         = const "binding-spec-merge"

instance PrettyForTrace BindingSpecMergeMsg where
  prettyForTrace = \case
    BindingSpecMergeConflict cQualName ->
      "conflicting binding specifications for C type: "
        >< textToCtxDoc (C.qualNameText cQualName)

--------------------------------------------------------------------------------

-- | All binding specification trace messages
data BindingSpecMsg =
    BindingSpecReadMsg    BindingSpecReadMsg
  | BindingSpecResolveMsg BindingSpecResolveMsg
  | BindingSpecMergeMsg   BindingSpecMergeMsg
  deriving stock    (Show, Generic)
  deriving anyclass (IsTrace Level, PrettyForTrace)

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
  deriving stock (Eq, Generic, Show)

-- | Aeson representation of 'Omittable'
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
