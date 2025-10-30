-- | Binding specification
--
-- Intended for qualified import.
--
-- > import HsBindgen.BindingSpec qualified as BindingSpec
module HsBindgen.BindingSpec (
    -- * Public API
    -- ** Type
    BindingSpec -- opaque
  , emptyBindingSpec
  , ExternalBindingSpec
  , PrescriptiveBindingSpec
    -- ** Configuration
  , EnableStdlibBindingSpec(..)
  , BindingSpecConfig(..)
  , Version.BindingSpecCompatibility(..)
    -- ** Loading
  , loadExtBindingSpecs
  , loadPrescriptiveBindingSpec
  , loadBindingSpecs
  , getStdlibBindingSpec
    -- ** Encoding
  , encodeBindingSpecJson
  , encodeBindingSpecYaml
    -- ** Trace messages
  , Common.BindingSpecReadMsg(..)
  , Common.BindingSpecResolveMsg(..)
  , Common.BindingSpecMergeMsg(..)
  , Common.BindingSpecMsg(..)

    -- * Internal API
  , BindingSpec.version
    -- ** Types
  , Common.Omittable(..)
  , BindingSpec.CTypeSpec(..)
  , BindingSpec.defCTypeSpec
  , BindingSpec.InstanceSpec(..)
  , BindingSpec.StrategySpec(..)
  , BindingSpec.ConstraintSpec(..)
    -- ** Query
  , getCTypes
  , lookupCTypeSpec
  ) where

import Data.ByteString qualified as BSS
import Data.ByteString.Lazy qualified as BSL

import Clang.Args (ClangArgs)
import Clang.Paths (SourcePath)

import HsBindgen.BindingSpec.Private.Common qualified as Common
import HsBindgen.BindingSpec.Private.Stdlib qualified as Stdlib
import HsBindgen.BindingSpec.Private.V1 qualified as BindingSpec
import HsBindgen.BindingSpec.Private.Version qualified as Version
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

-- | Binding specification
--
-- A binding specification serves two purposes:
--
-- * A /prescriptive binding specification/ is used to configure how bindings
--   are generated.
-- * An /external binding specification/ is used to specify existing bindings
--   that should be used, /external/ from the module being generated.
--
-- Note that a /generated binding specification/ may be used for either/both of
-- these two purposes.
data BindingSpec = BindingSpec {
      bindingSpecUnresolved :: BindingSpec.UnresolvedBindingSpec
    , bindingSpecResolved   :: BindingSpec.ResolvedBindingSpec
    }
  deriving stock (Show, Eq)

-- | Empty binding specification
emptyBindingSpec :: BindingSpec
emptyBindingSpec = BindingSpec {
      bindingSpecUnresolved = BindingSpec.empty
    , bindingSpecResolved   = BindingSpec.empty
    }

-- | External binding specification
--
-- This type alias is just used as documentation.
type ExternalBindingSpec = BindingSpec

-- | Prescriptive binding specification
--
-- This type alias is just used as documentation.
type PrescriptiveBindingSpec = BindingSpec

-- | Configure if the @stdlib@ binding specification should be used
data EnableStdlibBindingSpec =
    -- | Automatically include @stdlib@
    EnableStdlibBindingSpec
    -- | Do not include @stdlib@
  | DisableStdlibBindingSpec
  deriving stock (Show, Eq)

instance Default EnableStdlibBindingSpec where
  def = EnableStdlibBindingSpec

-- | Load external binding specifications
--
-- The format is determined by filename extension.  The following formats are
-- supported:
--
-- * YAML (@.yaml@ extension)
-- * JSON (@.json@ extension)
loadExtBindingSpecs ::
     Tracer IO Common.BindingSpecMsg
  -> ClangArgs
  -> EnableStdlibBindingSpec
  -> Version.BindingSpecCompatibility
  -> [FilePath]
  -> IO BindingSpec
loadExtBindingSpecs tracer args enableStdlib cmpt =
      fmap (uncurry BindingSpec)
    . BindingSpec.load
        tracer
        Common.BindingSpecResolveExternalHeader
        args
        stdSpec
        cmpt
  where
    stdSpec :: BindingSpec.UnresolvedBindingSpec
    stdSpec = case enableStdlib of
      EnableStdlibBindingSpec  -> Stdlib.bindingSpec
      DisableStdlibBindingSpec -> BindingSpec.empty

-- | Load prescriptive binding specification
--
-- The format is determined by filename extension.  The following formats are
-- supported:
--
-- * YAML (@.yaml@ extension)
-- * JSON (@.json@ extension)
loadPrescriptiveBindingSpec ::
     Tracer IO Common.BindingSpecMsg
  -> ClangArgs
  -> Version.BindingSpecCompatibility
  -> FilePath
  -> IO BindingSpec
loadPrescriptiveBindingSpec tracer args cmpt path = uncurry BindingSpec <$>
    BindingSpec.load
      tracer
      Common.BindingSpecResolvePrescriptiveHeader
      args
      BindingSpec.empty
      cmpt
      [path]

data BindingSpecConfig = BindingSpecConfig {
      stdlibSpec              :: EnableStdlibBindingSpec
    , compatibility           :: Version.BindingSpecCompatibility
    , extBindingSpecs         :: [FilePath]
    , prescriptiveBindingSpec :: Maybe FilePath
    }
  deriving stock (Show, Eq, Generic)

instance Default BindingSpecConfig where
  def = BindingSpecConfig {
          stdlibSpec              = EnableStdlibBindingSpec
        , compatibility           = def
        , extBindingSpecs         = []
        , prescriptiveBindingSpec = Nothing
        }

-- | A combination of 'loadExtBindingSpecs' and 'loadPrescriptiveBindingSpec'.
loadBindingSpecs ::
     Tracer IO Common.BindingSpecMsg
  -> ClangArgs
  -> BindingSpecConfig
  -> IO (ExternalBindingSpec, PrescriptiveBindingSpec)
loadBindingSpecs tracer clangArgs BindingSpecConfig{..} = do
    extSpecs <-
      loadExtBindingSpecs
        tracer
        clangArgs
        stdlibSpec
        compatibility
        extBindingSpecs
    pSpec <- case prescriptiveBindingSpec of
      Just path ->
        loadPrescriptiveBindingSpec
          tracer
          clangArgs
          compatibility
          path
      Nothing -> return emptyBindingSpec
    pure (extSpecs, pSpec)

-- | Get the standard library external binding specification
getStdlibBindingSpec ::
     Tracer IO Common.BindingSpecMsg
  -> ClangArgs
  -> IO (Hs.ModuleName, BindingSpec)
getStdlibBindingSpec tracer args = (Stdlib.hsModuleName,) <$>
    loadExtBindingSpecs
      tracer
      args
      EnableStdlibBindingSpec
      Version.BindingSpecStrict
      []

-- | Encode a binding specification (JSON format)
encodeBindingSpecJson :: Hs.ModuleName -> BindingSpec -> BSL.ByteString
encodeBindingSpecJson hsModuleName =
    BindingSpec.encodeJson hsModuleName . bindingSpecUnresolved

-- | Encode a binding specification (YAML format)
encodeBindingSpecYaml :: Hs.ModuleName -> BindingSpec -> BSS.ByteString
encodeBindingSpecYaml hsModuleName =
    BindingSpec.encodeYaml hsModuleName . bindingSpecUnresolved

{-------------------------------------------------------------------------------
  Internal API
-------------------------------------------------------------------------------}

-- | Get the C types in a binding specification
getCTypes :: BindingSpec -> Map C.QualName [Set SourcePath]
getCTypes = BindingSpec.getCTypes . bindingSpecResolved

-- | Lookup the @'Common.Omittable' 'CTypeSpec'@ associated with a C type
lookupCTypeSpec ::
     C.QualName
  -> Set SourcePath
  -> BindingSpec
  -> Maybe (Common.Omittable BindingSpec.CTypeSpec)
lookupCTypeSpec cQualName headers =
    BindingSpec.lookupCTypeSpec cQualName headers . bindingSpecResolved
