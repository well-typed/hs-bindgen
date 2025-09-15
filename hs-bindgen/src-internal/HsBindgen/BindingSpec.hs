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
    -- ** Loading
  , loadExtBindingSpecs
  , loadPrescriptiveBindingSpec
  , loadBindingSpecs
  , getStdlibBindingSpec
    -- ** Encoding
  , encodeBindingSpecJson
  , encodeBindingSpecYaml
    -- ** Trace messages
  , BindingSpec.BindingSpecReadMsg(..)
  , BindingSpec.BindingSpecResolveMsg(..)
  , BindingSpec.BindingSpecMergeMsg(..)
  , BindingSpec.BindingSpecMsg(..)

    -- * Internal API
    -- ** Types
  , BindingSpec.Omittable(..)
  , BindingSpec.TypeSpec(..)
  , BindingSpec.defaultTypeSpec
  , BindingSpec.InstanceSpec(..)
  , BindingSpec.StrategySpec(..)
  , BindingSpec.ConstraintSpec(..)
    -- ** Query
  , getTypes
  , lookupTypeSpec
  ) where

import Data.ByteString qualified as BSS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map

import Clang.Args (ClangArgs)
import Clang.Paths (SourcePath)

import HsBindgen.BindingSpec.Private qualified as BindingSpec
import HsBindgen.BindingSpec.Private.Stdlib qualified as Stdlib
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Imports
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
  | DisableStdlibBindingSpec
  deriving stock (Show, Eq)

-- | Load external binding specifications
--
-- The format is determined by filename extension.  The following formats are
-- supported:
--
-- * YAML (@.yaml@ extension)
-- * JSON (@.json@ extension)
loadExtBindingSpecs ::
     Tracer IO BindingSpec.BindingSpecMsg
  -> ClangArgs
  -> EnableStdlibBindingSpec
  -> [FilePath]
  -> IO BindingSpec
loadExtBindingSpecs tracer args enableStdlib =
      fmap (uncurry BindingSpec)
    . BindingSpec.load
        tracer
        BindingSpec.BindingSpecResolveExternalHeader
        args
        stdSpec
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
     Tracer IO BindingSpec.BindingSpecMsg
  -> ClangArgs
  -> FilePath
  -> IO BindingSpec
loadPrescriptiveBindingSpec tracer args path = uncurry BindingSpec <$>
    BindingSpec.load
      tracer
      BindingSpec.BindingSpecResolvePrescriptiveHeader
      args
      BindingSpec.empty
      [path]

data BindingSpecConfig = BindingSpecConfig {
      bindingSpecStdlibSpec              :: EnableStdlibBindingSpec
    , bindingSpecExtBindingSpecs         :: [FilePath]
    , bindingSpecPrescriptiveBindingSpec :: Maybe FilePath
    }
  deriving stock (Show, Eq, Generic)

instance Default BindingSpecConfig where
  def = BindingSpecConfig {
          bindingSpecStdlibSpec              = EnableStdlibBindingSpec
        , bindingSpecExtBindingSpecs         = []
        , bindingSpecPrescriptiveBindingSpec = Nothing
        }

-- | A combination of 'loadExtBindingSpecs' and 'loadPrescriptiveBindingSpec'.
loadBindingSpecs ::
     Tracer IO BindingSpec.BindingSpecMsg
  -> ClangArgs
  -> BindingSpecConfig
  -> IO (ExternalBindingSpec, PrescriptiveBindingSpec)
loadBindingSpecs tracer clangArgs BindingSpecConfig{..} = do
    extSpecs <- loadExtBindingSpecs
                  tracer
                  clangArgs
                  bindingSpecStdlibSpec
                  bindingSpecExtBindingSpecs
    pSpec <- case bindingSpecPrescriptiveBindingSpec of
               Just path -> loadPrescriptiveBindingSpec tracer clangArgs path
               Nothing   -> pure emptyBindingSpec
    pure (extSpecs, pSpec)

-- | Get the standard library external binding specification
getStdlibBindingSpec ::
     Tracer IO BindingSpec.BindingSpecMsg
  -> ClangArgs
  -> IO BindingSpec
getStdlibBindingSpec tracer args =
    loadExtBindingSpecs tracer args EnableStdlibBindingSpec []

-- | Encode a binding specification (JSON format)
encodeBindingSpecJson :: BindingSpec -> BSL.ByteString
encodeBindingSpecJson = BindingSpec.encodeJson . bindingSpecUnresolved

-- | Encode a binding specification (YAML format)
encodeBindingSpecYaml :: BindingSpec -> BSS.ByteString
encodeBindingSpecYaml = BindingSpec.encodeYaml . bindingSpecUnresolved

{-------------------------------------------------------------------------------
  Internal API
-------------------------------------------------------------------------------}

-- | Get the set of types in a binding specifications
getTypes :: BindingSpec -> Set C.QualName
getTypes = Map.keysSet . BindingSpec.bindingSpecTypes . bindingSpecResolved

-- | Lookup the @'Omittable' 'TypeSpec'@ associated with a C type
lookupTypeSpec ::
     C.QualName
  -> Set SourcePath
  -> BindingSpec
  -> Maybe (BindingSpec.Omittable BindingSpec.TypeSpec)
lookupTypeSpec cQualName headers =
    BindingSpec.lookupTypeSpec cQualName headers . bindingSpecResolved
