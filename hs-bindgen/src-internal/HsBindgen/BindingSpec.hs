-- | Binding specification
--
-- Intended for qualified import.
--
-- > import HsBindgen.BindingSpec qualified as BindingSpec
module HsBindgen.BindingSpec (
    -- * Public API
    -- ** Types
    BindingSpec -- opaque
  , ExternalBindingSpec
  , PrescriptiveBindingSpec
    -- ** Configuration
  , EnableStdlibBindingSpec(..)
  , BindingSpecConfig(..)
  , Version.BindingSpecCompatibility(..)
    -- ** Loading
  , getStdlibBindingSpec
  , loadExtBindingSpecs
  , loadPrescriptiveBindingSpec
  , loadBindingSpecs
    -- ** Encoding
  , Common.Format(..)
  , Common.getFormat
  , encode
    -- ** Trace messages
  , Common.BindingSpecReadMsg(..)
  , Common.BindingSpecResolveMsg(..)
  , Common.BindingSpecMergeMsg(..)
  , Common.BindingSpecMsg(..)

    -- * Internal API
  , BindingSpec.version
  , empty
  , moduleName
    -- ** Types
  , Common.Omittable(..)
  , BindingSpec.BindingSpecTarget(..)
  , BindingSpec.CTypeSpec(..)
  , BindingSpec.CTypeRep(..)
  , BindingSpec.HsTypeSpec(..)
  , BindingSpec.HsTypeRep(..)
  , BindingSpec.HsRecordRep(..)
  , BindingSpec.HsNewtypeRep(..)
  , BindingSpec.FFIType(..)
  , BindingSpec.InstanceSpec(..)
  , BindingSpec.StrategySpec(..)
  , BindingSpec.ConstraintSpec(..)
    -- ** Query
  , getCTypes
  , lookupCTypeSpec
  , lookupHsTypeSpec
    -- ** Merging
  , BindingSpec.MergedBindingSpecs
  , BindingSpec.lookupMergedBindingSpecs
  ) where

import Data.ByteString (ByteString)

import Clang.Args (ClangArgs)
import Clang.Paths (SourcePath)

import HsBindgen.BindingSpec.Private.Common qualified as Common
import HsBindgen.BindingSpec.Private.Stdlib qualified as Stdlib
import HsBindgen.BindingSpec.Private.V1 qualified as BindingSpec
import HsBindgen.BindingSpec.Private.Version qualified as Version
import HsBindgen.Config.ClangArgs qualified as ClangArgs
import HsBindgen.Frontend.Naming
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Monad
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Public API: Types
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

-- | External binding specification
--
-- This type alias is just used as documentation.
type ExternalBindingSpec = BindingSpec

-- | Prescriptive binding specification
--
-- This type alias is just used as documentation.
type PrescriptiveBindingSpec = BindingSpec

{-------------------------------------------------------------------------------
  Public API: Configuration
-------------------------------------------------------------------------------}

-- | Configure if the @stdlib@ binding specification should be used
data EnableStdlibBindingSpec =
    -- | Automatically include @stdlib@
    EnableStdlibBindingSpec
    -- | Do not include @stdlib@
  | DisableStdlibBindingSpec
  deriving stock (Show, Eq)

instance Default EnableStdlibBindingSpec where
  def = EnableStdlibBindingSpec

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

{-------------------------------------------------------------------------------
  Public API: Loading
-------------------------------------------------------------------------------}

-- | Get the standard library external binding specification
getStdlibBindingSpec ::
     Tracer Common.BindingSpecMsg
  -> ClangArgs
  -> IO ExternalBindingSpec
getStdlibBindingSpec tracer args = BindingSpec Stdlib.bindingSpec <$>
    BindingSpec.resolve
      (contramap Common.BindingSpecResolveMsg tracer)
      Common.BindingSpecResolveExternalHeader
      args
      Stdlib.bindingSpec

-- | Load external binding specifications
--
-- The format is determined by filename extension.  The following formats are
-- supported:
--
-- * YAML (@.yaml@ extension)
-- * JSON (@.json@ extension)
loadExtBindingSpecs ::
     Tracer Common.BindingSpecMsg
  -> ClangArgs
  -> ClangArgs.Target
  -> EnableStdlibBindingSpec
  -> Version.BindingSpecCompatibility
  -> [FilePath]
  -> IO BindingSpec.MergedBindingSpecs
loadExtBindingSpecs tracer args target enableStdlib cmpt paths = do
    uspecs <- withStdlib <$> mapMaybeM read' paths
    rspecs <- mapM resolve' uspecs
    let (msgs, mspec) = BindingSpec.merge rspecs
    mapM_ (traceWith tracerMerge) msgs
    return mspec
  where
    withStdlib ::
         [BindingSpec.UnresolvedBindingSpec]
      -> [BindingSpec.UnresolvedBindingSpec]
    withStdlib = case enableStdlib of
      DisableStdlibBindingSpec -> id
      EnableStdlibBindingSpec  -> (Stdlib.bindingSpec :)

    read' :: FilePath -> IO (Maybe BindingSpec.UnresolvedBindingSpec)
    read' path = BindingSpec.readFile tracerRead cmpt path >>= \case
      Nothing -> return Nothing
      Just uspec
        | uspec `BindingSpec.isCompatTarget` target -> return (Just uspec)
        | otherwise -> do
            traceWith tracerRead $ Common.BindingSpecReadIncompatibleTarget path
            return Nothing

    resolve' ::
         BindingSpec.UnresolvedBindingSpec
      -> IO BindingSpec.ResolvedBindingSpec
    resolve' =
      BindingSpec.resolve
        tracerResolve
        Common.BindingSpecResolveExternalHeader
        args

    tracerRead :: Tracer Common.BindingSpecReadMsg
    tracerRead = contramap Common.BindingSpecReadMsg tracer

    tracerResolve :: Tracer Common.BindingSpecResolveMsg
    tracerResolve = contramap Common.BindingSpecResolveMsg tracer

    tracerMerge :: Tracer Common.BindingSpecMergeMsg
    tracerMerge = contramap Common.BindingSpecMergeMsg tracer

-- | Load prescriptive binding specification
--
-- The format is determined by filename extension.  The following formats are
-- supported:
--
-- * YAML (@.yaml@ extension)
-- * JSON (@.json@ extension)
loadPrescriptiveBindingSpec ::
     Tracer Common.BindingSpecMsg
  -> ClangArgs
  -> ClangArgs.Target
  -> Hs.ModuleName
  -> Version.BindingSpecCompatibility
  -> Maybe FilePath
  -> IO PrescriptiveBindingSpec
loadPrescriptiveBindingSpec tracer args target hsModuleName cmpt =
    fmap (fromMaybe $ empty target hsModuleName) . \case
      Nothing   -> return Nothing
      Just path -> BindingSpec.readFile tracerRead cmpt path >>= \case
        Nothing -> return Nothing
        Just uspec
          | uspec `BindingSpec.isCompatTarget` target -> do
              when (BindingSpec.isAnyTarget uspec) . traceWith tracerRead $
                Common.BindingSpecReadAnyTargetNotEnforced path
              Just . BindingSpec uspec <$>
                BindingSpec.resolve
                  tracerResolve
                  Common.BindingSpecResolvePrescriptiveHeader
                  args
                  uspec
          | otherwise -> do
              traceWith tracerRead $
                Common.BindingSpecReadIncompatibleTarget path
              return Nothing
  where
    tracerRead :: Tracer Common.BindingSpecReadMsg
    tracerRead = contramap Common.BindingSpecReadMsg tracer

    tracerResolve :: Tracer Common.BindingSpecResolveMsg
    tracerResolve = contramap Common.BindingSpecResolveMsg tracer

-- | A combination of 'loadExtBindingSpecs' and 'loadPrescriptiveBindingSpec'
loadBindingSpecs ::
     Tracer Common.BindingSpecMsg
  -> ClangArgs
  -> ClangArgs.Target
  -> Hs.ModuleName
  -> BindingSpecConfig
  -> IO (BindingSpec.MergedBindingSpecs, PrescriptiveBindingSpec)
loadBindingSpecs tracer args target hsModuleName BindingSpecConfig{..} =
    (,)
      <$> loadExtBindingSpecs
            tracer
            args
            target
            stdlibSpec
            compatibility
            extBindingSpecs
      <*> loadPrescriptiveBindingSpec
            tracer
            args
            target
            hsModuleName
            compatibility
            prescriptiveBindingSpec

{-------------------------------------------------------------------------------
  Public API: Encoding
-------------------------------------------------------------------------------}

-- | Encode a binding specification
encode :: Common.Format -> BindingSpec -> ByteString
encode format =
      BindingSpec.encode BindingSpec.defCompareCDeclId format
    . bindingSpecUnresolved

{-------------------------------------------------------------------------------
  Internal API
-------------------------------------------------------------------------------}

-- | Construct an empty binding specification for the given target and module
empty :: ClangArgs.Target -> Hs.ModuleName -> BindingSpec
empty target hsModuleName = BindingSpec {
      bindingSpecUnresolved = BindingSpec.empty target hsModuleName
    , bindingSpecResolved   = BindingSpec.empty target hsModuleName
    }

-- | Get the module name for a binding specification
moduleName :: BindingSpec -> Hs.ModuleName
moduleName = BindingSpec.bindingSpecModule . bindingSpecUnresolved

{-------------------------------------------------------------------------------
  Internal API: Query
-------------------------------------------------------------------------------}

-- | Get the C types in a binding specification
getCTypes :: BindingSpec -> Map DeclId [Set SourcePath]
getCTypes = BindingSpec.getCTypes . bindingSpecResolved

-- | Lookup the @'Common.Omittable' 'BindingSpec.CTypeSpec'@ associated with a C
-- type
lookupCTypeSpec ::
     DeclId
  -> Set SourcePath
  -> BindingSpec
  -> Maybe (Hs.ModuleName, Common.Omittable BindingSpec.CTypeSpec)
lookupCTypeSpec cDeclId headers =
    BindingSpec.lookupCTypeSpec cDeclId headers . bindingSpecResolved

-- | Lookup the 'BindingSpec.HsTypeSpec' associated with a Haskell type
lookupHsTypeSpec ::
     Hs.Identifier
  -> BindingSpec
  -> Maybe BindingSpec.HsTypeSpec
lookupHsTypeSpec hsIdentifier =
    BindingSpec.lookupHsTypeSpec hsIdentifier . bindingSpecResolved
