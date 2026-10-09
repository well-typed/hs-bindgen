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
  , Prescriptive(..)
  , ResolvedExtBinding(..)
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
  , BindingSpec.currentBindingSpecVersion
  , empty
  , fromGenerated
  , encodeBy
  , moduleName
  , prescriptiveFor
    -- ** Types
  , Common.Omittable(..)
  , BindingSpec.CTypeSpec(..)
  , BindingSpec.CEnumSpec(..)
  , BindingSpec.HsTypeSpec(..)
  , BindingSpec.HsTypeRep(..)
  , BindingSpec.HsRecordRep(..)
  , BindingSpec.HsNewtypeRep(..)
  , BindingSpec.HsFFIType(..)
  , BindingSpec.hsSpecFFIType
  , BindingSpec.InstanceSpec(..)
    -- ** Query
  , getCTypes
  , lookupCTypeSpec
  , lookupHsTypeSpec
    -- ** Merging
  , BindingSpec.MergedBindingSpecs
  , mergeInto
  , BindingSpec.lookupMergedBindingSpecs
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Ord qualified as Ord

import Clang.Args (ClangArgs)
import Clang.Paths (RealPath)

import HsBindgen.BindingSpec.Private.Common qualified as Common
import HsBindgen.BindingSpec.Private.Stdlib qualified as Stdlib
import HsBindgen.BindingSpec.Private.V1 qualified as BindingSpec
import HsBindgen.BindingSpec.Private.Version qualified as Version
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
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
      unresolved :: BindingSpec.UnresolvedBindingSpec
    , resolved   :: BindingSpec.ResolvedBindingSpec
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

-- | Prescriptive binding specification as loaded from a file
--
-- A prescriptive binding specification may leave out its module (@hsmodule@).
-- Whether it did is not visible in a t'BindingSpec', which always has a
-- module, so it is kept here.
data Prescriptive = Prescriptive {
      -- | The module that the file names, if it names one
      declared :: Maybe Hs.ModuleName

      -- | The specification
      --
      -- Its module is the 'declared' one.  When the file names none, it is the
      -- module that the specification was loaded for.
    , spec :: PrescriptiveBindingSpec
    }
  deriving stock (Show, Eq)

-- | Resolved external binding
data ResolvedExtBinding = ResolvedExtBinding{
      -- | C declaration for which we are using this binding
      cName :: C.DeclId

      -- | Source location(s) of C declaration for which we are using this binding
    , locs :: C.DeclLocs

      -- | The Haskell type which will be used
    , hsName :: Hs.ExtRef

      -- | Additional information about the C type
    , cSpec :: BindingSpec.CTypeSpec

      -- | Additional information about the Haskell type
    , hsSpec :: BindingSpec.HsTypeSpec
    }
  deriving stock (Eq, Ord, Generic, Show)

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
  -> EnableStdlibBindingSpec
  -> Version.BindingSpecCompatibility
  -> [FilePath]
  -> IO BindingSpec.MergedBindingSpecs
loadExtBindingSpecs tracer args enableStdlib cmpt paths = do
    uspecs <- withStdlib <$> mapMaybeM read' paths
    rspecs <- mapM resolve' uspecs
    let (msgs, mspec) = BindingSpec.merge rspecs
    mapM_ (traceWith tracerMerge . withCallStack) msgs
    return mspec
  where
    withStdlib ::
         [BindingSpec.UnresolvedBindingSpec]
      -> [BindingSpec.UnresolvedBindingSpec]
    withStdlib = case enableStdlib of
      DisableStdlibBindingSpec -> id
      EnableStdlibBindingSpec  -> (Stdlib.bindingSpec :)

    read' :: FilePath -> IO (Maybe BindingSpec.UnresolvedBindingSpec)
    read' = fmap (fmap snd) . BindingSpec.readFile tracerRead cmpt Nothing

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
--
-- A file that does not name a module is loaded as a specification for the
-- given module.  A file that names another module is loaded as it is; it is up
-- to the user of the specification to decide what that means.
loadPrescriptiveBindingSpec ::
     Tracer Common.BindingSpecMsg
  -> ClangArgs
  -> Hs.ModuleName
  -> Version.BindingSpecCompatibility
  -> Maybe FilePath
  -> IO Prescriptive
loadPrescriptiveBindingSpec tracer args hsModuleName cmpt =
    fmap (fromMaybe $ Prescriptive Nothing (empty hsModuleName)) . \case
      Nothing   -> return Nothing
      Just path ->
        BindingSpec.readFile tracerRead cmpt (Just hsModuleName) path >>= \case
          Nothing -> return Nothing
          Just (declared, uspec) ->
            Just . Prescriptive declared . BindingSpec uspec <$>
              BindingSpec.resolve
                tracerResolve
                Common.BindingSpecResolvePrescriptiveHeader
                args
                uspec
  where
    tracerRead :: Tracer Common.BindingSpecReadMsg
    tracerRead = contramap Common.BindingSpecReadMsg tracer

    tracerResolve :: Tracer Common.BindingSpecResolveMsg
    tracerResolve = contramap Common.BindingSpecResolveMsg tracer

-- | A combination of 'loadExtBindingSpecs' and 'loadPrescriptiveBindingSpec'
loadBindingSpecs ::
     Tracer Common.BindingSpecMsg
  -> ClangArgs
  -> Hs.ModuleName
  -> BindingSpecConfig
  -> IO (BindingSpec.MergedBindingSpecs, Prescriptive)
loadBindingSpecs tracer args hsModuleName config =
    (,)
      <$> loadExtBindingSpecs
            tracer
            args
            config.stdlibSpec
            config.compatibility
            config.extBindingSpecs
      <*> loadPrescriptiveBindingSpec
            tracer
            args
            hsModuleName
            config.compatibility
            config.prescriptiveBindingSpec

{-------------------------------------------------------------------------------
  Public API: Encoding
-------------------------------------------------------------------------------}

-- | Encode a binding specification
--
-- The C types are in the order of their names.
encode :: Common.Format -> BindingSpec -> ByteString
encode = encodeBy (Ord.comparing C.renderDeclId)

{-------------------------------------------------------------------------------
  Internal API
-------------------------------------------------------------------------------}

-- | Construct an empty binding specification for the given module
empty :: Hs.ModuleName -> BindingSpec
empty hsModuleName = BindingSpec{
      unresolved = BindingSpec.empty hsModuleName
    , resolved   = BindingSpec.empty hsModuleName
    }

-- | The binding specification that @hs-bindgen@ generated for a module
--
-- A specification that is loaded from a file asks @libclang@ where its headers
-- are.  A generated one names main headers only, and where those are is known
-- from the translation unit that it was generated from.
fromGenerated ::
     HasCallStack
  => Map C.HashIncludeArg RealPath -- ^ Where each main header is
  -> BindingSpec.UnresolvedBindingSpec
  -> BindingSpec
fromGenerated mainHeaders uSpec =
    case BindingSpec.resolveWith (`Map.lookup` mainHeaders) uSpec of
      ([], rSpec) -> BindingSpec{
          unresolved = uSpec
        , resolved   = rSpec
        }
      (cDeclIds, _rSpec) -> panicPure $
           "Generated binding specification names no main header for: "
        ++ show cDeclIds

-- | Encode a binding specification, the C types in the given order
encodeBy ::
     (C.DeclId -> C.DeclId -> Ordering)
  -> Common.Format
  -> BindingSpec
  -> ByteString
encodeBy compareCDeclId format spec =
    BindingSpec.encode compareCDeclId format spec.unresolved

-- | Get the module name for a binding specification
moduleName :: BindingSpec -> Hs.ModuleName
moduleName spec = spec.unresolved.moduleName

-- | The prescriptive binding specification for one module of several
--
-- One translation unit can be generated as several modules, and there is one
-- prescriptive binding specification for all of them.  A file that names no
-- module configures every module, so each one gets the specification as its
-- own.  A file that names a module configures that module, and the others get
-- a specification that says nothing.
prescriptiveFor :: Hs.ModuleName -> Prescriptive -> PrescriptiveBindingSpec
prescriptiveFor hsModuleName prescriptive = case prescriptive.declared of
    Just declared
      | declared == hsModuleName -> prescriptive.spec
      | otherwise                -> empty hsModuleName
    Nothing -> BindingSpec{
        unresolved =
          prescriptive.spec.unresolved{BindingSpec.moduleName = hsModuleName}
      , resolved   =
          prescriptive.spec.resolved{BindingSpec.moduleName = hsModuleName}
      }

{-------------------------------------------------------------------------------
  Internal API: Query
-------------------------------------------------------------------------------}

-- | Get the C types in a binding specification
getCTypes :: BindingSpec -> Map C.DeclId [Set RealPath]
getCTypes spec = BindingSpec.getCTypes spec.resolved

-- | Lookup the @'Common.Omittable' 'BindingSpec.CTypeSpec'@ associated with a C
-- type
lookupCTypeSpec ::
     C.DeclId
  -> Set RealPath
  -> BindingSpec
  -> Maybe (Hs.ModuleName, Common.Omittable BindingSpec.CTypeSpec)
lookupCTypeSpec cDeclId headers spec =
    BindingSpec.lookupCTypeSpec cDeclId headers spec.resolved

-- | Lookup the 'BindingSpec.HsTypeSpec' associated with a Haskell type
lookupHsTypeSpec ::
     Hs.Name Hs.NsTypeConstr
  -> BindingSpec
  -> Maybe BindingSpec.HsTypeSpec
lookupHsTypeSpec hsIdentifier spec =
    BindingSpec.lookupHsTypeSpec hsIdentifier spec.resolved

{-------------------------------------------------------------------------------
  Internal API: Merging
-------------------------------------------------------------------------------}

-- | Add external binding specifications to merged ones
mergeInto ::
     BindingSpec.MergedBindingSpecs
  -> [ExternalBindingSpec]
  -> ([Common.BindingSpecMergeMsg], BindingSpec.MergedBindingSpecs)
mergeInto merged specs = BindingSpec.mergeInto merged $ map (.resolved) specs
