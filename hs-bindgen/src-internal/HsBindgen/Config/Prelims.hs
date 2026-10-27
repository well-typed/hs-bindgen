module HsBindgen.Config.Prelims (
    -- * Base module name
    BaseModuleName(..)
  , baseModuleNameToString
  , fromBaseModuleName

    -- * Field naming strategy
  , FieldNamingStrategy(..)

    -- * Qualified style
  , QualifiedStyle(..)

    -- * Module render config
  , ModuleRenderConfig(..)

    -- * Boot files
  , HsBootPolicy(..)

    -- * Libraries
  , LibraryImports(..)

    -- * Unique IDs
  , UniqueId (..)
  , UniqueIdMsg (..)
  , checkUniqueId
  ) where

import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Backend.Category
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Base module name
-------------------------------------------------------------------------------}

-- | Base module name from which other module names are derived
--
-- For example, the base module name might be @Generated@, from which we
-- derive @Generated@, @Generated.Safe@, etc.
newtype BaseModuleName = BaseModuleName { text :: Text }
  deriving stock (Eq, Generic)
  deriving newtype (IsString, Show)

instance Default BaseModuleName where
  def = "Generated"

baseModuleNameToString :: BaseModuleName -> String
baseModuleNameToString baseModule = Text.unpack baseModule.text

fromBaseModuleName :: BaseModuleName -> Maybe Category -> Hs.ModuleName
fromBaseModuleName (BaseModuleName base) Nothing =
    Hs.ModuleName base
fromBaseModuleName (BaseModuleName base) (Just CType) =
    Hs.ModuleName base
fromBaseModuleName (BaseModuleName base) (Just (CTerm cat)) =
    Hs.ModuleName (base <> "." <> submodule cat)
  where
    -- NOTE: It is important that types are stored in a module without any
    -- suffix; we depend on this assumption for binding specifications (which
    -- only refer to types, never to functions or globals).
    submodule :: TermCategory -> Text
    submodule CCallback = "Callback"
    submodule CSafe     = "Safe"
    submodule CUnsafe   = "Unsafe"
    submodule CFunPtr   = "FunPtr"
    submodule CGlobal   = "Global"

{-------------------------------------------------------------------------------
  Field naming strategy
-------------------------------------------------------------------------------}

-- | Strategy for naming struct\/union fields.
--
-- With 'AddFieldPrefixes' (the default), field names are prefixed with the
-- struct name to avoid name collisions (e.g., @timeval_tv_sec@).
--
-- With 'OmitFieldPrefixes', field names are not prefixed (e.g., @tv_sec@). This
-- usually necessitates the @DuplicateRecordFields@ GHC extension, which is
-- automatically added when this strategy is selected.
data FieldNamingStrategy =
    AddFieldPrefixes
  | OmitFieldPrefixes
  deriving stock (Show, Eq, Generic)

instance Default FieldNamingStrategy where
  def = AddFieldPrefixes

{-------------------------------------------------------------------------------
  Qualified style
-------------------------------------------------------------------------------}

-- | Style for qualified imports in generated code.
--
-- With 'PreQualified' (the default), imports are prepositive:
-- @import qualified Data.Proxy@.
--
-- With 'PostQualified', imports use the @ImportQualifiedPost@ extension:
-- @import Data.Proxy qualified@. This avoids warnings from
-- @-Wprepositive-qualified-module@.
data QualifiedStyle =
    PreQualified
  | PostQualified
  deriving stock (Show, Eq, Generic)

instance Default QualifiedStyle where
  def = PreQualified

{-------------------------------------------------------------------------------
  Module render config
-------------------------------------------------------------------------------}

-- | Configuration for rendering Haskell modules.
--
-- Bundles settings that affect the surface syntax of generated modules but
-- do not change semantics.
data ModuleRenderConfig = ModuleRenderConfig {
      qualifiedStyle :: QualifiedStyle
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Default)

{-------------------------------------------------------------------------------
  Boot files
-------------------------------------------------------------------------------}

-- | Whether to generate boot files
--
-- With 'GenerateHsBoot', every types module gets a boot file, and the code
-- behind its callbacks goes to a module of its own, @Lib.Foo.Callback@, which
-- has a boot file too. With 'DoNotGenerateHsBoot' (the default), that
-- code stays in the types module, and there are no boot files.
--
-- Only multi-module output has boot files.
data HsBootPolicy =
    GenerateHsBoot
  | DoNotGenerateHsBoot
  deriving stock (Show, Eq, Generic)

instance Default HsBootPolicy where
  def = DoNotGenerateHsBoot

{-------------------------------------------------------------------------------
  Libraries
-------------------------------------------------------------------------------}

-- | How the modules of a header import the types modules of the other headers
-- of its library (@--library@)
--
-- Both sets are empty without @--library@.
data LibraryImports = LibraryImports {
      -- | The types modules of the library
      --
      -- The types module and the boot files import these through their boot
      -- files, since the types modules of a library can use each other.
      typesModules    :: Set Hs.ModuleName

      -- | The types modules whose instances the term modules need
      --
      -- A term module converts values at the FFI, which needs the @FFIType@
      -- equation of each type it converts. Such an equation can come from
      -- another types module that the term module sees only through a boot
      -- file, which cannot hold it: with @typedef status shape_status;@ in
      -- @shape.h@, @FFIType Shape_status@ is @FFIType Status@, from the types
      -- module of @point.h@. So the term modules import these for their
      -- instances.
    , instanceModules :: Set Hs.ModuleName
    }
  deriving stock (Show, Eq)

instance Default LibraryImports where
  def = LibraryImports{
        typesModules    = Set.empty
      , instanceModules = Set.empty
      }

{-------------------------------------------------------------------------------
  Unique IDs
-------------------------------------------------------------------------------}

-- | C uses a global namespace. We must ensure that identifiers generated by
-- @hs-bindgen@ are unique.
--
-- Defaults to the empty string @t'UniqueId' ""@, which is __suboptimal__. We
-- encourage using unique identifiers such as
-- "com.well-typed.your-package-name".
data UniqueId = UniqueId { unUniqueId :: String }
  deriving stock (Show, Eq, Ord)

instance Default UniqueId where
  def = UniqueId ""

data UniqueIdMsg =
    UniqueIdEmpty
  deriving (Show, Eq, Ord)

instance PrettyForTrace UniqueIdMsg where
  prettyForTrace = \case
    UniqueIdEmpty -> PP.vcat [
        "empty unique identifier ('UniqueId', '--unique-id'):"
      , "  C uses a global namespace."
      , "  We encourage using a unique identifier to avoid duplicate symbol names."
      , "  For example, use and adapt 'com.example.package'."
      ]

instance IsTrace Level UniqueIdMsg where
  getDefaultLogLevel = \case
    UniqueIdEmpty     -> Warning
  getSource  = const HsBindgen
  getTraceId = const "unique-id"

checkUniqueId :: Tracer UniqueIdMsg -> UniqueId -> IO ()
checkUniqueId tracer (UniqueId val) = do
  when (null val) $
    traceWith tracer $ withCallStack UniqueIdEmpty
