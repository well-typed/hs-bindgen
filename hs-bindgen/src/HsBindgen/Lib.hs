-- | Main entry point for using @hs-bindgen@-as-a-library
--
-- This is split into three phases:
--
-- * Input preparation (e.g. parsing of C headers)
-- * Translation from C to Haskell
-- * Output processing (e.g. writing Haskell files)
--
-- The bulk of the work happens in translation (which is a pure function).
--
-- Intended for unqualified import.
--
-- NOTE: Client code should /NOT/ have to import from @hs-bindgen-libclang@;
-- that library should be considered internal to @hs-bindgen@.
module HsBindgen.Lib (
    -- * Prepare input
    CHeader   -- opaque
  , ParseMsg  -- opaque
  , ClangArgs
  , parseCHeader

    -- * Translation
  , HsModuleOpts(..)
  , HsModule -- opaque
  , genModule
  , genDecls

    -- * Process output
  , HsRenderOpts(..)
  , prettyHs
  , prettyC

    -- * Common pipelines
  , preprocess

    -- * Debugging
  , Element(..)
  , getClangAST

    -- * Logging
  , Tracer
  , contramap
  , PrettyLogMsg(..)
  , mkTracerIO
  ) where

import Data.Tree (Forest)
import Language.Haskell.Exts qualified as Hs
import Language.Haskell.Meta qualified as Meta
import Language.Haskell.TH qualified as TH
import Text.Show.Pretty qualified as Pretty

import HsBindgen.C.AST qualified as C
import HsBindgen.C.Parser (ParseMsg, Element(..))
import HsBindgen.C.Parser qualified as C
import HsBindgen.Clang.Args
import HsBindgen.Hs.Annotation
import HsBindgen.Hs.Render (HsRenderOpts(..))
import HsBindgen.Hs.Render qualified as Hs
import HsBindgen.Translation.LowLevel
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Type aliases

  These newtypes ensure that these types are opaque in client code. The precise
  nature of the C AST and Haskell AST is not part of the public API.
-------------------------------------------------------------------------------}

newtype CHeader = WrapCHeader {
      unwrapCHeader :: C.Header
    }

newtype HsModule = WrapHsModule {
      unwrapHsModule :: Hs.Module Ann
    }

{-------------------------------------------------------------------------------
  Prepare input

  TODO: <https://github.com/well-typed/hs-bindgen/issues/10>
  This needs to include parameters for cross compilation.

  TODO: <https://github.com/well-typed/hs-bindgen/issues/71>
  This needs to have fields with paths, preprocessor defines, etc.

  TODO: <https://github.com/well-typed/hs-bindgen/issues/75>
  Support multiple C headers.
-------------------------------------------------------------------------------}

-- | Parse C header
parseCHeader ::
     Tracer IO C.ParseMsg
  -> ClangArgs
  -> FilePath -> IO CHeader
parseCHeader tracer args fp =
    WrapCHeader . C.Header <$> C.parseHeaderWith args fp (C.foldDecls tracer)

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

genModule :: HsModuleOpts -> CHeader -> HsModule
genModule opts = WrapHsModule . generateModule opts . unwrapCHeader

genDecls :: CHeader -> [TH.Dec]
genDecls = map Meta.toDec . generateDeclarations . C.headerDecls . unwrapCHeader

{-------------------------------------------------------------------------------
  Processing output

  TODO: <https://github.com/well-typed/hs-bindgen/issues/75>
  We might want to support generating multiple Haskell modules.
-------------------------------------------------------------------------------}

prettyC :: CHeader -> IO ()
prettyC = Pretty.dumpIO . unwrapCHeader

prettyHs :: HsRenderOpts -> Maybe FilePath -> HsModule -> IO ()
prettyHs opts fp = Hs.renderIO opts fp . unwrapHsModule

{-------------------------------------------------------------------------------
  Common pipelines
-------------------------------------------------------------------------------}

preprocess ::
     Tracer IO C.ParseMsg  -- ^ Tracer for the C parser
  -> ClangArgs             -- ^ @libclang@ options
  -> FilePath              -- ^ Path to the C header
  -> HsModuleOpts          -- ^ Options for the Haskell module generation
  -> Hs.HsRenderOpts       -- ^ Options for rendering the generated Haskell code
  -> Maybe FilePath        -- ^ Name of the Haskell file (none for @stdout@)
  -> IO ()
preprocess tracer clangArgs inp modOpts renderOpts out = do
    modl <- genModule modOpts <$> parseCHeader tracer clangArgs inp
    prettyHs renderOpts out modl

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

-- | Return the raw @libclang@ AST
--
-- This is primarily for debugging.
getClangAST :: ClangArgs -> FilePath -> IO (Forest Element)
getClangAST args fp = C.parseHeaderWith args fp C.foldClangAST
