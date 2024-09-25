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
  , Predicate(..)
  , parseCHeader

    -- ** Clang arguments
  , ClangArgs(..)
  , CStandard(..)
  , defaultClangArgs

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
  , Preprocess(..)
  , preprocess

    -- * Debugging
  , Element(..)
  , getClangAST
  , getComments
  , getTargetTriple
  , genHaskell

    -- * Logging
  , Tracer
  , nullTracer
  , contramap
  , PrettyLogMsg(..)
  , mkTracer
  , mkTracerIO
  , traceThrow
  ) where

import Data.Text (Text)
import Data.Tree (Forest)
import GHC.Generics (Generic)
import Language.Haskell.Exts qualified as E
import Language.Haskell.TH qualified as TH
import Text.Show.Pretty qualified as Pretty

import HsBindgen.Backend.HsSrcExts (Ann)
import HsBindgen.Backend.HsSrcExts.Render (HsRenderOpts(..))
import HsBindgen.Backend.HsSrcExts.Render qualified as Backend.E
import HsBindgen.Backend.HsSrcExts.Translation (HsModuleOpts(..))
import HsBindgen.Backend.HsSrcExts.Translation qualified as Backend.E
import HsBindgen.Backend.TH.Translation qualified as Backend.TH
import HsBindgen.C.AST qualified as C
import HsBindgen.C.Parser (ParseMsg, Element(..))
import HsBindgen.C.Parser qualified as C
import HsBindgen.C.Predicate (Predicate(..))
import HsBindgen.Clang.Args
import HsBindgen.Clang.Util.Diagnostics qualified as C (Diagnostic)
import HsBindgen.Clang.Util.SourceLoc
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Translation.LowLevel qualified as LowLevel
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Type aliases

  These newtypes ensure that these types are opaque in client code. The precise
  nature of the C AST and Haskell AST is not part of the public API.
-------------------------------------------------------------------------------}

newtype CHeader = WrapCHeader {
      unwrapCHeader :: C.Header
    }
  deriving (Eq, Generic)

newtype HsModule = WrapHsModule {
      unwrapHsModule :: E.Module Ann
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
     Tracer IO C.Diagnostic
  -> Tracer IO C.ParseMsg
  -> Predicate
  -> ClangArgs
  -> FilePath -> IO CHeader
parseCHeader traceWarnings traceParseMsgs p args fp =
    fmap (WrapCHeader . C.Header) $
      C.parseHeaderWith traceWarnings args fp $
        C.foldDecls traceParseMsgs p

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

genModule :: HsModuleOpts -> CHeader -> HsModule
genModule opts = WrapHsModule . Backend.E.translate opts . unwrapCHeader

genDecls :: TH.Quote q => CHeader -> q [TH.Dec]
genDecls = Backend.TH.translateC . unwrapCHeader

{-------------------------------------------------------------------------------
  Processing output

  TODO: <https://github.com/well-typed/hs-bindgen/issues/75>
  We might want to support generating multiple Haskell modules.
-------------------------------------------------------------------------------}

prettyC :: CHeader -> IO ()
prettyC = Pretty.dumpIO . unwrapCHeader

prettyHs :: HsRenderOpts -> Maybe FilePath -> HsModule -> IO ()
prettyHs opts fp = Backend.E.renderIO opts fp . unwrapHsModule

{-------------------------------------------------------------------------------
  Common pipelines
-------------------------------------------------------------------------------}

data Preprocess = Preprocess {
      -- | Tracer for warnings from @libclang@
      preprocessTraceWarnings :: Tracer IO C.Diagnostic

      -- | Tracer for /our/ C parser
    , preprocessTraceParseMsgs :: Tracer IO C.ParseMsg

      -- | Select definitions
    , preprocessPredicate :: Predicate

      -- | @libclang@ options
    , preprocessClangArgs :: ClangArgs

      -- | Path to the C header
    , preprocessInputPath :: FilePath

      -- | Options for the Haskell module generation
    , preprocessModuleOpts :: HsModuleOpts

      -- | Options for rendering generated Haskell
    , preprocessRenderOpts :: HsRenderOpts

      -- | Name of the Haskell file (none for @stdout@)
    , preprocessOutputPath :: Maybe FilePath
    }

preprocess :: Preprocess -> IO ()
preprocess prep = do
    modl <- genModule (preprocessModuleOpts prep) <$>
              parseCHeader
                (preprocessTraceWarnings  prep)
                (preprocessTraceParseMsgs prep)
                (preprocessPredicate      prep)
                (preprocessClangArgs      prep)
                (preprocessInputPath      prep)
    prettyHs (preprocessRenderOpts prep) (preprocessOutputPath prep) modl

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

-- | Return the raw @libclang@ AST
--
-- This is primarily for debugging.
getClangAST ::
     Tracer IO C.Diagnostic
  -> Predicate
  -> ClangArgs
  -> FilePath
  -> IO (Forest Element)
getClangAST tracer predicate args fp =
    C.parseHeaderWith tracer args fp $
      C.foldClangAST predicate

-- | Get comments as HTML for all top-level declarations
--
-- For now this is primarily for debugging, but perhaps this could be made part
-- of the library proper.
getComments ::
     Tracer IO C.Diagnostic
  -> Predicate
  -> ClangArgs
  -> FilePath
  -> IO (Forest (SourceLoc, Text, Maybe Text))
getComments tracer predicate args fp =
    C.parseHeaderWith tracer args fp $
      C.foldComments predicate

-- | Return the target triple for translation unit
getTargetTriple ::
     Tracer IO C.Diagnostic
  -> ClangArgs
  -> FilePath
  -> IO Text
getTargetTriple tracer args fp =
    C.withTranslationUnit tracer args fp $
      C.getTranslationUnitTargetTriple

-- | Generate our internal Haskell representation of the translated C header
genHaskell :: CHeader -> [Hs.Decl f]
genHaskell = LowLevel.generateDeclarations . unwrapCHeader


