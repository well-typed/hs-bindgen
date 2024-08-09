-- | Specification of what @hs-bindgen@ is meant to do
--
-- This is split into three parts, with separate interpreters:
--
-- > prepareInput  :: PrepareInput  inp     -> IO inp
-- > translate     :: Translation   inp out ->    inp -> out
-- > processOutput :: ProcessOutput     out ->           out -> result
--
-- where the bulk of the logic lives in 'translate', which is a pure function.
module HsBindgen.Spec (
    Spec(..)
    -- * Prepare input
  , PrepareInput(..)
  , ClangArgs
    -- * Translate
  , Translation(..)
  , HsModuleOpts(..)
    -- * Process output
  , ProcessOutput(..)
  , HsRenderOpts(..)
  ) where

import Language.Haskell.Exts qualified as Hs
import Language.Haskell.TH (Q)
import Language.Haskell.TH qualified as TH

import HsBindgen.C.AST qualified as C
import HsBindgen.Hs.Annotation
import HsBindgen.Hs.Render (HsRenderOpts(..))
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Top-level specification
-------------------------------------------------------------------------------}

data Spec result where
  Preprocess ::
       PrepareInput inp
    -> Translation inp out
    -> ProcessOutput out
    -> Spec (IO ())

  GenSplice ::
       PrepareInput inp
    -> Translation inp [Hs.Decl Ann]
    -> Spec (Q [TH.Dec])

{-------------------------------------------------------------------------------
  Prepare the input

  TODO: <https://github.com/well-typed/hs-bindgen/issues/10>
  This needs to include parameters for cross compilation.

  TODO: <https://github.com/well-typed/hs-bindgen/issues/71>
  This needs to have fields with paths, preprocessor defines, etc.

  TODO: <https://github.com/well-typed/hs-bindgen/issues/75>
  Support multiple C headers.
-------------------------------------------------------------------------------}

data PrepareInput inp where
  ParseCHeader ::
       Tracer IO String
    -> ClangArgs -> FilePath -> PrepareInput C.Header

-- | @libclang@ command line arguments
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/83>
-- We should have a proper data type instead of @[String]@ for the arguments
-- (part of #10 and #71).
type ClangArgs = [String]

{-------------------------------------------------------------------------------
  Translation (this should be a pure function)
-------------------------------------------------------------------------------}

data Translation inp out where
  ParseOnly  :: Translation C.Header C.Header
  GenDecls   :: Translation C.Header [Hs.Decl Ann]
  GenModule  :: HsModuleOpts -> Translation C.Header (Hs.Module Ann)

data HsModuleOpts = HsModuleOpts {
      hsModuleName :: String
    }

{-------------------------------------------------------------------------------
  Process the output

  TODO: <https://github.com/well-typed/hs-bindgen/issues/75>
  We might want to support generating multiple Haskell modules.
-------------------------------------------------------------------------------}

data ProcessOutput out where
  PrettyC  :: ProcessOutput C.Header
  PrettyHs :: HsRenderOpts -> Maybe FilePath -> ProcessOutput (Hs.Module Ann)
