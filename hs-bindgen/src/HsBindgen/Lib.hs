-- | Main entry point for using @hs-bindgen@-as-a-library
--
-- Intended for unqualified import.
module HsBindgen.Lib (
    -- * Specification
    Spec(..)
  , execSpec
    -- ** Prepare input
  , PrepareInput(..)
  , ClangArgs
    -- ** Translate
  , Translation(..)
  , HsModuleOpts(..)
    -- ** Process output
  , ProcessOutput(..)
  , HsRenderOpts(..)

    -- * Logging
  , Tracer
  , mkTracerIO

    -- * Type aliases
    --
    -- This types should be considered opaque.
  , CHeader
  , HsModule
  ) where

import Language.Haskell.Exts qualified as Hs

import HsBindgen.C.AST qualified as C
import HsBindgen.Hs.Annotation
import HsBindgen.Spec
import HsBindgen.Spec.Execution (execSpec)
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Type aliases
-------------------------------------------------------------------------------}

type CHeader  = C.Header
type HsModule = Hs.Module Ann