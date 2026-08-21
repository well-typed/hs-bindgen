-- | Calling conventions
--
-- Intended for unqualified import.
module HsBindgen.Backend.Hs.CallConv (
    CWrapper(..)
  , getCWrappersSource
  , CallConv(..)
  , ImportStyle(..)
  , capiModule
  ) where

import GHC.Generics (Generic)

import HsBindgen.Backend.Runtime qualified as Runtime
import HsBindgen.IR.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | The 'CallConvUserlandCapi' requires a wrapper on the C side with a
-- corresponding import.
newtype CWrapper = CWrapper {
      definition :: String
    }
  deriving (Show, Generic)

-- | Source of the C translation unit containing the wrappers
--
-- The wrappers are preceded by the /whole/ root header, verbatim and in order:
-- only that reproduces the environment in which the declarations were parsed,
-- including the interleaving of @#define@s and @#include@s.
--
-- Duplicate @#include@s are preserved deliberately: re-inclusion at a different
-- point in the sequence is meaningful (X-macro headers). A header that cannot
-- tolerate double inclusion fails the root header parse, before any C stage.
--
-- We emit nothing when there are no wrappers, so that a module without wrappers
-- does not gain a C translation unit.
getCWrappersSource :: [C.RootDirective C.HashIncludeArg] -> [CWrapper] -> String
getCWrappersSource _         [] = ""
getCWrappersSource directives wrappers =
    C.renderRootDirectives directives ++ concatMap (.definition) wrappers

data CallConv =
    -- | Our default calling convention: userland CAPI
    --
    -- There is no need to import the C header into the Haskell source file in
    -- this case (the C header is only used by the wrapper on the C side).
    --
    -- We directly attach the C-side wrappers.
    CallConvUserlandCapi CWrapper

    -- | The standard GHC @capi@ calling convention
    --
    -- Although the @capi@ calling convention technically supports by-reference
    -- imports, it does not actually do anything different than @ccall@ in this
    -- case, and ignores the C header. For this reason we only support @ccall@
    -- here.
    --
    -- NOTE: At the moment, we do not use the standard @capi@ calling
    -- convention. We used it for symbol imports, but now also create wrappers
    -- for those to work around bugs on Windows. We decided to leave the
    -- constructor in, in case we need it in the future.
  | CallConvGhcCapi FilePath

    -- | The standard GHC \"ccall\" calling convention
  | CallConvGhcCCall ImportStyle
  deriving stock (Generic, Show)

data ImportStyle =
    -- | Regular import
    ImportAsValue

    -- | @foreign import capi safe "header.h &foo" foo :: Ptr ..
  | ImportAsPtr
  deriving stock (Generic, Show)

-- | Module providing the userland CAPI support code.
capiModule :: Hs.ModuleName
capiModule = Runtime.moduleName Runtime.CAPI
