-- | Calling conventions
--
-- Intended for unqualified import.
module HsBindgen.Backend.Hs.CallConv (
    CWrapper(..)
  , getCWrappersSource
  , CallConv(..)
  , ImportStyle(..)
  ) where

import GHC.Generics (Generic)

import HsBindgen.Frontend.RootHeader

import Witherable (ordNub)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | The 'CallConvUserlandCAPI' requires a wrapper on the C side with a
-- corresponding import.
data CWrapper = CWrapper {
      definition     :: String
    , hashIncludeArg :: HashIncludeArg
    }
  deriving (Show, Generic)

getCWrappersSource :: [CWrapper] -> String
getCWrappersSource wrappers = unlines $ headers ++ bodies
    where
      getImport :: CWrapper -> String
      getImport wrapper = "#include <" ++ wrapper.hashIncludeArg.path ++ ">"

      headers, bodies :: [String]
      -- It is important that we don't include the same header more than once,
      -- /especially/ for non-extern non-static globals.
      headers = ordNub $ map getImport wrappers
      bodies = map (.definition) wrappers

data CallConv =
    -- | Our default calling convention: userland CAPI
    --
    -- There is no need to import the C header into the Haskell source file in
    -- this case (the C header is only used by the wrapper on the C side).
    --
    -- We directly attach the C-side wrappers.
    CallConvUserlandCAPI CWrapper

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
  | CallConvGhcCAPI FilePath

    -- | The standard GHC \"ccall\" calling convention
  | CallConvGhcCCall ImportStyle
  deriving stock (Generic, Show)

data ImportStyle =
    -- | Regular import
    ImportAsValue

    -- | @foreign import capi safe "header.h &foo" foo :: Ptr ..
  | ImportAsPtr
  deriving stock (Generic, Show)
