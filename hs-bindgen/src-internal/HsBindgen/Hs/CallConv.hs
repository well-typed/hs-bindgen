-- | Calling conventions
--
-- Intended for unqualified import.
module HsBindgen.Hs.CallConv (
    CallConv(..)
  , ImportStyle(..)
  ) where

import GHC.Generics (Generic)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data CallConv =
    -- | Our default calling convention: userland CAPI
    --
    -- The header is not required in this case (it is only used by the wrapper).
    CallConvUserlandCAPI

    -- | The standard GHC @capi@ calling convention
    --
    -- Although @capi@ technically supports by-reference imports, it does not
    -- actually do anything different than @ccall@ in this case, and ignores the
    -- header. For this reason we only support @ccall@ here.
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
