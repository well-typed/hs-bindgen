{-# OPTIONS_HADDOCK hide #-}

-- | Function pointer utilities and type classes with pre-generated instances.
--
-- This module provides the 'ToFunPtr' and 'FromFunPtr' type classes along with
-- Template Haskell generated instances for common function signatures.
--
-- NOTE: For now, this module is classified "Support" because the definitions
-- are re-exported from the runtime prelude. Should we add definitions intended
-- for qualified import, we need to add a public module.
module HsBindgen.Runtime.Support.FunPtr (
    -- * Re-exports from "HsBindgen.Runtime.FunPtr.Class"
    ToFunPtr (..)
  , FromFunPtr (..)
  , withFunPtr
  , withFunPtrAs
  ) where

import HsBindgen.Runtime.Support.FunPtr.Class
import HsBindgen.Runtime.Support.TH.Instances ()
