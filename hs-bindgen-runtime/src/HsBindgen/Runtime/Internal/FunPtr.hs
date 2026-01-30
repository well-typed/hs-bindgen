-- NOTE: For now, this module is classified "Internal" because the definitions
-- are re-exported from the runtime prelude. Should we add definitions intended
-- for qualified import, we need to add a public module.

-- | Function pointer utilities and type classes with pre-generated instances.
--
-- This module provides the 'ToFunPtr' and 'FromFunPtr' type classes along with
-- Template Haskell generated instances for common function signatures.
--
-- Users should import this module to get both the type classes and the instances.
--
module HsBindgen.Runtime.Internal.FunPtr (
    -- * Re-exports from "HsBindgen.Runtime.FunPtr.Class"
    ToFunPtr (..)
  , FromFunPtr (..)
  , withFunPtr
    -- * Template Haskell generated instances
    --
    -- The instances are defined in "HsBindgen.Runtime.TH.Instances"
  , module HsBindgen.Runtime.TH.Instances
  ) where

import HsBindgen.Runtime.Internal.FunPtr.Class
import HsBindgen.Runtime.TH.Instances
