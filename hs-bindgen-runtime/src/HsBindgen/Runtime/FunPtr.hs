-- | Function pointer utilities and type classes with pre-generated instances.
--
-- This module provides the 'ToFunPtr' and 'FromFunPtr' type classes along with
-- Template Haskell generated instances for common function signatures.
--
-- Users should import this module to get both the type classes and the instances.
--
module HsBindgen.Runtime.FunPtr (
    -- * Re-exports from "HsBindgen.Runtime.FunPtr.Class"
    ToFunPtr (..)
  , FromFunPtr (..)
  , withFunPtr
    -- * Template Haskell generated instances
    --
    -- The instances are defined in "HsBindgen.Runtime.TH.Instances"
  , module HsBindgen.Runtime.TH.Instances
  ) where

import HsBindgen.Runtime.FunPtr.Class
import HsBindgen.Runtime.TH.Instances
