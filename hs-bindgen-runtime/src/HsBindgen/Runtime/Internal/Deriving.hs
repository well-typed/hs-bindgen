-- | When @hs-bindgen@ generates code that uses @deriving via@, the constructors
--   of the used type need to be in scope.
--
-- Otherwise, we get errors of the form
--
-- @
-- • Couldn't match representation of type ‘Data.Array.Byte.ByteArray’
--                            with that of ‘HsBindgen.Runtime.SizedByteArray.SizedByteArray 8 8’
--     arising from a use of ‘GHC.Prim.coerce’
--   The data constructor ‘HsBindgen.Runtime.SizedByteArray.SizedByteArray’
--     of newtype ‘HsBindgen.Runtime.SizedByteArray.SizedByteArray’
--     is not in scope
-- @
--
-- This module provides these constructors. It is re-exported from the public
-- API of `hs-bindgen`; in particular, "HsBindgen.TH".
module HsBindgen.Runtime.Internal.Deriving (
    EquivStorable(..)
  , SizedByteArray(..)
  ) where

import HsBindgen.Runtime.Internal.SizedByteArray
import HsBindgen.Runtime.Marshal
