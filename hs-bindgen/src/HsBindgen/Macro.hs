-- | The macro instance of @hs-bindgen@ is based on @c-expr-dsl@.
--
-- The macro language is not yet stable, and so all definitions are exported as
-- opaque.
--
-- Intended for qualified import.
--
-- @
-- import HsBindgen.Macro (CExpr)
-- import HsBindgen.Macro qualified as Macro
-- @
module HsBindgen.Macro (
    -- * Macro language interface
    Macro.Lang  -- opaque
    -- * Integration with @c-expr@
  , CExpr       -- opaque
  , Macro.cExpr -- opaque
  ) where

import HsBindgen.Internal.Macro (CExpr)
import HsBindgen.Internal.Macro qualified as Macro
