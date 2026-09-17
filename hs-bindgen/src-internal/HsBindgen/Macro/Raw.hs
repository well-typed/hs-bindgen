-- | The 'Raw' macro language.
--
-- Every macro is a value-like macro.
--
-- Translate all macros to their token spellings, as a
-- 'HsBindgen.Runtime.Macro.Raw'.
--
-- Intended for unqualified import.
module HsBindgen.Macro.Raw (
    Raw -- opaque
  , raw -- opaque
  ) where

import HsBindgen.Macro.Raw.Lang
import HsBindgen.Macro.Raw.Parse
