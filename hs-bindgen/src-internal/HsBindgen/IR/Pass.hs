-- | Pass
--
-- The IR varies per pass.
--
-- From outside the @HsBindgen.IR@ hierarchy, this module should always be used,
-- /not/ submodules.  Submodules need to be used within the @HsBindgen.IR@
-- hierarchy to avoid import cycles.
--
-- Intended for unqualified import.
--
-- > import HsBindgen.IR.Pass
module HsBindgen.IR.Pass (
    -- * Definition
    Pass
    -- * Associated type families
  , IsPass
  , PassId(..)
  , PassScopedName(..)
  , PassMacro(..)
  , PassExtBinding(..)
  , PassCommentDecl(..)
  , PassAnn(..)
  , PassMsg(..)
  , AMsg
    -- * Coercion
  , CoercePass(..)
  , CoercePassParam(..)
  , CoercePassId(..)
  , CoercePassMacroId(..)
  , CoercePassMacroBody(..)
  , CoercePassMacroUnderlying(..)
  , CoercePassCommentDecl(..)
  , CoercePassAnn(..)
    -- * Defaults
  , NoAnn(..)
  , NoMsg
  ) where

import HsBindgen.IR.Pass.Ann
import HsBindgen.IR.Pass.CommentDecl
import HsBindgen.IR.Pass.Definition
import HsBindgen.IR.Pass.ExtBinding
import HsBindgen.IR.Pass.Id
import HsBindgen.IR.Pass.Macro
import HsBindgen.IR.Pass.Msg
import HsBindgen.IR.Pass.ScopedName

{-------------------------------------------------------------------------------
  Associated type families
-------------------------------------------------------------------------------}

-- | Class alias for all pass classes
--
-- An instance is /not/ declared in this module.  Each pass should explicilty
-- declare the instance, so that an error is shown at that location if any
-- @Pass*@ instances are missing.
class (
      PassAnn         p
    , PassCommentDecl p
    , PassId          p
    , PassExtBinding  p
    , PassMacro       p
    , PassMsg         p
    , PassScopedName  p
    ) => IsPass (p :: Pass)
