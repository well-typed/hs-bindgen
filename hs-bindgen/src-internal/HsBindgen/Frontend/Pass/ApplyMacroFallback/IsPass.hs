-- | Definitions for the @ApplyMacroFallback@ pass
--
-- This module is intended to be imported unqualified.
--
-- > import HsBindgen.Frontend.Pass.ApplyMacroFallback.IsPass
--
module HsBindgen.Frontend.Pass.ApplyMacroFallback.IsPass (
    ApplyMacroFallback
    -- * Configuration
  , MacroFallback(..)
  ) where

import HsBindgen.Frontend.Pass.Parse.IsPass (ReparseInfo, Tokens)
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.IR.Pass.Types (CoercePassAnonRef)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type ApplyMacroFallback :: Pass
data ApplyMacroFallback a

-- | Every macro is decided: typechecked, or exported as its raw token spellings
-- ('MacroBody' is 'TypecheckedMacro ApplyMacroFallback'). Other declarations
-- still carry their 'ReparseInfo' annotations.
type family AnnApplyMacroFallback (ix :: Symbol) :: Star where
  AnnApplyMacroFallback "IndirectField" = ReparseInfo Tokens
  AnnApplyMacroFallback "Function"      = ReparseInfo Tokens
  AnnApplyMacroFallback "Global"        = ReparseInfo Tokens
  AnnApplyMacroFallback "RegularField"  = ReparseInfo Tokens
  AnnApplyMacroFallback "Typedef"       = ReparseInfo Tokens
  AnnApplyMacroFallback _               = NoAnn

instance IsPass ApplyMacroFallback

instance PassId ApplyMacroFallback

instance PassScopedName ApplyMacroFallback

instance PassTypes ApplyMacroFallback

instance PassMacro ApplyMacroFallback where
  type MacroId   ApplyMacroFallback = Id ApplyMacroFallback
  type MacroBody ApplyMacroFallback = TypecheckedMacro ApplyMacroFallback

  macroIdId _ = id

instance PassExtBinding ApplyMacroFallback

instance PassCommentDecl ApplyMacroFallback where
  type CommentDecl ApplyMacroFallback = Maybe (C.Comment ApplyMacroFallback)

instance PassAnn ApplyMacroFallback where
  type Ann ix ApplyMacroFallback = AnnApplyMacroFallback ix

instance PassMsg ApplyMacroFallback

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Which macros the macro language gives up on are exported as their raw
-- macro (@HsBindgen.Runtime.Macro.Raw@)
--
-- The predicate is matched like the selection predicate.
--
-- Default: 'BFalse', no macro falls back.
newtype MacroFallback = MacroFallback {
      predicate :: Boolean SelectionPredicate
    }
  deriving stock (Show, Eq, Generic)

instance Default MacroFallback where
  def = MacroFallback BFalse

{-------------------------------------------------------------------------------
  CoercePass: TypecheckMacros → ApplyMacroFallback
-------------------------------------------------------------------------------}

instance CoercePassAnonRef          TypecheckMacros ApplyMacroFallback
instance CoercePassId               TypecheckMacros ApplyMacroFallback
instance CoercePassTypes            TypecheckMacros ApplyMacroFallback
instance CoercePassMacroId          TypecheckMacros ApplyMacroFallback
instance CoercePassMacroUnderlying  TypecheckMacros ApplyMacroFallback

instance CoercePassAnn "Function"      TypecheckMacros ApplyMacroFallback
instance CoercePassAnn "Global"        TypecheckMacros ApplyMacroFallback
instance CoercePassAnn "IndirectField" TypecheckMacros ApplyMacroFallback
instance CoercePassAnn "RegularField"  TypecheckMacros ApplyMacroFallback
instance CoercePassAnn "TypeFunArg"    TypecheckMacros ApplyMacroFallback
instance CoercePassAnn "Typedef"       TypecheckMacros ApplyMacroFallback
instance CoercePassCommentDecl         TypecheckMacros ApplyMacroFallback where
  coercePassCommentDecl _ = fmap coercePass
