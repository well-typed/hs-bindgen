-- | Macro definitions the macro language gave up on
--
-- Until the @ApplyMacroFallback@ pass decides, every macro definition carries
-- its raw macro alongside the macro-language result, so that a macro the
-- language gives up on in a later stage can still fall back.
--
-- Intended for unqualified import.
module HsBindgen.Frontend.MacroFallback (
    PendingMacro(..)
  , MacroHandling(..)
  , nextStage
  , FallbackMacro(..)
  ) where

import HsBindgen.Runtime.Macro qualified as Runtime.Macro

import HsBindgen.Imports
import HsBindgen.Macro.Error

-- | A macro definition before the fallback decision
--
-- Used as @'HsBindgen.IR.Pass.MacroBody' p l@, so the order of type parameters
-- is fixed.
--
-- A pending macro is "usable", even when the macro language gave up on it;
-- see 'HsBindgen.Frontend.Analysis.DeclIndex.UsableEntry'.
data PendingMacro body l = PendingMacro {
      -- | Token spellings of the raw macro
      raw      :: Runtime.Macro.Raw Text
    , handling :: MacroHandling body l
    }
  deriving stock (Generic)

deriving stock instance Show (body l) => Show (PendingMacro body l)
deriving stock instance Eq   (body l) => Eq   (PendingMacro body l)

data MacroHandling body l =
    Handled (body l)
    -- | The macro language gave up on the macro
  | Unhandled MacroLangFailure
  deriving stock (Generic)

deriving stock instance Show (body l) => Show (MacroHandling body l)
deriving stock instance Eq   (body l) => Eq   (MacroHandling body l)

-- | Run the next stage of the macro language on a macro it still handles
nextStage ::
     (e -> MacroLangFailure)
     -- ^ Records the stage that gave up
  -> (body l -> Either e (body' l))
  -> MacroHandling body l
  -> MacroHandling body' l
nextStage tag stage = \case
    Handled x         -> either (Unhandled . tag) Handled (stage x)
    Unhandled failure -> Unhandled failure

-- | A macro exported as its raw token spellings
data FallbackMacro = FallbackMacro {
      raw     :: Runtime.Macro.Raw Text
    , failure :: MacroLangFailure
    }
  deriving stock (Show, Eq, Generic)
