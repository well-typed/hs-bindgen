-- | Trace messages for the @TranslateMacros@ pass
--
-- This module is intended to be imported unqualified.
module HsBindgen.Frontend.Pass.TranslateMacros.IsPass.Msg (
    -- * Msg
    TranslateMacrosMsg
    -- * DelayedMsg
  , DelayedTranslateMacrosMsg
  ) where

import GHC.Generics (Generic)

import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Msg
-------------------------------------------------------------------------------}

data TranslateMacrosMsg
  deriving stock Show

instance PrettyForTrace TranslateMacrosMsg where
  prettyForTrace = \case

instance IsTrace Level TranslateMacrosMsg where
  getDefaultLogLevel = \case
  getSource          = const HsBindgen
  getTraceId         = const "translate-macros"

{-------------------------------------------------------------------------------
  DelayedMsg
-------------------------------------------------------------------------------}

data DelayedTranslateMacrosMsg
  deriving stock (Show, Generic)

instance PrettyForTrace DelayedTranslateMacrosMsg where
  prettyForTrace = \case

instance IsTrace Level DelayedTranslateMacrosMsg where
  getDefaultLogLevel = \case
  getSource          = const HsBindgen
  getTraceId         = const "translate-macros"
