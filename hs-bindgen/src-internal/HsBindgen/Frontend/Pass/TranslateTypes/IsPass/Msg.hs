-- | Delayed trace messages for the @TranslateTypes@ pass
--
-- This module is intended to be imported unqualified.
module HsBindgen.Frontend.Pass.TranslateTypes.IsPass.Msg (
    DelayedTranslateTypesMsg
  ) where

import GHC.Generics (Generic)

import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  DelayedMsg
-------------------------------------------------------------------------------}

data DelayedTranslateTypesMsg
  deriving stock (Show, Generic)

instance PrettyForTrace DelayedTranslateTypesMsg where
  prettyForTrace = \case

instance IsTrace Level DelayedTranslateTypesMsg where
  getDefaultLogLevel = \case
  getSource          = const HsBindgen
  getTraceId         = const "translate-types"
