-- | Trace messages for the @TranslateTypes@ pass
--
-- This module is intended to be imported unqualified.
module HsBindgen.Frontend.Pass.TranslateTypes.IsPass.Msg (
    -- * Msg
    TranslateTypesMsg
    -- * DelayedMsg
  , DelayedTranslateTypesMsg
  ) where

import GHC.Generics (Generic)

import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Msg
-------------------------------------------------------------------------------}

data TranslateTypesMsg
  deriving stock Show

instance PrettyForTrace TranslateTypesMsg where
  prettyForTrace = \case

instance IsTrace Level TranslateTypesMsg where
  getDefaultLogLevel = \case
  getSource          = const HsBindgen
  getTraceId         = const "translate-types"

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
