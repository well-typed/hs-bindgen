-- | Trace messages for the @TranslateMacros@ pass
--
-- This module is intended to be imported unqualified.
module HsBindgen.Frontend.Pass.TranslateMacros.IsPass.Msg (
    -- * Msg
    TranslateMacrosMsg
    -- * DelayedMsg
  , DelayedTranslateMacrosMsg (..)
  ) where

import GHC.Generics (Generic)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Frontend.Pass.TranslateMacros.UnderlyingType
import HsBindgen.IR.C qualified as C
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

data DelayedTranslateMacrosMsg =
    TranslateMacrosRegisterUnderlyingTypeDuplicate ErrorDuplicate
  | TranslateMacrosRegisterUnderlyingType          RegisteredUnderlyingType
  | TranslateMacrosLookupUnderlyingType            C.DeclId UnderlyingType
  deriving stock (Show, Generic)

instance PrettyForTrace DelayedTranslateMacrosMsg where
  prettyForTrace = \case
    TranslateMacrosRegisterUnderlyingTypeDuplicate dup -> PP.hsep [
        "Tried to register underlying type information more than once. Old = "
      , PP.show dup.new
      , "; New = "
      , PP.show dup.old
      ]
    TranslateMacrosRegisterUnderlyingType ty -> PP.hsep [
        "Registered underlying type information:"
      , PP.show ty
      ]
    TranslateMacrosLookupUnderlyingType declId ty -> PP.hsep [
        "Looked up underlying type information for"
      , prettyForTrace declId
      , ":"
      , PP.show ty
      ]

instance IsTrace Level DelayedTranslateMacrosMsg where
  getDefaultLogLevel = \case
    TranslateMacrosRegisterUnderlyingTypeDuplicate{} -> Bug
    TranslateMacrosRegisterUnderlyingType{}          -> Debug
    TranslateMacrosLookupUnderlyingType _ ty         -> case ty of
      MissingUnderlyingType{}                     -> Bug
      RegisteredUnderlyingType UnderlyingType{}   -> Debug
      RegisteredUnderlyingType NoUnderlyingType{} -> Debug
  getSource          = const HsBindgen
  getTraceId         = const "translate-macros"
