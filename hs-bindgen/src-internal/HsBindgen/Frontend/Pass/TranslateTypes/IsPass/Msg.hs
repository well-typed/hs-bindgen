-- | Delayed trace messages for the @TranslateTypes@ pass
--
-- This module is intended to be imported unqualified.
module HsBindgen.Frontend.Pass.TranslateTypes.IsPass.Msg (
    DelayedTranslateTypesMsg (..)
  ) where

import GHC.Generics (Generic)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Frontend.Pass.TranslateTypes.Error (FunPosition)
import HsBindgen.Frontend.Pass.TranslateTypes.FFIType (ExtBindingFallback (..))
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  DelayedMsg
-------------------------------------------------------------------------------}

data DelayedTranslateTypesMsg =
    -- | An external binding passed by value has no FFI type in its binding
    -- specification
    TranslateTypesExtBindingFallback FunPosition ExtBindingFallback
  deriving stock (Show, Generic)

instance PrettyForTrace DelayedTranslateTypesMsg where
  prettyForTrace = \case
      TranslateTypesExtBindingFallback pos fallback -> PP.vcat [
          PP.hsep [
              "The binding specification of"
            , prettyExtRef fallback.extRef
            , "gives no FFI type;"
            , prettyForTrace pos
            , "uses FFI type"
            , prettyExtRef (Hs.ffiExtRef fallback.assumed)
            ]
        , "This requires a matching `HasFFIType` instance."
        ]
    where
      prettyExtRef :: Hs.ExtRef -> PP.CtxDoc
      prettyExtRef ref = PP.singleQuotes $ PP.hcat [
            prettyForTrace ref.moduleName
          , "."
          , PP.text ref.name.text
          ]

instance IsTrace Level DelayedTranslateTypesMsg where
  getDefaultLogLevel = \case
    TranslateTypesExtBindingFallback{} -> Warning
  getSource          = const HsBindgen
  getTraceId         = const "translate-types"
