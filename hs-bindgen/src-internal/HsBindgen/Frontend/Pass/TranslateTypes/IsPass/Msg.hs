-- | Delayed trace messages for the @TranslateTypes@ pass
--
-- This module is intended to be imported unqualified.
module HsBindgen.Frontend.Pass.TranslateTypes.IsPass.Msg (
    DelayedTranslateTypesMsg (..)
  , prettyExtBindingFallback
  ) where

import GHC.Generics (Generic)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Frontend.Pass.TranslateTypes.FFIType (ExtBindingFallback (..))
import HsBindgen.Frontend.Pass.TranslateTypes.Position (FunPosition)
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  DelayedMsg
-------------------------------------------------------------------------------}

data DelayedTranslateTypesMsg =
    -- | An external binding passed directly has no FFI type in its binding
    -- specification
    TranslateTypesExtBindingFallback FunPosition ExtBindingFallback
  deriving stock (Show, Generic)

instance PrettyForTrace DelayedTranslateTypesMsg where
  prettyForTrace = \case
      TranslateTypesExtBindingFallback pos fallback ->
        prettyExtBindingFallback (prettyForTrace pos) fallback

instance IsTrace Level DelayedTranslateTypesMsg where
  getDefaultLogLevel = \case
    TranslateTypesExtBindingFallback{} -> Warning
  getSource          = const HsBindgen
  getTraceId         = const "translate-types"

-- | Render an external binding without FFI type
prettyExtBindingFallback ::
     PP.CtxDoc -- ^ The position that uses the external binding
  -> ExtBindingFallback
  -> PP.CtxDoc
prettyExtBindingFallback pos fallback = PP.vcat [
      PP.hsep [
          "The binding specification of"
        , prettyExtRef fallback.extRef
        , "gives no FFI type;"
        , pos
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
