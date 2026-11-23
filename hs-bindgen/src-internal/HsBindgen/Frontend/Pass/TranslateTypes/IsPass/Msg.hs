-- | Delayed trace messages for the @TranslateTypes@ pass
--
-- This module is intended to be imported unqualified.
module HsBindgen.Frontend.Pass.TranslateTypes.IsPass.Msg (
    DelayedTranslateTypesMsg (..)
  , FFITypesMsg (..)
  , logLevelFFITypesMsg
  ) where

import GHC.Generics (Generic)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Frontend.Pass.TranslateTypes.Position (FunPosition)
import HsBindgen.Frontend.Pass.TranslateTypes.SugarInfo
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  DelayedMsg
-------------------------------------------------------------------------------}

data DelayedTranslateTypesMsg =
    -- | Messages while deriving an FFI type for a macro declaration
    TranslateTypesMacroFFIType                 FFITypesMsg
    -- | Messages while deriving FFI types for a function declaration
  | TranslateTypesFunctionFFITypes FunPosition FFITypesMsg
  | TranslateTypesRegisterSugarInfoDuplicate   ErrorDuplicate
  | TranslateTypesRegisterSugarInfo            RegSugarInfo
  deriving stock (Show, Generic)

instance PrettyForTrace DelayedTranslateTypesMsg where
  prettyForTrace = \case
      TranslateTypesMacroFFIType msg ->
        PP.hang ("While deriving an FFI type for a macro declaration site:") 2 $
          prettyForTrace msg
      TranslateTypesFunctionFFITypes pos msg ->
        PP.hang ("While deriving an FFI type for a function declaration at" PP.<+> prettyForTrace pos PP.>< ":") 2 $
          prettyForTrace msg
      TranslateTypesRegisterSugarInfoDuplicate dup -> PP.vcat [
          "Tried to register sugar info more than once;"
        , "Old = " PP.>< PP.show dup.new
        , "New = " PP.>< PP.show dup.old
        ]
      TranslateTypesRegisterSugarInfo ty -> PP.hsep [
          "Registered sugar info:"
        , PP.show ty
        ]

instance IsTrace Level DelayedTranslateTypesMsg where
  getDefaultLogLevel = \case
    -- Deriving an FFI type for a macro declaration may fail for benign reasons.
    -- For example, a macro type that references an external struct type has no
    -- FFI type, but this will still cause an 'FFITypesExtBindingNotSpecified'
    -- message to be emitted. For this reason, these messages are always at
    -- Debug-level (for now).
    TranslateTypesMacroFFIType       _msg      -> Debug
    TranslateTypesFunctionFFITypes _ msg       -> logLevelFFITypesMsg msg
    TranslateTypesRegisterSugarInfoDuplicate{} -> Bug
    TranslateTypesRegisterSugarInfo{}          -> Debug
  getSource          = const HsBindgen
  getTraceId         = const "translate-types"

{-------------------------------------------------------------------------------
  FFITypesMsg
-------------------------------------------------------------------------------}

data FFITypesMsg =
    -- | An external binding did not specify an FFI type
    FFITypesExtBindingNotSpecified Hs.ExtRef
  | FFITypesLookupSugarInfo        (Hs.Name Hs.NsTypeConstr) SugarInfo
  deriving stock (Show, Generic)

instance PrettyForTrace FFITypesMsg where
  prettyForTrace = \case
      FFITypesExtBindingNotSpecified extRef -> PP.hsep [
          "The binding specification of"
        , prettyExtRef extRef
        , "specifies no FFI type."
        ]
      FFITypesLookupSugarInfo name ty -> PP.hsep [
          "Looked up sugar info for"
        , prettyForTrace name
        , ":"
        , PP.show ty
        ]
    where
      prettyExtRef :: Hs.ExtRef -> PP.CtxDoc
      prettyExtRef ref = PP.singleQuotes $ PP.hcat [
            prettyForTrace ref.moduleName
          , "."
          , PP.text ref.name.text
          ]

logLevelFFITypesMsg :: FFITypesMsg -> Level
logLevelFFITypesMsg = \case
    FFITypesExtBindingNotSpecified{} -> Warning
    FFITypesLookupSugarInfo _ ty     -> case ty of
      -- TODO <https://github.com/well-typed/hs-bindgen/issues/2376>:
      -- MissingSugarInfo should become Bug at some point when underlying types
      -- are completely removed
      MissingSugarInfo{}      -> Debug
      RegSugarInfo Sugar{}    -> Debug
      RegSugarInfo NotSugar{} -> Debug
