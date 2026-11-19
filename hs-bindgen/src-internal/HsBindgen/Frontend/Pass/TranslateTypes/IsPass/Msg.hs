-- | Delayed trace messages for the @TranslateTypes@ pass
--
-- This module is intended to be imported unqualified.
module HsBindgen.Frontend.Pass.TranslateTypes.IsPass.Msg (
    DelayedTranslateTypesMsg (..)
  , FFITypesMsg (..)
  , ExtBindingFallback (..)
  ) where

import GHC.Generics (Generic)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Frontend.Pass.TranslateTypes.Error (FunPosition)
import HsBindgen.Frontend.Pass.TranslateTypes.SugarInfo
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  DelayedMsg
-------------------------------------------------------------------------------}

data DelayedTranslateTypesMsg =
    TranslateTypesFFITypes                   FunPosition FFITypesMsg
  | TranslateTypesRegisterSugarInfoDuplicate ErrorDuplicate
  | TranslateTypesRegisterSugarInfo          RegSugarInfo
  deriving stock (Show, Generic)

instance PrettyForTrace DelayedTranslateTypesMsg where
  prettyForTrace = \case
      TranslateTypesFFITypes pos msg ->
        PP.hang ("While deriving an FFI type at" PP.<+> prettyForTrace pos PP.>< ":") 2 $
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
    TranslateTypesFFITypes _ msg               -> logLevelFFITypesMsg msg
    TranslateTypesRegisterSugarInfoDuplicate{} -> Bug
    TranslateTypesRegisterSugarInfo{}          -> Debug
  getSource          = const HsBindgen
  getTraceId         = const "translate-types"

{-------------------------------------------------------------------------------
  FFITypesMsg
-------------------------------------------------------------------------------}

data FFITypesMsg =
    -- | An external binding passed directly has no FFI type in its binding
    -- specification
    FFITypesExtBindingFallback ExtBindingFallback
  | FFITypesLookupSugarInfo    (Hs.Name Hs.NsTypeConstr) SugarInfo
  deriving stock (Show, Generic)

instance PrettyForTrace FFITypesMsg where
  prettyForTrace = \case
      FFITypesExtBindingFallback fallback -> PP.vcat [
          PP.hsep [
              "The binding specification of"
            , prettyExtRef fallback.extRef
            , "gives no FFI type."
            ]
        , PP.hsep [
              "Falling back to "
            , prettyExtRef (Hs.ffiExtRef fallback.assumed)
            ]
        , "This requires a matching `HasFFIType` instance."
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
    FFITypesExtBindingFallback{} -> Warning
    FFITypesLookupSugarInfo _ ty -> case ty of
      -- TODO: MissingSugarInfo should become Bug at some point when underlying
      -- types are completely removed
      MissingSugarInfo{}      -> Debug
      RegSugarInfo Sugar{}    -> Debug
      RegSugarInfo NotSugar{} -> Debug

-- | An external binding whose binding specification gives no FFI type
--
-- We assume the FFI type of its underlying type instead. The generated code
-- compiles only if the external type has a @HasFFIType@ instance with that
-- FFI type.
data ExtBindingFallback = ExtBindingFallback {
      extRef  :: Hs.ExtRef
      -- | The FFI type of the underlying type
    , assumed :: Hs.FFIType
    }
  deriving stock (Show, Eq)
