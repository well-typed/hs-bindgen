-- | Delayed trace messages for the @TranslateTypes@ pass
--
-- This module is intended to be imported unqualified.
module HsBindgen.Frontend.Pass.TranslateTypes.IsPass.Msg (
    DelayedTranslateTypesMsg (..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Foldable (toList)
import GHC.Generics (Generic)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Frontend.Pass.TranslateTypes.Error (FunPosition,
                                                     FunPositionError,
                                                     FunTypeLocation,
                                                     positionErrorsLevel,
                                                     prettyPositionIn)
import HsBindgen.Frontend.Pass.TranslateTypes.FFIType (ExtBindingFallback (..))
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  DelayedMsg
-------------------------------------------------------------------------------}

data DelayedTranslateTypesMsg =
    -- | An external binding passed directly has no FFI type in its binding
    -- specification
    TranslateTypesExtBindingFallback FunTypeLocation FunPosition ExtBindingFallback
    -- | A function type with serialised positions gets no @ToFunPtr@ and
    -- @FromFunPtr@ instances
    --
    -- Function pointers have no C wrapper.
  | TranslateTypesFunTypeSerialized FunTypeLocation (NonEmpty FunPosition)
    -- | A function type with positions we cannot pass gets no @ToFunPtr@ and
    -- @FromFunPtr@ instances
    --
    -- The positions in the errors are those of the function type.
  | TranslateTypesFunTypeUnclassified FunTypeLocation (NonEmpty FunPositionError)
  deriving stock (Show, Generic)

instance PrettyForTrace DelayedTranslateTypesMsg where
  prettyForTrace = \case
      TranslateTypesExtBindingFallback loc pos fallback -> PP.vcat [
          PP.hsep [
              "The binding specification of"
            , prettyExtRef fallback.extRef
            , "gives no FFI type;"
            , prettyPositionIn loc pos
            , "uses FFI type"
            , prettyExtRef (Hs.ffiExtRef fallback.assumed)
            ]
        , "This requires a matching `HasFFIType` instance."
        ]
      TranslateTypesFunTypeSerialized loc positions ->
        PP.hang
          (PP.hsep [
              noInstancesFor loc
            , "a function pointer cannot pass struct, union or complex values,"
            , "as needed by:"
            ])
          2
          (PP.vcat $ map prettyForTrace (toList positions))
      TranslateTypesFunTypeUnclassified loc errs ->
        PP.hang (noInstancesFor loc) 2 $
          PP.vcat $ map prettyForTrace (toList errs)
    where
      noInstancesFor :: FunTypeLocation -> PP.CtxDoc
      noInstancesFor loc = PP.hsep [
            "No `ToFunPtr` and `FromFunPtr` instances for"
          , prettyForTrace loc PP.>< ":"
          ]

      prettyExtRef :: Hs.ExtRef -> PP.CtxDoc
      prettyExtRef ref = PP.singleQuotes $ PP.hcat [
            prettyForTrace ref.moduleName
          , "."
          , PP.text ref.name.text
          ]

instance IsTrace Level DelayedTranslateTypesMsg where
  getDefaultLogLevel = \case
    TranslateTypesExtBindingFallback{}       -> Warning
    TranslateTypesFunTypeSerialized{}        -> Info
    TranslateTypesFunTypeUnclassified _ errs -> positionErrorsLevel errs
  getSource          = const HsBindgen
  getTraceId         = const "translate-types"
