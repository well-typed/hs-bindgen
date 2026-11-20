-- | Trace messages for the @FunPtrInstances@ pass
--
-- This module is intended to be imported unqualified.
module HsBindgen.Frontend.Pass.FunPtrInstances.IsPass.Msg (
    FunPtrInstancesMsg (..)
  ) where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Frontend.Pass.TranslateTypes.Error (FunPositionError,
                                                     positionErrorsLevel)
import HsBindgen.Frontend.Pass.TranslateTypes.FFIType (ExtBindingFallback)
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass.Msg (prettyExtBindingFallback)
import HsBindgen.Frontend.Pass.TranslateTypes.Position (FunPosition,
                                                        FunTypeLocation,
                                                        prettyPositionIn)
import HsBindgen.Util.Tracer

-- | Messages about a function type
--
-- Each distinct function type is reported once, at its first occurrence that
-- would otherwise get instances.
data FunPtrInstancesMsg =
    -- | An external binding passed directly has no FFI type in its binding
    -- specification
    FunPtrInstancesExtBindingFallback FunTypeLocation FunPosition ExtBindingFallback
    -- | The function type passes a serialised value, so it does not get
    -- @ToFunPtr@ and @FromFunPtr@ instances
  | FunPtrInstancesSerialized FunTypeLocation (NonEmpty FunPosition)
    -- | The function type has an argument or result we cannot pass, so it
    -- does not get @ToFunPtr@ and @FromFunPtr@ instances
    --
    -- The positions in the errors are relative to this function type.
  | FunPtrInstancesPositionErrors FunTypeLocation (NonEmpty FunPositionError)
  deriving stock (Show, Generic)

instance PrettyForTrace FunPtrInstancesMsg where
  prettyForTrace = \case
      FunPtrInstancesExtBindingFallback loc pos fallback ->
        prettyExtBindingFallback (prettyPositionIn loc pos) fallback
      FunPtrInstancesSerialized loc positions ->
        PP.hang
          (PP.hsep [
              noInstancesFor loc
            , "function pointers cannot pass structs, unions or complex"
            , "values, as needed by:"
            ])
          2
          (PP.vcat $ map prettyForTrace (toList positions))
      FunPtrInstancesPositionErrors loc errs ->
        PP.hang (noInstancesFor loc) 2 $
          PP.vcat $ map prettyForTrace (toList errs)
    where
      noInstancesFor :: FunTypeLocation -> PP.CtxDoc
      noInstancesFor loc = PP.hsep [
            "No `ToFunPtr` and `FromFunPtr` instances for"
          , prettyForTrace loc PP.>< ":"
          ]

instance IsTrace Level FunPtrInstancesMsg where
  getDefaultLogLevel = \case
    FunPtrInstancesExtBindingFallback{}    -> Warning
    FunPtrInstancesSerialized{}            -> Info
    FunPtrInstancesPositionErrors _ errs   -> positionErrorsLevel errs
  getSource          = const HsBindgen
  getTraceId         = const "fun-ptr-instances"
