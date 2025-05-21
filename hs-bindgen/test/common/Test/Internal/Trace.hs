{-# LANGUAGE LambdaCase #-}

module Test.Internal.Trace
  ( degradeKnownTraces
  ) where

import Clang.HighLevel.Types (Diagnostic (diagnosticCategory))
import HsBindgen.Lib

-- | Degrade log levels of known traces in tests.
degradeKnownTraces :: CustomLogLevel Trace
degradeKnownTraces = CustomLogLevel $ \case
  -- "Sematic Issue": "declaration does not declare anything".
  TraceDiagnostic x | diagnosticCategory x == 2 -> Debug
  -- "Nullability Issue": "pointer is missing a nullability type specifier (_Nonnull, _Nullable, or _Null_unspecified)".
  TraceDiagnostic x | diagnosticCategory x == 26 -> Debug
  TraceExtraClangArgs _ -> Debug
  TraceSkipped _        -> Debug
  x                     -> getDefaultLogLevel x
