{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Internal.Trace
  ( degradeKnownTraces
  ) where

import Clang.HighLevel.Types (Diagnostic (diagnosticCategoryText))
import HsBindgen.Lib

-- | Degrade log levels of known traces in tests.
degradeKnownTraces :: CustomLogLevel Trace
degradeKnownTraces = CustomLogLevel $ \case
  -- "Declaration does not declare anything".
  TraceDiagnostic x | diagnosticCategoryText x == "Semantic Issue" -> Debug
  -- "Pointer is missing a nullability type specifier (_Nonnull, _Nullable, or _Null_unspecified)".
  TraceDiagnostic x | diagnosticCategoryText x == "Nullability Issue" -> Debug
  TraceExtraClangArgs _ -> Debug
  TraceSkipped _        -> Debug
  x                     -> getDefaultLogLevel x
