-- | Trace messages for the @PrepareReparse@ pass
--
-- This module is intended to be imported unqualified.
module HsBindgen.Frontend.Pass.PrepareReparse.IsPass.Msg (
    -- * Msg
    PrepareReparseMsg (..)
    -- * DelayedMsg
  , DelayedPrepareReparseMsg (..)
  ) where

import GHC.Generics (Generic)
import System.Exit (ExitCode)
import Text.Parsec.Error qualified as Parsec
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Util.Tracer (IsTrace (..), Level (Bug, Debug, Info, Warning),
                              PrettyForTrace (..), Source (HsBindgen))

{-------------------------------------------------------------------------------
  Msg
-------------------------------------------------------------------------------}

data PrepareReparseMsg =
    -- * Warning

    -- | We need the @clang@ executable to preprocess select macro invocations,
    -- but we could not find it. Is it missing from the PATH environment
    -- variable?
    --
    -- The generated Haskell bindings might not fully support macro types as a
    -- result.
    PrepareReparseClangExeNotFound

    -- * Info

    -- | There are macro invocations that are not inwards floatable, so we can
    -- not expand them.
  | PrepareReparseInvocationsNotFloatable


    -- * Debug

    -- | We wrote a temporary header to a temporary file path
  | PrepareReparseWriteTempHeader
      -- | Header path
      FilePath
      -- | Header contents
      String
    -- | Printed preprocessor command
  | PrepareReparsePreprocessorCommand String
    -- | Exit code resulting from running the @clang@ executable for preprocessing
  | PrepareReparsePreprocessorExitCode ExitCode
    -- | @stdout@ output resulting from running the @clang@ executable for
    -- preprocessing
  | PrepareReparsePreprocessorStdout String
    -- | @stderr@ output resulting from running the @clang@ executable for
    -- preprocessing
  | PrepareReparsePreprocessorStderr String
    -- | Header contents with only the reparse targets cut out
  | PrepareReparseTempHeaderCutContents String

    -- * Bug

    -- | We tried to run the @clang@ executable to preprocess select macro
    -- invocations, but it failed to run.
    --
    -- The generated Haskell bindings might not fully support macro types as a
    -- result.
  | PrepareReparsePreprocessorFailed
    -- | We tried to single out the interesting lines of code from the
    -- preprocessor output, but we failed.
    --
    -- This can happen only if we fail to find a specific line directive that
    -- would point out where the interesting lines of code start. This is likely
    -- a bug in @hs-bindgen@.
  | PrepareReparseInterpretPreprocessorOutputFailed
    -- | We failed to parse the preprocessor output
    --
    -- This is likely a bug in @hs-bindgen@.
  | PrepareReparseParsePreprocessorOutputFailed Parsec.ParseError
  deriving stock Show

instance PrettyForTrace PrepareReparseMsg where
  prettyForTrace = \case
      -- * Warning

      PrepareReparseClangExeNotFound -> PP.vsep [
          PP.hsep [
              "We could not find @clang@ executable, but we need it to preprocess"
            , "select macro invocations. Is it missing from the PATH environment"
            , "variable?"
            ]
        , PP.hsep [
              "The generated Haskell bindings might not fully support macro types as a"
            , "result."
            ]
        ]

      -- * Info

      PrepareReparseInvocationsNotFloatable -> PP.hsep [
          "There are macro invocations that are not inwards floatable, so we can"
        , "not expand them."
        ]

      -- * Debug

      PrepareReparseWriteTempHeader path contents -> PP.hsep [
          "Creating a temporary header file at"
        , PP.string path
        , "with contents:"
        ] PP.$$ PP.string contents
      PrepareReparsePreprocessorCommand cmd -> PP.hsep [
          "Running the clang preprocessor with invocation:"
        , PP.string cmd
        ]
      PrepareReparsePreprocessorExitCode ec -> PP.hsep [
          "The clang preprocessor exited with exit code:"
        , PP.string (show ec)
        ]
      PrepareReparsePreprocessorStdout stdout -> PP.vcat [
          PP.string "Clang preprocessor stdout:"
        , PP.string $ if null stdout then "empty" else stdout
        ]
      PrepareReparsePreprocessorStderr stderr -> PP.vcat [
          PP.string "Clang preprocessor stderr:"
        , PP.string $ if null stderr then "empty" else show stderr
        ]
      PrepareReparseTempHeaderCutContents cutContents -> PP.vcat [
          "Header contents with only the targets cut out:"
        , PP.string cutContents
        ]

      -- * Bug

      PrepareReparsePreprocessorFailed -> PP.hsep [
          "We tried to run the clang executable to preprocess select macro"
        , "invocations, but it failed to run."
        ]
      PrepareReparseInterpretPreprocessorOutputFailed -> PP.hsep [
          "We failed to single out the interesting lines of code from the"
        , "preprocessor output."
        ]
      PrepareReparseParsePreprocessorOutputFailed e -> PP.hsep [
          "We failed to parse the preprocessor output:"
        , PP.string (show e)
        ]

instance IsTrace Level PrepareReparseMsg where
  getDefaultLogLevel = \case
      -- * Warning
      PrepareReparseClangExeNotFound{} -> Warning
      -- * Info
      PrepareReparseInvocationsNotFloatable{} -> Info
      -- * Debug
      PrepareReparseWriteTempHeader{} -> Debug
      PrepareReparsePreprocessorCommand{} -> Debug
      PrepareReparsePreprocessorExitCode{} -> Debug
      PrepareReparsePreprocessorStdout{} -> Debug
      PrepareReparsePreprocessorStderr{} -> Debug
      PrepareReparseTempHeaderCutContents{} -> Debug
      -- * Bug
      PrepareReparsePreprocessorFailed{} -> Bug
      PrepareReparseInterpretPreprocessorOutputFailed{} -> Bug
      PrepareReparseParsePreprocessorOutputFailed{} -> Bug
  getSource          = const HsBindgen
  getTraceId         = const "prepare-reparse"

{-------------------------------------------------------------------------------
  DelayedMsg
-------------------------------------------------------------------------------}

data DelayedPrepareReparseMsg =
    -- | Failed to prepare this declaration for reparsing
    PrepareReparseFailed
  deriving stock (Show, Generic)

instance PrettyForTrace DelayedPrepareReparseMsg where
  prettyForTrace = \case
    PrepareReparseFailed -> PP.hsep [
        "Failed to prepare this declaration for reparsing"
      ]

instance IsTrace Level DelayedPrepareReparseMsg where
  getDefaultLogLevel = \case
      PrepareReparseFailed{} -> Bug
  getSource          = const HsBindgen
  getTraceId         = const "prepare-reparse"
