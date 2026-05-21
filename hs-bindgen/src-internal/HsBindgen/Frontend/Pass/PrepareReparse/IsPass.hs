-- | Definitions for the @PrepareReparse@ pass
--
-- This module is intended to be imported unqualified.
--
-- > import HsBindgen.Frontend.Pass.PrepareReparse.IsPass
--
module HsBindgen.Frontend.Pass.PrepareReparse.IsPass (
    PrepareReparse
    -- * Tokens
  , FlatTokens (..)
    -- * Msg
  , PrepareReparseMsg (..)
  ) where

import GHC.IO.Exception (ExitCode)
import Text.Parsec.Error qualified as Parsec
import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel.Types (MultiLoc)

import HsBindgen.Frontend.AST.Coerce (CoercePass (coercePass), CoercePassAnn,
                                      CoercePassCommentDecl (..), CoercePassId,
                                      CoercePassMacroBody (..),
                                      CoercePassMacroId)
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.DeclMeta (DeclMeta)
import HsBindgen.Frontend.Pass (IsPass (Ann, CommentDecl, Id, MacroBody, MacroId, Msg, macroIdId),
                                NoAnn, Pass)
import HsBindgen.Frontend.Pass.Parse.IsPass (ReparseInfo)
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass (CheckedMacro (..),
                                                       TypecheckMacros)
import HsBindgen.Imports (Star, Symbol)
import HsBindgen.Util.Tracer (IsTrace (..), Level (Bug, Debug, Warning),
                              PrettyForTrace (..), Source (HsBindgen),
                              WithCallStack)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type PrepareReparse :: Pass
data PrepareReparse a

type family AnnPrepareReparse (ix :: Symbol) :: Star where
  AnnPrepareReparse "TranslationUnit" = DeclMeta
  AnnPrepareReparse "StructField"     = ReparseInfo FlatTokens
  AnnPrepareReparse "UnionField"      = ReparseInfo FlatTokens
  AnnPrepareReparse "Typedef"         = ReparseInfo FlatTokens
  AnnPrepareReparse "Function"        = ReparseInfo FlatTokens
  AnnPrepareReparse "Global"          = ReparseInfo FlatTokens
  AnnPrepareReparse _                 = NoAnn

instance IsPass PrepareReparse where
  type MacroBody   PrepareReparse = CheckedMacro PrepareReparse
  type Ann ix      PrepareReparse = AnnPrepareReparse ix
  type Msg         PrepareReparse = WithCallStack PrepareReparseMsg
  type MacroId     PrepareReparse = Id PrepareReparse
  type CommentDecl PrepareReparse = Maybe (C.Comment PrepareReparse)
  macroIdId _ = id

{-------------------------------------------------------------------------------
  Tokens
-------------------------------------------------------------------------------}

-- | @libclang@ tokens flattened into a single string
data FlatTokens = FlatTokens {
      -- | Tokens flattened into a single string
      flatten  :: String
      -- | Location of the first token before flattening
    , locStart :: MultiLoc
    }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  CoercePass: TypecheckMacros → PrepareReparse
-------------------------------------------------------------------------------}

instance CoercePassId               TypecheckMacros PrepareReparse
instance CoercePassMacroId          TypecheckMacros PrepareReparse
instance CoercePassAnn "TypeFunArg" TypecheckMacros PrepareReparse

instance CoercePassCommentDecl      TypecheckMacros PrepareReparse where
  coercePassCommentDecl _ = fmap coercePass

instance CoercePassMacroBody        TypecheckMacros PrepareReparse where
  coercePassMacroBody _ = \case
      MacroType ty -> MacroType $ coercePass ty
      MacroValue val -> MacroValue $ coercePass val

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
