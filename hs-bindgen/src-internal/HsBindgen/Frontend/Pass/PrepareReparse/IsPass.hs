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

import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel.Types (MultiLoc)

import HsBindgen.Frontend.AST.Coerce (CoercePass (coercePass), CoercePassAnn,
                                      CoercePassCommentDecl (..), CoercePassId,
                                      CoercePassMacroBody (..),
                                      CoercePassMacroId)
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Pass (IsPass (Ann, CommentDecl, Id, MacroBody, MacroId, Msg, macroIdId),
                                NoAnn, Pass)
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass (DeclMeta)
import HsBindgen.Frontend.Pass.Parse.IsPass (ReparseInfo)
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass (CheckedMacro (..),
                                                       TypecheckMacros)
import HsBindgen.Imports (Star, Symbol)
import HsBindgen.Util.Tracer (IsTrace (..), Level (Debug, Warning),
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
      MacroExpr expr -> MacroExpr $ coercePass expr

{-------------------------------------------------------------------------------
  Msg
-------------------------------------------------------------------------------}

data PrepareReparseMsg =
    PrepareReparseWriteTempHeader
      -- | Header path
      FilePath
      -- | Header contents
      String
  | PrepareReparsePreprocessorCommand
      -- | Pretty-printed command
      String
  | PrepareReparsePreprocessorExitCode
      ExitCode
  | PrepareReparsePreprocessorStdout
      ExitCode
      -- | @stdout@
      String
  | PrepareReparsePreprocessorStderr
      ExitCode
      -- | @stderr@
      String
  | PrepareReparseReadTempHeaderCutContents
      -- | Header contents with only the targets cut out
      String
  deriving stock Show

instance PrettyForTrace PrepareReparseMsg where
  prettyForTrace = \case
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
      PrepareReparsePreprocessorStdout _ stdout -> PP.vcat [
          PP.string "Clang preprocessor stdout:"
        , PP.string $ if null stdout then "empty" else stdout
        ]
      PrepareReparsePreprocessorStderr _ stderr -> PP.vcat [
          PP.string "Clang preprocessor stderr:"
        , PP.string $ if null stderr then "empty" else show stderr
        ]
      PrepareReparseReadTempHeaderCutContents cutContents -> PP.vcat [
          "Header contents with only the targets cut out:"
        , PP.string cutContents
        ]

instance IsTrace Level PrepareReparseMsg where
  getDefaultLogLevel = \case
    PrepareReparseWriteTempHeader{} -> Debug
    PrepareReparsePreprocessorCommand{} -> Debug
    PrepareReparsePreprocessorExitCode{} -> Debug
    PrepareReparsePreprocessorStdout ec _ -> case ec of
        ExitSuccess   -> Debug
        ExitFailure _ -> Warning
    PrepareReparsePreprocessorStderr ec _ -> case ec of
        ExitSuccess   -> Debug
        ExitFailure _ -> Warning
    PrepareReparseReadTempHeaderCutContents{} -> Debug
  getSource          = const HsBindgen
  getTraceId         = const "prepare-reparse"
