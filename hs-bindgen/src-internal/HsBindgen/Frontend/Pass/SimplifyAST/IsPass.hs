module HsBindgen.Frontend.Pass.SimplifyAST.IsPass (
    SimplifyAST
    -- * Trace messages
  , SimplifyASTMsg(..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Macro.Type
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type SimplifyAST :: Pass
data SimplifyAST a

-- Preserve annotations from @Parse@ pass
type family AnnSimplifyAST (ix :: Symbol) where
  AnnSimplifyAST "Struct"      = IsAnon
  AnnSimplifyAST "StructField" = (ReparseInfo Tokens, FieldOrigin)
  AnnSimplifyAST "Union"       = IsAnon
  AnnSimplifyAST "UnionField"  = (ReparseInfo Tokens, FieldOrigin)
  AnnSimplifyAST "Typedef"     = ReparseInfo Tokens
  AnnSimplifyAST "Function"    = ReparseInfo Tokens
  AnnSimplifyAST "Global"      = ReparseInfo Tokens
  AnnSimplifyAST _             = NoAnn

instance IsPass SimplifyAST

instance PassId SimplifyAST where
  type Id SimplifyAST = C.PrelimDeclId

  idNameKind     _ = C.prelimDeclIdNameKind
  idSourceName   _ = C.prelimDeclIdSourceName
  idLocationInfo _ = C.prelimDeclIdLocationInfo

instance PassScopedName SimplifyAST

instance PassMacro SimplifyAST where
  type MacroBody SimplifyAST = ParsedMacroBody

instance PassExtBinding SimplifyAST

instance PassCommentDecl SimplifyAST

instance PassAnn SimplifyAST where
  type Ann ix SimplifyAST = AnnSimplifyAST ix

instance PassMsg SimplifyAST where
  type Msg SimplifyAST = SimplifyASTMsg

instance CoercePassId                Parse SimplifyAST
instance CoercePassMacroBody         Parse SimplifyAST
instance CoercePassMacroId           Parse SimplifyAST
instance CoercePassMacroUnderlying   Parse SimplifyAST

instance CoercePassAnn "TypeFunArg" Parse SimplifyAST
instance CoercePassAnn "Global"     Parse SimplifyAST
instance CoercePassCommentDecl      Parse SimplifyAST

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data SimplifyASTMsg =
    SimplifyASTAnonymousEnum C.AnonId
  deriving stock (Show, Generic)

instance PrettyForTrace SimplifyASTMsg where
  prettyForTrace = \case
      SimplifyASTAnonymousEnum anonId -> PP.hsep [
          "Transformed anonymous enum: "
        , prettyForTrace anonId
        , "into constant patterns."
        ]

instance IsTrace Level SimplifyASTMsg where
  getDefaultLogLevel = \case
      SimplifyASTAnonymousEnum{} -> Info

  getSource  = const HsBindgen
  getTraceId = const "simplify-ast"
