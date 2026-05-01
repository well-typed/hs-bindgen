module HsBindgen.Frontend.Pass.SimplifyAST.IsPass (
    SimplifyAST
    -- * Trace messages
  , SimplifyASTMsg(..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.LocationInfo (prelimDeclIdLocationInfo)
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (AnonId, PrelimDeclId)
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId qualified as PrelimDeclId
import HsBindgen.Imports
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

instance IsPass SimplifyAST where
  type Id          SimplifyAST = PrelimDeclId
  type MacroBody   SimplifyAST = ParsedMacro
  type ExtBinding  SimplifyAST = Void
  type Ann ix      SimplifyAST = AnnSimplifyAST ix
  type Msg         SimplifyAST = WithCallStack SimplifyASTMsg
  type CommentDecl SimplifyAST = ()

  idNameKind     _ = PrelimDeclId.nameKind
  idSourceName   _ = PrelimDeclId.sourceName
  idLocationInfo _ = prelimDeclIdLocationInfo

instance CoercePassId               Parse SimplifyAST
instance CoercePassMacroBody        Parse SimplifyAST
instance CoercePassMacroId          Parse SimplifyAST

instance CoercePassAnn "TypeFunArg" Parse SimplifyAST
instance CoercePassAnn "Global"     Parse SimplifyAST
instance CoercePassCommentDecl      Parse SimplifyAST

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data SimplifyASTMsg =
    SimplifyASTAnonymousEnum AnonId
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
