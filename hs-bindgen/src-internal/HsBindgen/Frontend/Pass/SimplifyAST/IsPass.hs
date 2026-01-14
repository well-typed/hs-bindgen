module HsBindgen.Frontend.Pass.SimplifyAST.IsPass (
    SimplifyAST
    -- * Trace messages
  , SimplifyASTMsg(..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Frontend.AST.Coerce (CoercePassId, CoercePassMacroBody,
                                      CoercePassMacroId)
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
  AnnSimplifyAST "StructField" = ReparseInfo
  AnnSimplifyAST "UnionField"  = ReparseInfo
  AnnSimplifyAST "Typedef"     = ReparseInfo
  AnnSimplifyAST "Function"    = ReparseInfo
  AnnSimplifyAST _             = NoAnn

instance IsPass SimplifyAST where
  type Id         SimplifyAST = PrelimDeclId
  type MacroBody  SimplifyAST = UnparsedMacro
  type ExtBinding SimplifyAST = Void
  type Ann ix     SimplifyAST = AnnSimplifyAST ix
  type Msg        SimplifyAST = SimplifyASTMsg

  idNameKind     _ = PrelimDeclId.nameKind
  idSourceName   _ = PrelimDeclId.sourceName
  idLocationInfo _ = prelimDeclIdLocationInfo

instance CoercePassId Parse SimplifyAST where
instance CoercePassMacroBody Parse SimplifyAST where
instance CoercePassMacroId Parse SimplifyAST where

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
