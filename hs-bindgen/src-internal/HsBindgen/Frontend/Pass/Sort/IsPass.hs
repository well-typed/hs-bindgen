module HsBindgen.Frontend.Pass.Sort.IsPass (
    Sort
  , DeclMeta(..)
  , coerceDeclMeta
  , SortMsg(..)
  ) where

import HsBindgen.Frontend.Analysis.DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph
import HsBindgen.Frontend.AST.Internal (ValidPass)
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.NonParsedDecls
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition

  The only thing that changes in this pass is the annotation on the top-level
  'TranslationUnit'; for everything else we simply refer to the 'Parse' pass.
-------------------------------------------------------------------------------}

type Sort :: Pass
data Sort a deriving anyclass ValidPass

type family AnnSort (ix :: Symbol) :: Star where
  AnnSort "TranslationUnit" = DeclMeta Sort
  AnnSort "StructField"     = ReparseInfo
  AnnSort "UnionField"      = ReparseInfo
  AnnSort "Typedef"         = ReparseInfo
  AnnSort "Function"        = ReparseInfo
  AnnSort _                 = NoAnn

instance IsPass Sort where
  type Id           Sort = C.PrelimDeclId
  type FieldName    Sort = C.Name
  type ArgumentName Sort = Maybe C.Name
  type TypedefRef   Sort = C.Name
  type MacroBody    Sort = UnparsedMacro
  type ExtBinding   Sort = Void
  type Ann ix       Sort = AnnSort ix
  type Msg          Sort = SortMsg

{-------------------------------------------------------------------------------
  Information about the declarations
-------------------------------------------------------------------------------}

data DeclMeta p = DeclMeta {
      declIndex     :: DeclIndex
    , declUseDecl   :: UseDeclGraph
    , declDeclUse   :: DeclUseGraph
    , declNonParsed :: NonParsedDecls
    , declParseMsgs :: ParseMsgs p
    }

deriving instance ValidPass p => Show (DeclMeta p)

coerceDeclMeta :: forall p p'. (Id p ~ Id p', Ord (ParseMsgKey p'))
  => DeclMeta p -> DeclMeta p'
coerceDeclMeta declMeta = declMeta {
    declParseMsgs = coerceParseMsgs (declParseMsgs declMeta)
    }

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data SortMsg =
    SortErrorDeclIndex DeclIndexError
  deriving stock    (Show, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)
