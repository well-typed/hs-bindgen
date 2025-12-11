-- | Analyse usage of anonymous declarations
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.AnonUsage (AnonUsageAnalysis)
-- > import HsBindgen.Frontend.Analysis.AnonUsage qualified as AnonUsageAnalysis
module HsBindgen.Frontend.Analysis.AnonUsage (
    AnonUsageAnalysis(..)
  , Context(..)
  , fromDecls
  ) where

import Data.Map qualified as Map

import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | How are anonymous data types used?
data AnonUsageAnalysis = AnonUsageAnalysis{
      anonUsage :: Map C.AnonId Context
    }

data Context =
    Field (C.DeclInfo Parse) (C.FieldInfo Parse)
  | TypedefVal (C.DeclInfo Parse)
  | TypedefRef (C.DeclInfo Parse)

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

fromDecls :: [C.Decl Parse] -> AnonUsageAnalysis
fromDecls = AnonUsageAnalysis . Map.fromList . concatMap analyseDecl

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

-- | Analyse declaration
--
-- NOTE: Anonymous declarations that appear in function signatures and
-- global variables are unusable, and so we do not assign a name to them
-- (this will cause them to be removed from the list of declarations).
analyseDecl :: C.Decl Parse -> [(C.AnonId, Context)]
analyseDecl decl =
    case decl.declKind of
      C.DeclStruct   x -> analyseStruct  decl.declInfo x
      C.DeclUnion    x -> analyseUnion   decl.declInfo x
      C.DeclTypedef  x -> analyseTypedef decl.declInfo x
      C.DeclEnum     _ -> []
      C.DeclOpaque   _ -> []
      C.DeclMacro    _ -> []
      C.DeclFunction _ -> []
      C.DeclGlobal   _ -> []

analyseStruct :: C.DeclInfo Parse -> C.Struct Parse -> [(C.AnonId, Context)]
analyseStruct declInfo struct =
    concatMap aux struct.structFields
  where
    aux :: C.StructField Parse -> [(C.AnonId, Context)]
    aux f = analyseType (Field declInfo f.structFieldInfo) f.structFieldType

analyseUnion :: C.DeclInfo Parse -> C.Union Parse -> [(C.AnonId, Context)]
analyseUnion declInfo union =
    concatMap aux union.unionFields
  where
    aux :: C.UnionField Parse -> [(C.AnonId, Context)]
    aux f = analyseType (Field declInfo f.unionFieldInfo) f.unionFieldType

analyseTypedef :: C.DeclInfo Parse -> C.Typedef Parse -> [(C.AnonId, Context)]
analyseTypedef declInfo typedef =
    analyseType (TypedefVal declInfo) typedef.typedefType

{-------------------------------------------------------------------------------
  Types

  This is where the real work happens; the rest is just setting up context.
-------------------------------------------------------------------------------}

analyseType :: Context -> C.Type Parse -> [(C.AnonId, Context)]
analyseType = go
  where
    go :: Context -> C.Type Parse -> [(C.AnonId, Context)]
    go ctxt = \case
        -- Interesting cases
        C.TypeRef     ref -> typeRef ctxt ref
        C.TypePointer ty  -> go (byRef ctxt) ty
        C.TypeConst   ty  -> go ctxt ty

        -- Trivial cases
        C.TypeComplex{}    -> []
        C.TypeExtBinding{} -> []
        C.TypePrim{}       -> []
        C.TypeTypedef{}    -> []
        C.TypeVoid{}       -> []

        -- Anonymous declarations that appear in these types unusable
        -- See also comment in 'analyseDecl'
        C.TypeConstArray{}      -> []
        C.TypeIncompleteArray{} -> []
        C.TypeFun{}             -> []
        C.TypeBlock{}           -> []

    typeRef :: Context -> C.PrelimDeclId -> [(C.AnonId, Context)]
    typeRef ctxt = \case
        C.PrelimDeclIdNamed{}           -> []
        C.PrelimDeclIdAnon anonId _kind -> [(anonId, ctxt)]

    byRef :: Context -> Context
    byRef ctxt =
       case ctxt of
         TypedefVal declInfo -> TypedefRef declInfo
         _otherwise          -> ctxt
