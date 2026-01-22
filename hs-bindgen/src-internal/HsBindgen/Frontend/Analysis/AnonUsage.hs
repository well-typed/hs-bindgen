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

import HsBindgen.Errors
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass (Parse)
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (AnonId)
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId qualified as PrelimDeclId
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | How are anonymous data types used?
data AnonUsageAnalysis = AnonUsageAnalysis{
      map :: Map AnonId Context
    }
  deriving stock (Show)

data Context =
    -- | Anonymous declaration used inside a field
    --
    -- E.g.
    --
    -- > struct rect {
    -- >   struct { int x; int y; } topleft;
    -- >   struct { int x; int y; } bottomright;
    -- > }
    Field (C.DeclInfo Parse) (C.FieldInfo Parse)

    -- | Direct use of anonymous declaration inside in a typedef
    --
    -- E.g.
    --
    -- > typedef struct { int; int y; } point;
  | TypedefDirect (C.DeclInfo Parse)

    -- | Indirect use of an anonymous declaration inside a typedef
    --
    -- The most typical example of this is
    --
    -- > typedef struct { int; int y; } * point;
    --
    -- but there are many others, such as
    --
    -- > typedef struct { int; int y; } points[10];
    --
    -- We distinguish this from 'TypedefDirect' because in the case of
    -- 'TypedefDirect' we use the name of typedef as the name of the struct
    -- (indeed, @clang >= 16@ already does this out of the box), but in the case
    -- of 'TypedefIndirect' we add a @_Aux@ suffix, because now the two types
    -- are meaningfully different (and @clang@ assigns no name at all).
  | TypedefIndirect (C.DeclInfo Parse)
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

fromDecls :: [C.Decl Parse] -> AnonUsageAnalysis
fromDecls decls = AnonUsageAnalysis{
      map = Map.fromListWithKey resolveConflicts $
             concatMap analyseDecl decls
    }

-- | Resolve conflicts
--
-- Anonymous declarations can in rare circumstances have multiple use sites.
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/1430>
-- There is (at least) one other example we should take care of.
resolveConflicts :: AnonId -> Context -> Context -> Context
resolveConflicts anonId new old =
    case (old, new) of
      (Field decl1 _, Field decl2 _) | decl1.id == decl2.id ->
        -- Example:
        --
        -- > struct rect {
        -- >   struct { int x; int y; } tl, br;
        -- > };
        --
        -- We choose the first field (in source order).
        old
      (TypedefDirect _, TypedefDirect _) ->
        -- Example:
        --
        -- > typedef struct { int x; int y; } point1a, point1b;
        --
        -- We choose the first typedef (in source order).
        old
      (TypedefIndirect _, TypedefIndirect _) ->
        -- Example:
        --
        -- > typedef struct { int x; int y; } *point3a, *point3b;
        --
        -- We choose the first typedef (in source order).
        old
      _otherwise ->
         panicPure $ concat [
             "Conflicting use sites for "
           , show anonId
           , ": "
           , show (old, new)
           ]

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

-- | Analyse declaration
--
-- NOTE: Anonymous declarations that appear in function signatures and
-- global variables are unusable, and so we do not assign a name to them
-- (this will cause them to be removed from the list of declarations).
analyseDecl :: C.Decl Parse -> [(AnonId, Context)]
analyseDecl decl =
    case decl.kind of
      C.DeclStruct           x -> analyseStruct  decl.info x
      C.DeclUnion            x -> analyseUnion   decl.info x
      C.DeclTypedef          x -> analyseTypedef decl.info x
      C.DeclEnum             _ -> []
      C.DeclAnonEnumConstant _ -> []
      C.DeclOpaque             -> []
      C.DeclMacro            _ -> []
      C.DeclFunction         _ -> []
      C.DeclGlobal           _ -> []

analyseStruct :: C.DeclInfo Parse -> C.Struct Parse -> [(AnonId, Context)]
analyseStruct info struct = concat [
      concatMap aux struct.fields
    , concatMap aux struct.flam
    ]
  where
    aux :: C.StructField Parse -> [(AnonId, Context)]
    aux f = analyseType (Field info f.info) f.typ

analyseUnion :: C.DeclInfo Parse -> C.Union Parse -> [(AnonId, Context)]
analyseUnion info union =
    concatMap aux union.fields
  where
    aux :: C.UnionField Parse -> [(AnonId, Context)]
    aux f = analyseType (Field info f.info) f.typ

analyseTypedef :: C.DeclInfo Parse -> C.Typedef Parse -> [(AnonId, Context)]
analyseTypedef info typedef = analyseType (TypedefDirect info) typedef.typ

{-------------------------------------------------------------------------------
  Types

  This is where the real work happens; the rest is just setting up context.
-------------------------------------------------------------------------------}

analyseType :: Context -> C.Type Parse -> [(AnonId, Context)]
analyseType = go
  where
    go :: Context -> C.Type Parse -> [(AnonId, Context)]
    go ctxt = \case
        -- Base case
        C.TypeRef ref ->
            case ref of
              PrelimDeclId.Named{}     -> []
              PrelimDeclId.Anon anonId -> [(anonId, ctxt)]
        C.TypeEnum ref ->
            case ref.name of
              PrelimDeclId.Named{}     -> []
              PrelimDeclId.Anon anonId -> [(anonId, ctxt)]

        -- Recursion
        --
        -- For the @const@ case, something like
        --
        -- > typedef const struct { .. } foo;
        --
        -- perhaps, we follow @libclang@ and consider this an indirect usage
        -- (\"follow\" in the sense that @libclang@ does /not/ assign the name
        -- of the typedef to the struct in this case; we will add the suffix).
        C.TypePointers _n     ty -> indirect ty
        C.TypeQual _qual ty      -> indirect ty
        C.TypeConstArray _sz  ty -> indirect ty
        C.TypeIncompleteArray ty -> indirect ty
        C.TypeBlock           ty -> indirect ty
        C.TypeFun args res       -> concatMap indirect args ++ indirect res

        -- Trivial cases
        C.TypeComplex{}    -> []
        C.TypePrim{}       -> []
        C.TypeTypedef{}    -> []
        C.TypeVoid{}       -> []
      where
        indirect :: C.Type Parse -> [(AnonId, Context)]
        indirect =
           case ctxt of
             TypedefDirect declInfo -> go (TypedefIndirect declInfo)
             _otherwise             -> go ctxt
