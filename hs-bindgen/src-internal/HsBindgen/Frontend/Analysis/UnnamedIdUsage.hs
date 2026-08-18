-- | Analyse usage of unnamed declarations
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.UnnamedIdUsage (UnnamedIdUsageAnalysis)
-- > import HsBindgen.Frontend.Analysis.UnnamedIdUsage qualified as UnnamedIdUsageAnalysis
module HsBindgen.Frontend.Analysis.UnnamedIdUsage (
    UnnamedIdUsageAnalysis(..)
  , Context(..)
  , fromDecls
  ) where

import Data.Map qualified as Map

import HsBindgen.Errors
import HsBindgen.Frontend.Pass.Parse.IsPass (Parse)
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | How are unnamed data types used?
data UnnamedIdUsageAnalysis = UnnamedIdUsageAnalysis{
      map :: Map C.UnnamedId Context
    }
  deriving stock (Show)

data Context =
    -- | Unnamed declaration inside a direct, named field
    --
    -- E.g.
    --
    -- > struct rect {
    -- >   struct { int x; int y; } topleft;
    -- >   struct { int x; int y; } bottomright;
    -- > }
    --
    -- @topleft@ and @bottomright@ are fields that reference untagged structs.
    -- Both fields are direct fields of struct @rect@.
    --
    -- NOTE: after the @Parse@ pass, unnamed fields can not exist. In case we
    -- parse unnamed fields (i.e., implicit fields) in the @Parse@ pass, then we
    -- generate a name for those fields. See the
    -- "HsBindgen.Frontend.Pass.Parse.Decl.ImplicitFields" module for more
    -- information.
    --
    -- Indirect fields are not analysed, because indirect fields only exist for
    -- regular fields and those will have been analysed already.
    --
    -- E.g.
    --
    -- > struct S {
    -- >   struct { // <-- anonymous struct
    -- >     struct { // <-- untagged struct
    -- >       int y;
    -- >     } x;
    -- >   };
    -- > };
    --
    -- @x@ is a field that references an untagged struct. @x@ is a direct field
    -- of the anonymous struct, and an indirect field of struct @rect@.
    Field (C.DeclInfo Parse) (C.FieldInfo Parse)

    -- | Direct use of unnamed declaration inside in a typedef
    --
    -- E.g.
    --
    -- > typedef struct { int; int y; } point;
  | TypedefDirect (C.DeclInfo Parse)

    -- | Indirect use of an unnamed declaration inside a typedef
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

    -- | Unnamed declaration used as the type of a global variable
    --
    -- E.g.
    --
    -- > struct { int x; int y; } a;
    --
    -- In this case, we use the name of the global variable as the name of
    -- the struct.
  | GlobalVar (C.DeclInfo Parse)
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

fromDecls :: [C.Decl l Parse] -> UnnamedIdUsageAnalysis
fromDecls decls = UnnamedIdUsageAnalysis{
      map = Map.fromListWithKey resolveConflicts $
             concatMap analyseDecl decls
    }

-- | Resolve conflicts
--
-- Unnamed declarations can in rare circumstances have multiple use sites.
resolveConflicts :: C.UnnamedId -> Context -> Context -> Context
resolveConflicts unnamedId new old =
    case (old, new) of
      {-----------------------------------------------------------------------------
        Fields
      -----------------------------------------------------------------------------}

      (Field decl1 _, Field decl2 _) | decl1.id == decl2.id ->
        -- Example:
        --
        -- > struct rect {
        -- >   struct { int x; int y; } tl, br;
        -- > };
        --
        -- Multiple declarators for the same field.
        -- We choose the first field (in source order).
        old

      {-----------------------------------------------------------------------------
        Clang < 16
      -----------------------------------------------------------------------------}

      (TypedefDirect _, TypedefIndirect _) ->
        -- Example:
        --
        -- > typedef struct { int x; int y; } point2a, *point2b;
        --
        -- Mixed direct and indirect typedefs (direct seen first).
        old

      (TypedefIndirect _, TypedefDirect _) ->
        -- Mirror of the above case (when indirect appears first in source order).
        new

      (TypedefDirect _, TypedefDirect _) ->
        -- Example:
        --
        -- > typedef struct { int x; int y; } point1a, point1b;
        --
        -- Multiple direct typedefs for the same untagged struct.
        -- We choose the first typedef (in source order).
        old

      {-----------------------------------------------------------------------------
        Clang >= 16
      -----------------------------------------------------------------------------}

      (TypedefIndirect _, TypedefIndirect _) ->
        -- Example:
        --
        -- > typedef struct { int x; int y; } *point3a, *point3b;
        --
        -- Multiple indirect typedefs for the same untagged struct.
        -- We choose the first typedef (in source order).
        --
        -- NOTE: This is the ONLY conflict case that can occur in Clang >= 16.
        -- In Clang >= 16, when a typedef has both direct and indirect declarators
        -- (e.g., `typedef struct { ... } a, *b;`), Clang names the untagged struct
        -- with the first direct typedef name (@a@), and the indirect typedef ('b')
        -- becomes a pointer to that named type. Thus no conflict exists.
        old

      _otherwise ->
         panicPure $ concat [
             "Conflicting use sites for "
           , show unnamedId
           , ": "
           , show (old, new)
           ]

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

-- | Analyse declaration
--
-- NOTE: Unnamed declarations that appear in function signatures and
-- global variables are unusable, and so we do not assign a name to them
-- (this will cause them to be removed from the list of declarations).
analyseDecl :: C.Decl l Parse -> [(C.UnnamedId, Context)]
analyseDecl decl =
    case decl.kind of
      C.DeclStruct               x -> analyseStruct  decl.info x
      C.DeclUnion                x -> analyseUnion   decl.info x
      C.DeclTypedef              x -> analyseTypedef decl.info x
      C.DeclEnum                 _ -> []
      C.DeclUntaggedEnumConstant _ -> []
      C.DeclOpaque{}               -> []
      C.DeclMacro                _ -> []
      C.DeclFunction             _ -> []
      C.DeclGlobal               x -> analyseGlobal  decl.info x.typ

analyseStruct :: C.DeclInfo Parse -> C.Struct Parse -> [(C.UnnamedId, Context)]
analyseStruct info struct = concat [
      concatMap (analyseField info)        struct.fields
    , foldMap   (analyseRegularField info) (C.flamStructField struct.flam)
    ]

analyseUnion :: C.DeclInfo Parse -> C.Union Parse -> [(C.UnnamedId, Context)]
analyseUnion info union =
    concatMap (analyseField info) union.fields

analyseField :: C.DeclInfo Parse -> C.Field Parse -> [(C.UnnamedId, Context)]
analyseField info = C.elimField (analyseRegularField info) (analyseImplicitField info)

analyseRegularField :: C.DeclInfo Parse -> C.RegularField Parse -> [(C.UnnamedId, Context)]
analyseRegularField info field = analyseType (Field info field.info) field.typ

analyseImplicitField :: C.DeclInfo Parse -> C.ImplicitField Parse -> [(C.UnnamedId, Context)]
analyseImplicitField info field = concat [
      analyseType (Field info field.info) field.typ
    , concatMap (analyseIndirectField info) field.indirect
    ]

analyseIndirectField :: C.DeclInfo Parse -> C.IndirectField Parse -> [(C.UnnamedId, Context)]
analyseIndirectField _info _field =
    -- Indirect fields can only exist if there is a direct field, and we always
    -- prefer the direct field. See also the haddocks on 'Field'.
    []

analyseTypedef :: C.DeclInfo Parse -> C.Typedef Parse -> [(C.UnnamedId, Context)]
analyseTypedef info typedef = analyseType (TypedefDirect info) typedef.typ

analyseGlobal :: C.DeclInfo Parse -> C.Type Parse -> [(C.UnnamedId, Context)]
analyseGlobal info = analyseType (GlobalVar info)

{-------------------------------------------------------------------------------
  Types

  This is where the real work happens; the rest is just setting up context.
-------------------------------------------------------------------------------}

analyseType :: Context -> C.Type Parse -> [(C.UnnamedId, Context)]
analyseType = go
  where
    go :: Context -> C.Type Parse -> [(C.UnnamedId, Context)]
    go ctxt = \case
        -- Base case
        C.TypeRef ref ->
            case ref of
              C.PrelimDeclIdNamed{}     -> []
              C.PrelimDeclIdUnnamed unnamedId -> [(unnamedId, ctxt)]
        C.TypeEnum ref ->
            case ref.name of
              C.PrelimDeclIdNamed{}     -> []
              C.PrelimDeclIdUnnamed unnamedId -> [(unnamedId, ctxt)]

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
        C.TypeFun args res       -> concatMap analyseTypeFunArg args ++ indirect res

        -- Trivial cases
        C.TypeComplex{}    -> []
        C.TypePrim{}       -> []
        C.TypeTypedef{}    -> []
        C.TypeVoid{}       -> []
      where
        indirect :: C.Type Parse -> [(C.UnnamedId, Context)]
        indirect =
           case ctxt of
             TypedefDirect declInfo -> go (TypedefIndirect declInfo)
             _otherwise             -> go ctxt

        analyseTypeFunArg :: C.TypeFunArg Parse -> [(C.UnnamedId, Context)]
        analyseTypeFunArg arg = indirect arg.typ
