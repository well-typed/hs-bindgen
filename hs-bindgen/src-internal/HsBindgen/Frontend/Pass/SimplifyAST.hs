{-# LANGUAGE NamedFieldPuns #-}

module HsBindgen.Frontend.Pass.SimplifyAST (
    simplifyAST
  ) where

import Data.List (mapAccumL)
import Data.Map.Strict qualified as Map

import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (AnonId, PrelimDeclId (..))
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Pass.SimplifyAST.IsPass (SimplifyAST,
                                                   SimplifyASTMsg (..))
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Simplify AST by converting anonymous enums to pattern synonyms
--
-- Anonymous enums (e.g., @enum { FOO, BAR }@) are converted into
-- separate pattern synonym declarations (e.g., @pattern fOO :: CUInt@, @pattern bAR :: CUInt@)
-- and references to the anonymous enum type are replaced with the underlying type.
--
-- We process declarations in a single pass, building a map of anonymous enum IDs
-- to their underlying types as we go. This works because the C parser always
-- extracts anonymous types before the declarations that reference them.
simplifyAST ::
     [ParseResult Parse]
  -> ([ParseResult SimplifyAST], [SimplifyASTMsg])
simplifyAST parseResults = (concat results, concat msgs)
  where
    (_, resultsAndMsgs) = mapAccumL processResult Map.empty parseResults
    (results, msgs) = unzip resultsAndMsgs

    processResult :: Map.Map AnonId (C.Type Parse)
                  -> ParseResult Parse
                  -> (Map.Map AnonId (C.Type Parse), ([ParseResult SimplifyAST], [SimplifyASTMsg]))
    processResult anonEnumTypes result =
      case result.classification of
        ParseResultSuccess success ->
          case success.decl of
            -- Found anonymous enum: transform it and add to map for future references
            C.Decl{info, kind = C.DeclEnum enum}
              | Anon anonId <- info.id ->
                  let declResults = simplifyDecl anonEnumTypes result success success.decl
                      msg = SimplifyASTAnonymousEnum anonId
                      anonEnumTypes' = Map.insert anonId enum.typ anonEnumTypes
                  in (anonEnumTypes', (declResults, [msg]))
            -- Other declaration: transform using current map
            _ ->
              let declResults = simplifyDecl anonEnumTypes result success success.decl
              in (anonEnumTypes, (declResults, []))
        ParseResultNotAttempted notAttempted ->
          (anonEnumTypes, ([ParseResult result.id result.loc (ParseResultNotAttempted notAttempted)], []))
        ParseResultFailure failure ->
          (anonEnumTypes, ([ParseResult result.id result.loc (ParseResultFailure failure)], []))

{-------------------------------------------------------------------------------
  Simplify declarations
-------------------------------------------------------------------------------}

simplifyDecl ::
     Map.Map AnonId (C.Type Parse)
  -> ParseResult Parse
  -> ParseSuccess Parse
  -> C.Decl Parse
  -> [ParseResult SimplifyAST]
simplifyDecl anonEnumTypes result success C.Decl{info, kind} =
  case kind of
    C.DeclEnum enum
      -- Anonymous enum: convert each constant to a pattern synonym declaration
      | Anon _ <- info.id ->
         [ result {
               id = newId
             , classification = ParseResultSuccess success {
                 decl = C.Decl{
                        info = newInfo
                      , kind = C.DeclAnonEnumConstant C.AnonEnumConstant{
                               typ      = coercePass enum.typ
                             , constant = coercePass constant
                             }
                      , ann = NoAnn
                      }
                                        }
             }
           | constant <- enum.constants
           , let newId = Named (C.DeclName constant.info.name.text C.NameKindOrdinary)
                 newInfo :: C.DeclInfo SimplifyAST
                 newInfo = (coercePass info) { C.id = newId }
           ]
    -- Other declarations: keep but replace anonymous enum type references
    _otherwise ->
      let
        kind' = replaceAnonEnumRefs anonEnumTypes kind
      in [ParseResult {
              id = result.id
            , loc = result.loc
            , classification = ParseResultSuccess success {
              decl = C.Decl{
                info = coercePass info
              , kind = kind'
              , ann = NoAnn
              }
            }
          }]

{-------------------------------------------------------------------------------
  Replace anonymous enum references with underlying types
-------------------------------------------------------------------------------}

replaceAnonEnumRefs :: Map.Map AnonId (C.Type Parse) -> C.DeclKind Parse -> C.DeclKind SimplifyAST
replaceAnonEnumRefs anonEnumTypes = \case
  C.DeclStruct struct ->
    C.DeclStruct $ C.Struct {
    C.fields      = map (replaceInStructField anonEnumTypes) struct.fields
    , C.flam      = fmap (replaceInStructField anonEnumTypes) struct.flam
    , C.sizeof    = struct.sizeof
    , C.alignment = struct.alignment
    , C.ann       = struct.ann
    }
  C.DeclUnion union ->
    C.DeclUnion $ C.Union {
    C.fields      = map (replaceInUnionField anonEnumTypes) union.fields
    , C.sizeof    = union.sizeof
    , C.alignment = union.alignment
    , C.ann       = union.ann
    }
  C.DeclTypedef typedef ->
    C.DeclTypedef $ C.Typedef {
      C.typ = replaceInType anonEnumTypes typedef.typ
    , C.ann = typedef.ann
    }
  C.DeclEnum enum ->
    C.DeclEnum $ coercePass enum
  C.DeclFunction function ->
    C.DeclFunction $ C.Function {
      C.args  = [ (name, replaceInType anonEnumTypes ty)
                | (name, ty) <- function.args
                ]
    , C.res   = replaceInType anonEnumTypes function.res
    , C.attrs = function.attrs
    , C.ann   = function.ann
    }
  C.DeclAnonEnumConstant anonEnumConst ->
    C.DeclAnonEnumConstant (coercePass anonEnumConst)
  C.DeclGlobal typ ->
    C.DeclGlobal $ replaceInType anonEnumTypes typ
  C.DeclMacro macro -> C.DeclMacro macro
  C.DeclOpaque -> C.DeclOpaque

replaceInStructField :: Map.Map AnonId (C.Type Parse) -> C.StructField Parse -> C.StructField SimplifyAST
replaceInStructField anonEnumTypes field =
  C.StructField {
    C.typ    = replaceInType anonEnumTypes field.typ
  , C.info   = coercePass field.info
  , C.offset = field.offset
  , C.width  = field.width
  , C.ann    = field.ann
  }

replaceInUnionField :: Map.Map AnonId (C.Type Parse) -> C.UnionField Parse -> C.UnionField SimplifyAST
replaceInUnionField anonEnumTypes field =
  C.UnionField {
    C.typ  = replaceInType anonEnumTypes field.typ
  , C.info = coercePass field.info
  , C.ann  = field.ann
  }

replaceInType :: Map.Map AnonId (C.Type Parse) -> C.Type Parse -> C.Type SimplifyAST
replaceInType anonEnumTypes = go
  where
    go :: C.Type Parse -> C.Type SimplifyAST
    go = \case
      -- Replace anonymous enum reference with underlying type
      C.TypeRef (Anon anonId)
        | Just underlyingType <- Map.lookup anonId anonEnumTypes ->
          coercePass underlyingType

      -- Keep other references as-is
      C.TypeRef ref -> C.TypeRef ref

      C.TypeTypedef ref ->
        C.TypeTypedef $ C.Ref ref.name (go ref.underlying)

      -- Recursive cases
      C.TypePointers n ty      -> C.TypePointers n (go ty)
      C.TypeConstArray n ty    -> C.TypeConstArray n (go ty)
      C.TypeIncompleteArray ty -> C.TypeIncompleteArray (go ty)
      C.TypeBlock ty           -> C.TypeBlock (go ty)
      C.TypeQual qual ty       -> C.TypeQual qual (go ty)
      C.TypeFun args res       -> C.TypeFun (map go args) (go res)

      -- Simple cases
      C.TypeVoid       -> C.TypeVoid
      C.TypePrim pt    -> C.TypePrim pt
      C.TypeComplex pt -> C.TypeComplex pt
