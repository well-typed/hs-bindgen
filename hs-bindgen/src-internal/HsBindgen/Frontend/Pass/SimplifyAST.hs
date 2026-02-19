{-# LANGUAGE NamedFieldPuns #-}

module HsBindgen.Frontend.Pass.SimplifyAST (
    simplifyAST
  ) where

import Data.Map.Strict qualified as Map

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.AnonUsage (AnonUsageAnalysis (..))
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (PrelimDeclId (..))
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Pass.SimplifyAST.IsPass (SimplifyAST,
                                                   SimplifyASTMsg (..))
import HsBindgen.Language.C (DeclName (..), NameKind (..), PrimType,
                             ScopedName (..))

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Simplify AST by converting anonymous enums (with no use sites) to pattern synonyms
--
-- Anonymous enums with no use sites (e.g., @enum { FOO, BAR }@) are converted into
-- separate pattern synonym declarations (e.g., @pattern fOO :: CUInt@, @pattern bAR :: CUInt@).
-- Anonymous enums that ARE used in type signatures are kept as-is.
simplifyAST ::
     AnonUsageAnalysis
  -> [ParseResult Parse]
  -> ([ParseResult SimplifyAST], [SimplifyASTMsg])
simplifyAST usage parseResults = (results, msgs)
  where
    processedResults = map processResult parseResults
    results = concatMap fst processedResults
    msgs = concatMap snd processedResults

    processResult :: ParseResult Parse -> ([ParseResult SimplifyAST], [SimplifyASTMsg])
    processResult result =
      case result.classification of
        ParseResultSuccess success ->
          case success.decl of
            -- Found anonymous enum: check if it has use sites
            C.Decl{info, kind = C.DeclEnum enum}
              | Anon anonId <- info.id
              , Map.notMember anonId usage.map ->
                ( [ result {
                     id = newId
                   , classification = ParseResultSuccess success {
                       decl = C.Decl{
                              info = newInfo
                            , kind = C.DeclAnonEnumConstant C.AnonEnumConstant{
                                     typ      = extractPrimType enum.typ
                                   , constant = coercePass constant
                                   }
                            , ann = NoAnn
                            }
                                          }
                   }
                 | constant <- enum.constants
                 , let ScopedName nameText = constant.info.name
                       newId = Named (DeclName nameText NameKindOrdinary)
                       newInfo :: C.DeclInfo SimplifyAST
                       newInfo = (coercePass info) { C.id = newId }
                 ]
                , [SimplifyASTAnonymousEnum anonId]
                )
            decl -> ( [ParseResult {
                      id = result.id
                    , loc = result.loc
                    , classification = ParseResultSuccess success {
                        decl = C.Decl{
                          info = coercePass decl.info
                        , kind = coercePass decl.kind
                        , ann = NoAnn
                        }
                      }
                    }]
                 , []
                 )
        ParseResultNotAttempted notAttempted ->
          ([ParseResult result.id result.loc (ParseResultNotAttempted notAttempted)], [])
        ParseResultFailure failure ->
          ([ParseResult result.id result.loc (ParseResultFailure failure)], [])

{-------------------------------------------------------------------------------
  Extract PrimType from Type
-------------------------------------------------------------------------------}

-- | Extract PrimType from a C.Type
-- Anonymous enum types are always primitive types (e.g., unsigned int, int, etc.)
extractPrimType :: C.Type Parse -> PrimType
extractPrimType (C.TypePrim pt) = pt
extractPrimType ty = panicPure $ concat [
    "Expected TypePrim but got "
  , show ty
  ]
