{-# LANGUAGE NamedFieldPuns #-}

module HsBindgen.Frontend.Pass.SimplifyAST (
    simplifyAST
  ) where

import Data.Map.Strict qualified as Map
import GHC.Stack (HasCallStack)

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.AnonUsage (AnonUsageAnalysis (..))
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Pass.SimplifyAST.IsPass (SimplifyAST,
                                                   SimplifyASTMsg (..))
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Language.C qualified as C
import HsBindgen.Util.Tracer (withCallStack)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Simplify AST by converting anonymous enums (with no use sites) to pattern synonyms
--
-- Anonymous enums with no use sites (e.g., @enum { FOO, BAR }@) are converted into
-- separate pattern synonym declarations (e.g., @pattern fOO :: CUInt@, @pattern bAR :: CUInt@).
-- Anonymous enums that ARE used in type signatures are kept as-is.
simplifyAST ::
     HasCallStack
  => AnonUsageAnalysis
  -> [ParseResult l Parse]
  -> ([ParseResult l SimplifyAST], [AMsg SimplifyAST])
simplifyAST usage parseResults = (results, msgs)
  where
    processedResults = map processResult parseResults
    results = concatMap fst processedResults
    msgs = concatMap snd processedResults

    processResult ::
         HasCallStack
      => ParseResult l Parse
      -> ([ParseResult l SimplifyAST], [AMsg SimplifyAST])
    processResult result =
      case result.classification of
        ParseResultSuccess success ->
          case success.decl of
            -- Found anonymous enum: check if it has use sites
            C.Decl{info, kind = C.DeclEnum enum}
              | C.PrelimDeclIdAnon anonId <- info.id
              , Map.notMember anonId usage.map ->
                ( [ result {
                     id = newId
                   , classification = ParseResultSuccess success {
                       decl = C.Decl{
                              info = newInfo
                            , kind = C.DeclAnonEnumConstant C.AnonEnumConstant{
                                     typ      = extractPrimType enum.typ
                                   , constant = C.coercePass constant
                                   }
                            , ann = NoAnn
                            }
                                          }
                   }
                 | constant <- enum.constants
                 , let C.ScopedName nameText = constant.info.name
                       newId = C.PrelimDeclIdNamed (C.DeclName nameText C.NameKindOrdinary)
                       newInfo :: C.DeclInfo SimplifyAST
                       newInfo = (C.coercePass info :: C.DeclInfo SimplifyAST)
                                   { C.id = newId }
                 ]
                , [withCallStack (SimplifyASTAnonymousEnum anonId)]
                )
            decl -> ( [ParseResult {
                      id = result.id
                    , loc = result.loc
                    , classification = ParseResultSuccess success {
                        decl = C.Decl{
                          info = C.coercePass decl.info
                        , kind = C.coercePass decl.kind
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
extractPrimType :: C.Type Parse -> C.PrimType
extractPrimType (C.TypePrim pt) = pt
extractPrimType ty = panicPure $ concat [
    "Expected TypePrim but got "
  , show ty
  ]
