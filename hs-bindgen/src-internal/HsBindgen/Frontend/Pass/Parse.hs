-- | Parse the clang AST
module HsBindgen.Frontend.Pass.Parse (parseDecls) where

import Clang.HighLevel qualified as HighLevel
import Clang.LowLevel.Core

import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass.Parse.Decl
import HsBindgen.Frontend.Pass.Parse.Decl.Monad qualified as ParseDecl
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.ProcessIncludes (GetMainHeader)
import HsBindgen.Frontend.RootHeader (RootHeader)
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

parseDecls ::
     Tracer IO ParseMsg
  -> RootHeader
  -> Predicate.ParsePredicate
  -> IncludeGraph
  -> Predicate.IsMainHeader
  -> Predicate.IsInMainHeaderDir
  -> GetMainHeader
  -> CXTranslationUnit
  -> IO (C.TranslationUnit Parse)
parseDecls
  tracer
  rootHeader
  predicate
  includeGraph
  isMainHeader
  isInMainHeaderDir
  getMainHeader
  unit = do
    root  <- clang_getTranslationUnitCursor unit
    (omittedDecls, decls) <- fmap (fmap concat) . ParseDecl.run parseEnv $
      HighLevel.clang_visitChildren root foldDecl
    return C.TranslationUnit{
        unitDecls        = decls
      , unitIncludeGraph = includeGraph
      , unitAnn          = omittedDecls
      }
  where
    parseEnv :: ParseDecl.Env
    parseEnv = ParseDecl.Env{
          envUnit              = unit
        , envRootHeader        = rootHeader
        , envIsMainHeader      = isMainHeader
        , envIsInMainHeaderDir = isInMainHeaderDir
        , envGetMainHeader     = getMainHeader
        , envPredicate         = predicate
        , envTracer            = tracer
        }
