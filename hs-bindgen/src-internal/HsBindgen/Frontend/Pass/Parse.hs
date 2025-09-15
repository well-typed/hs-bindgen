-- | Parse the clang AST
module HsBindgen.Frontend.Pass.Parse (
    parseDecls
  ) where

import Clang.HighLevel qualified as HighLevel
import Clang.LowLevel.Core

import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass.Parse.Decl
import HsBindgen.Frontend.Pass.Parse.Decl.Monad qualified as ParseDecl
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Predicate qualified as Predicate
import HsBindgen.Frontend.ProcessIncludes
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
  -> GetMainHeadersAndInclude
  -> CXTranslationUnit
  -> IO (C.TranslationUnit Parse)
parseDecls
  tracer
  rootHeader
  predicate
  includeGraph
  isMainHeader
  isInMainHeaderDir
  getMainHeadersAndInclude
  unit = do
    let parseEnv :: ParseDecl.Env
        parseEnv  = ParseDecl.Env{
            envUnit                     = unit
          , envRootHeader               = rootHeader
          , envIsMainHeader             = isMainHeader
          , envIsInMainHeaderDir        = isInMainHeaderDir
          , envGetMainHeadersAndInclude = getMainHeadersAndInclude
          , envPredicate                = predicate
          , envTracer                   = tracer
          }
    root  <- clang_getTranslationUnitCursor unit
    (omittedDecls, decls) <- fmap (fmap concat) . ParseDecl.run parseEnv $
      HighLevel.clang_visitChildren root foldDecl
    let reifiedUnit = C.TranslationUnit{
        unitDecls        = decls
      , unitIncludeGraph = includeGraph
      , unitAnn          = omittedDecls
      }
    pure reifiedUnit
