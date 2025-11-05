-- | Parse the clang AST
module HsBindgen.Frontend.Pass.Parse (
    parseDecls
  ) where

import Clang.HighLevel qualified as HighLevel
import Clang.LowLevel.Core

import HsBindgen.Frontend.Pass.Parse.Decl
import HsBindgen.Frontend.Pass.Parse.Decl.Monad qualified as ParseDecl
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Frontend.ProcessIncludes
import HsBindgen.Frontend.RootHeader (RootHeader)
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

parseDecls ::
     Tracer IO ImmediateParseMsg
  -> RootHeader
  -> Boolean ParsePredicate
  -> IsMainHeader
  -> IsInMainHeaderDir
  -> GetMainHeadersAndInclude
  -> CXTranslationUnit
  -> IO [ParseResult]
parseDecls
  tracer
  rootHeader
  predicate
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
    fmap concat . ParseDecl.run parseEnv $
      HighLevel.clang_visitChildren root topLevelDecl
