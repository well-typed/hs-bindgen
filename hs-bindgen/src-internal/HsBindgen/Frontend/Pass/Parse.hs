-- | Parse the clang AST
module HsBindgen.Frontend.Pass.Parse (
    module HsBindgen.Frontend.Pass.Parse.IsPass
  , parseTranslationUnit
  ) where

import Data.Bifunctor

import Clang.HighLevel qualified as HighLevel
import Clang.LowLevel.Core

import HsBindgen.Frontend.AST
import HsBindgen.Frontend.Graph.UseDef qualified as UseDefGraph
import HsBindgen.Frontend.Pass.Parse.Decl
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Monad

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

parseTranslationUnit ::
     CXTranslationUnit
  -> IO (TranslationUnit Parse, [UnsupportedError])
parseTranslationUnit unit = do
    root <- clang_getTranslationUnitCursor unit
    (decls, ExtraOutput{outputGraph, outputErrors}) <-
      fmap (first concat) . runParseMonad unit $
        HighLevel.clang_visitChildren root foldDecl
    let useDefGraph = UseDefGraph.fromDecls outputGraph decls
    return (
        TranslationUnit{
            unitDecls        = UseDefGraph.toDecls useDefGraph
          , unitIncludeGraph = outputGraph
          , unitAnn          = useDefGraph
          }
      , outputErrors
      )

