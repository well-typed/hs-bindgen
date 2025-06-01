-- | Parse the clang AST
module HsBindgen.Frontend.Pass.Parse (
    module HsBindgen.Frontend.Pass.Parse.IsPass
  , parseTranslationUnit
  ) where

import Clang.HighLevel qualified as HighLevel
import Clang.LowLevel.Core

import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Graph.UseDef qualified as UseDefGraph
import HsBindgen.Frontend.Pass.Parse.Decl
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Monad
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

parseTranslationUnit ::
     ParseEnv
  -> IO (C.TranslationUnit Parse)
parseTranslationUnit env = do
    root <- clang_getTranslationUnitCursor (envUnit env)
    (decls, outputGraph) <-
      fmap (first concat) . runParseMonad env $
        HighLevel.clang_visitChildren root foldDecl
    let useDefGraph = UseDefGraph.fromDecls outputGraph decls
    pure $ C.TranslationUnit {
        unitDecls        = UseDefGraph.toDecls useDefGraph
      , unitIncludeGraph = outputGraph
      , unitAnn          = useDefGraph
      }
