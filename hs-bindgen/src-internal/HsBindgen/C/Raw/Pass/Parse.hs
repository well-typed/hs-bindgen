-- | Parse the clang AST
module HsBindgen.C.Raw.Pass.Parse (
    module HsBindgen.C.Raw.Pass.Parse.IsPass
  , parseRawAST
  ) where

import Clang.HighLevel qualified as HighLevel
import Clang.LowLevel.Core

import HsBindgen.C.Raw.AST
import HsBindgen.C.Raw.Graph.Includes (IncludeGraph)
import HsBindgen.C.Raw.Pass.Parse.Decl
import HsBindgen.C.Raw.Pass.Parse.IsPass
import HsBindgen.C.Raw.Pass.Parse.Monad

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

parseRawAST :: CXCursor -> IO ([Decl Parsed], IncludeGraph)
parseRawAST root = do
    (decls, includeGraph) <-
      fmap aux . runParseMonad $
        HighLevel.clang_visitChildren root foldDecl
    return (decls, includeGraph)
  where
    aux :: ([[Decl Parsed]], IncludeGraph) -> ([Decl Parsed], IncludeGraph)
    aux (decls, includeGraph) = (concat decls, includeGraph)

