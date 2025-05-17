-- | Main entry point into the clang AST folding code
--
-- Intended for unqualified import.
module HsBindgen.C.Raw (
    getRawAST
  ) where

import Clang.LowLevel.Core

import HsBindgen.C.Raw.AST (Decl)
import HsBindgen.C.Raw.Graph.DefUse qualified as DefUseGraph
import HsBindgen.C.Raw.Graph.Includes (IncludeGraph)
import HsBindgen.C.Raw.Graph.UseDef qualified as UseDefGraph
import HsBindgen.C.Raw.Pass.Parse
import HsBindgen.C.Raw.Pass.RenameAnon

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- TODO: Think about naming (is this still "raw"?)
-- TODO: Do we actually need to return the includegraph here, or can we do
-- all processing that requires the includegraph here?
-- TODO: Can we avoid having macro errors in the "final" AST?
-- TODO: Can we distinguish between use cases for macros?
getRawAST :: CXCursor -> IO ([Decl Renamed], IncludeGraph)
getRawAST root = do
    (decls, includeGraph) <- parseRawAST root
    let useDefGraph = UseDefGraph.fromDecls includeGraph decls
        defUseGraph = DefUseGraph.fromUseDef useDefGraph
    putStrLn $ DefUseGraph.dumpMermaid defUseGraph
    let sorted  = UseDefGraph.toDecls useDefGraph
        renamed = renameAnon defUseGraph sorted
    -- TODO: Binding spec
    -- Only other use of IncludeGraph is TH "adddDpeendentFile" for all
    -- (transitive) dependencies
    -- TODO: Annotations in AST for external bindings
    -- TODO: Annotation for Haskell name
    -- TODO: TTG-style
    return (renamed, includeGraph)
