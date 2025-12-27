{-# LANGUAGE NoFieldSelectors  #-}
{-# LANGUAGE NoNamedFieldPuns  #-}
{-# LANGUAGE NoRecordWildCards #-}

-- | Parse the clang AST
module HsBindgen.Frontend.Pass.Parse (
    parseDecls
  ) where

import Clang.HighLevel qualified as HighLevel
import Clang.LowLevel.Core

import HsBindgen.Frontend.Pass.Parse.Decl
import HsBindgen.Frontend.Pass.Parse.Decl.Monad qualified as ParseDecl
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Result

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

parseDecls :: ParseDecl.Env -> CXTranslationUnit -> IO [ParseResult Parse]
parseDecls parseEnv unit = do
    root <- clang_getTranslationUnitCursor unit
    fmap concat . ParseDecl.run parseEnv $
      HighLevel.clang_visitChildren root topLevelDecl
