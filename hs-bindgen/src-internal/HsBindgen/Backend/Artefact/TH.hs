module HsBindgen.Backend.Artefact.TH
  ( getExtensions
  , getThDecls

    -- * Internal; used in tests
  , genBindingsFromCHeader
  ) where

import Data.Set qualified as Set
import HsBindgen.Backend.Artefact.TH.Translation
import HsBindgen.Backend.Extensions
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Guasi
import Language.Haskell.TH qualified as TH

import Clang.Paths
import HsBindgen
import HsBindgen.Imports

-- | Get required extensions.
getExtensions :: Applicative f => Artefact f (Set TH.Extension)
getExtensions =
    Lift
      (FinalDecls :* Nil)
      (\(I decls :* Nil) -> pure $ foldMap requiredExtensions decls)

-- | Get Template Haskell declarations.
getThDecls :: Guasi q => Artefact q [TH.Dec]
getThDecls =
    Lift
      (Dependencies :* FinalDecls :* getExtensions :* Nil)
      (\(I deps :* I decls :* I requiredExts :* Nil) ->
         genBindingsFromCHeader deps decls requiredExts
      )

-- | Internal; used in tests; non-IO part of 'withHsBindgen'
genBindingsFromCHeader
    :: Guasi q
    => [SourcePath]
    -> [SHs.SDecl]
    -> Set TH.Extension
    -> q [TH.Dec]
genBindingsFromCHeader deps decls requiredExts = do
    -- Record dependencies, including transitively included headers.
    mapM_ (addDependentFile . getSourcePath) deps

    -- Check language extensions.
    --
    -- TODO: We could also check which enabled extension may interfere with the
    -- generated code (e.g. Strict/Data).
    enabledExts <- Set.fromList <$> extsEnabled
    let missingExts  = requiredExts `Set.difference` enabledExts
    -- TODO: The following error should be a trace.
    unless (null missingExts) $ do
      reportError $ "Missing LANGUAGE extensions: "
        ++ unwords (map show (toList missingExts))

    -- Generate TH declarations.
    fmap concat $ traverse mkDecl decls
