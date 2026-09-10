{-# LANGUAGE OverloadedLabels #-}

-- | Parse the clang AST
module HsBindgen.Frontend.Pass.Parse (
    parseDecls
  ) where

import Data.Map.Strict qualified as Map

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core

import HsBindgen.Frontend.Pass.Parse.Decl
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Monad.Decl qualified as ParseDecl
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Syntax (MacroDefinition)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

parseDecls ::
     forall l.
     Macro.Lang l
  -> ParseDecl.Env
  -> IO ([ParseResult l Parse], [MacroDefinition])
parseDecls macroLang parseEnv = do
    root <- clang_getTranslationUnitCursor parseEnv.unit
    ParseDecl.run parseEnv $ do
      resultsWithLocs <- HighLevel.clang_visitChildren root (topLevelDecl macroLang)
      let resultsOriginalOrder :: [ParseResult l Parse]
          resultsOriginalOrder = concatMap snd resultsWithLocs
      macroDefinitions <- ParseDecl.getMacroDefinitions
      -- 'resultsOriginalOrder' is in sequence order (the order in which
      -- libclang visits the declarations). We additionally record the source
      -- order of each declaration in its 'sourceOrderIndex', obtained by
      -- sorting the declarations by source position.
      --
      -- Comparing source positions across the translation unit requires
      -- 'clang_isBeforeInTranslationUnit', available only with Clang >= 20.1.
      -- On older versions we leave 'sourceOrderIndex' as 'Nothing' and return
      -- the declarations in sequence order, unchanged.
      (,macroDefinitions) <$> case clang_isBeforeInTranslationUnit of
        Just isBeforeInUnit -> do
          let isBefore (a, _) (b, _) = isBeforeInUnit a b
          resultsSourceOrder :: [ParseResult l Parse] <-
            liftIO $ concatMap snd <$> sortByM isBefore resultsWithLocs
          let -- The map is keyed on @('Id' 'Parse', 'SingleLoc')@ rather than
              -- just @'Id' 'Parse'@ to handle forward declarations at different
              -- locations with the same name.
              sourceOrderMap :: Map (Id Parse, SingleLoc) Natural
              sourceOrderMap = Map.fromList
                [ ((r.id, r.loc), i)
                | (i, r) <- zip [0..] resultsSourceOrder
                ]
          ParseDecl.traceImmediateGlobal ParseSourceOrderPopulated
          -- We only add source-order indices to successful parses here. We
          -- /could/ also populate them for non-successful parses.
          pure $ map (setSourceOrderIndex sourceOrderMap) resultsOriginalOrder
        Nothing -> do
          ParseDecl.traceImmediateGlobal ParseSourceOrderUnavailable
          pure resultsOriginalOrder

{-------------------------------------------------------------------------------
  Orderings

  We distinguish a few orderings of declarations (see issue #1580). These
  definitions live here for now; they should eventually move to wherever the
  orderings are defined and used centrally.

  * Sequence order: the order in which libclang presents the declarations to
    our parser (the order in which the cursor visits them). libclang visits
    macros first and the remaining declarations in source order, so sequence
    order is /not/ source order. This is the order of 'resultsOriginalOrder',
    and the order in which 'parseDecls' returns its results.

  * Source order: roughly, how the declarations appear in the C source. We
    record it per declaration in 'DeclInfo.sourceOrderIndex', computed above by
    sorting on source position via 'clang_isBeforeInTranslationUnit' (accurate,
    but requires Clang >= 20.1). Note that @annSortKey@ (in
    "HsBindgen.Frontend.Analysis.DeclUseGraph.Construction") computes a
    /best-effort/ source order without that API, used only as a tiebreak when
    ordering the output; it can be inaccurate, e.g. when a header includes
    another header part-way through its own declarations.

  * Dependency order: the order according to the use-decl graph; if @A@ has a
    by-value use of @B@ then @B@ comes before @A@. This is the order the rest of
    the frontend works in (established by @toDecls@ in
    "HsBindgen.Frontend.Analysis.DeclUseGraph.Query").
-------------------------------------------------------------------------------}

-- | Stable merge sort using a monadic strict-less-than predicate.
--
-- @isBefore x y@ should return 'True' iff @x@ strictly precedes @y@. When
-- neither element strictly precedes the other, ties are broken in favour of
-- the left input (stable).
sortByM :: Monad m => (a -> a -> m Bool) -> [a] -> m [a]
sortByM isBefore = go
  where
    go []  = pure []
    go [x] = pure [x]
    go xs  = do
      let (l, r) = splitAt (length xs `div` 2) xs
      l' <- go l
      r' <- go r
      merge l' r'

    merge []     ys     = pure ys
    merge xs     []     = pure xs
    merge (x:xs) (y:ys) = do
      yBeforeX <- isBefore y x
      if yBeforeX
        then (y :) <$> merge (x:xs) ys
        else (x :) <$> merge xs     (y:ys)

{-------------------------------------------------------------------------------
  Internal helpers
-------------------------------------------------------------------------------}

-- | Populate the source-order index in the 'DeclInfo' of a successful
--   'ParseResult'
setSourceOrderIndex ::
     Map (Id Parse, SingleLoc) Natural
  -> ParseResult l Parse
  -> ParseResult l Parse
setSourceOrderIndex sourceOrderMap result =
    result
      &  #classification % #_ParseResultSuccess % #decl % #info % #sourceOrderIndex
      .~ Map.lookup (result.id, result.loc) sourceOrderMap
