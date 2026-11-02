{-# LANGUAGE OverloadedLabels #-}

-- | Parse the clang AST
module HsBindgen.Frontend.Pass.Parse (
    parseDecls
  ) where

import Data.List.NonEmpty qualified as NE
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
import HsBindgen.Macro.UniqueExpansion (Analysis, analyseMacroDefinitions,
                                        isFailure, parseDefinition)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

parseDecls ::
     forall l.
     Macro.Lang l
  -> ParseDecl.Env
  -> IO ([ParseResult l Parse], Analysis)
parseDecls macroLang parseEnv = do
    root <- clang_getTranslationUnitCursor parseEnv.unit
    ParseDecl.run parseEnv $ do
      resultsWithLocs <- HighLevel.clang_visitChildren root (topLevelDecl macroLang)
      let resultsOriginalOrder :: [ParseResult l Parse]
          resultsOriginalOrder = concatMap snd resultsWithLocs
      macroAnalysis <- analyseMacros
      -- 'resultsOriginalOrder' is in sequence order (see the orderings in
      -- "HsBindgen.Frontend.DeclDB"). We additionally record the source
      -- order of each declaration in its 'sourceOrderIndex', obtained by
      -- sorting the declarations by source position.
      --
      -- Comparing source positions across the translation unit requires
      -- 'clang_isBeforeInTranslationUnit', available only with Clang >= 20.1.
      -- On older versions we leave 'sourceOrderIndex' as 'Nothing' and return
      -- the declarations in sequence order, unchanged.
      (,macroAnalysis) <$> case clang_isBeforeInTranslationUnit of
        Just isBeforeInUnit -> do
          let isBefore (a, _) (b, _) = isBeforeInUnit a b
          resultsSourceOrder :: [ParseResult l Parse] <-
            liftIO $ concatMap snd <$> sortByM isBefore resultsWithLocs
          let -- The map is keyed on @('Id' 'Parse', 'SingleLoc')@ rather than
              -- just @'Id' 'Parse'@ to handle forward declarations at different
              -- locations with the same name.
              sourceOrderMap :: Map (Id Parse, SingleLoc C.DeclPath) Natural
              sourceOrderMap = Map.fromList
                [ ((parseResultId r, (parseResultInfo r).loc), i)
                | (i, r) <- zip [0..] resultsSourceOrder
                ]
          ParseDecl.traceImmediateGlobal ParseSourceOrderPopulated
          -- We only add source-order indices to successful parses here. We
          -- /could/ also populate them for non-successful parses.
          pure $ map (setSourceOrderIndex sourceOrderMap) resultsOriginalOrder
        Nothing -> do
          ParseDecl.traceImmediateGlobal ParseSourceOrderUnavailable
          pure resultsOriginalOrder

-- | Analyse all macro definitions for ambiguity
analyseMacros :: ParseDecl.ParseDecl Analysis
analyseMacros = do
    definitions <- map parseDefinition <$> ParseDecl.getMacroDefinitions
    forM_ (NE.nonEmpty $ mapMaybe isFailure definitions) $
      ParseDecl.traceImmediateGlobal . ParseMacroDefinitionAnalysisFailed
    pure $ analyseMacroDefinitions definitions

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
     Map (Id Parse, SingleLoc C.DeclPath) Natural
  -> ParseResult l Parse
  -> ParseResult l Parse
setSourceOrderIndex sourceOrderMap result =
    result
      &  #_ParseResultSuccess % #decl % #info % #sourceOrderIndex
      .~ Map.lookup
           (parseResultId result, (parseResultInfo result).loc)
           sourceOrderMap
