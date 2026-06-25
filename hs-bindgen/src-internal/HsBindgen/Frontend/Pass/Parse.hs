{-# LANGUAGE OverloadedLabels #-}

-- | Parse the clang AST
module HsBindgen.Frontend.Pass.Parse (
    parseDecls
  ) where

import Data.Map.Strict qualified as Map

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core

import HsBindgen.Clang.Macros (MacroDefinition)
import HsBindgen.Frontend.Pass.Parse.Decl
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Monad.Decl qualified as ParseDecl
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Macro.Interface qualified as Macro

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
      -- If the version of Clang is >= 20.1, we can obtain sequence order by
      -- sorting declarations by source position.
      --
      -- However, for older versions of Clang, the required API function is
      -- unavailable. Hence, we refrain from sorting declarations, and only
      -- populate the sequence order into 'DeclInfo'.
      (,macroDefinitions) <$> case clang_isBeforeInTranslationUnit of
        Just isBeforeInUnit -> do
          let isBefore (a, _) (b, _) = isBeforeInUnit a b
          resultsSequenceOrder :: [ParseResult l Parse] <-
            liftIO $ concatMap snd <$> sortByM isBefore resultsWithLocs
          let -- The map is keyed on @('Id' 'Parse', 'SingleLoc')@ rather than
              -- just @'Id' 'Parse'@ to handle forward declarations at different
              -- locations with the same name.
              seqNrMap :: Map (Id Parse, SingleLoc) Natural
              seqNrMap = Map.fromList
                [ ((r.id, r.loc), i)
                | (i, r) <- zip [0..] resultsSequenceOrder
                ]
          ParseDecl.traceImmediateGlobal ParseSeqNrPopulated
          -- We only add sequence numbers to successful parses here. We /could/
          -- also populate the sequence numbers to non-successful parses.
          pure $ map (setSeqNr seqNrMap) resultsOriginalOrder
        Nothing -> do
          ParseDecl.traceImmediateGlobal ParseSeqNrUnavailable
          pure resultsOriginalOrder

{-------------------------------------------------------------------------------
  Sequence order
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

-- | Populate the sequence number in the 'DeclInfo' of a successful
--   'ParseResult'
setSeqNr ::
     Map (Id Parse, SingleLoc) Natural
  -> ParseResult l Parse
  -> ParseResult l Parse
setSeqNr seqNrMap result =
    result
      &  #classification % #_ParseResultSuccess % #decl % #info % #seqNr
      .~ Map.lookup (result.id, result.loc) seqNrMap
