{-# LANGUAGE OverloadedLabels #-}

-- | Parse the clang AST
module HsBindgen.Frontend.Pass.Parse (
    parseDecls
  ) where

import Data.Map.Strict qualified as Map

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core

import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Decl
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Monad.Decl qualified as ParseDecl
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

parseDecls :: ParseDecl.Env -> IO [ParseResult Parse]
parseDecls parseEnv = do
    root <- clang_getTranslationUnitCursor parseEnv.unit
    ParseDecl.run parseEnv $ do
      resultsWithLocs <- HighLevel.clang_visitChildren root topLevelDecl
      allMacros <- reverse <$> ParseDecl.getMacroDefinitions
      let resultsOriginalOrder :: [ParseResult Parse]
          resultsOriginalOrder = map snd allMacros ++ concatMap snd resultsWithLocs
      -- If the version of Clang is >= 20.1, we can obtain sequence order by
      -- threading in macros between non-macro declarations.
      --
      -- However, for older version of Clang, the required API function is
      -- unavailable. Hence, we refrain from sorting declarations, and only
      -- populate the sequence order into 'DeclInfo'.
      case clang_isBeforeInTranslationUnit of
        Just isBeforeInUnit -> do
          resultsSequenceOrder :: [ParseResult Parse] <-
            liftIO $ threadMacros isBeforeInUnit allMacros resultsWithLocs
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

-- | Thread macros in-between non-macro declarations to obtain sequence order.
threadMacros ::
     (CXSourceLocation -> CXSourceLocation -> IO Bool)
  -> [(CXSourceLocation, ParseResult Parse)]
  -> [(CXSourceLocation, [ParseResult Parse])]
  -> IO [ParseResult Parse]
threadMacros p macros = \case
    [] ->
      pure $ map snd macros
    (x:xs) -> do
      (macrosBefore, macrosAfter) <- spanMacroDefinitions p macros (fst x)
      xs' <- threadMacros p macrosAfter xs
      pure $ map snd macrosBefore ++ snd x ++ xs'

-- | Similar to 'Data.List.span' but with a monadic predicate.
spanMacroDefinitions ::
     (CXSourceLocation -> CXSourceLocation -> IO Bool)
  -> [(CXSourceLocation, ParseResult Parse)]
  -> CXSourceLocation
  -> IO ( [(CXSourceLocation, ParseResult Parse)]
        , [(CXSourceLocation, ParseResult Parse)] )
spanMacroDefinitions isBeforeInTranslationUnit macros rhs = do
      (macrosBefore, macrosAfter) <- spanM isBefore macros
      pure (macrosBefore, macrosAfter)
  where
    isBefore :: (CXSourceLocation, ParseResult Parse) -> IO Bool
    isBefore (lhs, _) = isBeforeInTranslationUnit lhs rhs

    spanM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
    spanM _ []    = pure ([], [])
    spanM f (x:xs) = do
      ok <- f x
      if ok
        then first (x :) <$> spanM f xs
        else pure ([], x : xs)

{-------------------------------------------------------------------------------
  Internal helpers
-------------------------------------------------------------------------------}

-- | Populate the sequence number in the 'DeclInfo' of a successful
--   'ParseResult'
setSeqNr ::
     Map (Id Parse, SingleLoc) Natural
  -> ParseResult Parse
  -> ParseResult Parse
setSeqNr seqNrMap result =
    result
      &  #classification % #_ParseResultSuccess % #decl % #info % #seqNr
      .~ Map.lookup (result.id, result.loc) seqNrMap
