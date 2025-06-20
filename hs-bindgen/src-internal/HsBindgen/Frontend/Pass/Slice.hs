module HsBindgen.Frontend.Pass.Slice (
    SliceSpec (..)
  , sliceDecls
  , SliceError
  ) where

import Data.Foldable qualified as Foldable
import Data.Set qualified as Set

import HsBindgen.C.Predicate (IsMainFile)
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph,
                                                 getTransitiveDeps)
import HsBindgen.Frontend.AST.Coerce (CoercePass (coercePass))
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass (Parse)
import HsBindgen.Frontend.Pass.Slice.IsPass (Slice)
import HsBindgen.Frontend.Pass.Sort.IsPass (DeclMeta (declUsage), Sort)

data SliceSpec = Slice | NoSlice

sliceDecls ::
     SliceSpec
  -> IsMainFile
  -> C.TranslationUnit Sort
  -> (C.TranslationUnit Slice, [SliceError])
sliceDecls sliceSpec isMainFile unitSort = case sliceSpec of
  NoSlice -> (unitSlice, [])
  -- With program slicing, we select all declarations while parsing, and slice
  -- declarations with transitive dependencies here. At the moment, we pick all
  -- declarations in the main header files and their transitive dependencies.
  Slice   -> let useDeclGraph :: UseDeclGraph
                 useDeclGraph = declUsage $ C.unitAnn $ unitSort

                 decls :: [C.Decl Parse]
                 decls = map coercePass $ C.unitDecls unitSort

                 mainDecls = filter (isMainFile . C.declLoc . C.declInfo) decls

                 getTransitives = getTransitiveDeps useDeclGraph . C.declQualId

                 transitiveDeps = Foldable.fold $ map getTransitives mainDecls

                 declsSlice :: [C.Decl Parse]
                 declsSlice = filter ((`Set.member` transitiveDeps) . C.declQualId) decls

              in (unitSlice { C.unitDecls = map coercePass declsSlice }, [])
  where
    unitSlice :: C.TranslationUnit Slice
    unitSlice = coercePass unitSort

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data SliceError
