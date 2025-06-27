module HsBindgen.Frontend.Pass.Slice (
    ProgramSlicing (..)
  , sliceDecls
  , SliceMsg (..)
  ) where

import Data.Foldable qualified as Foldable
import Data.List (partition)
import Data.Set (Set)
import Data.Set qualified as Set

import Clang.HighLevel.Types (SingleLoc (singleLocPath))
import HsBindgen.C.Predicate (IsMainFile, Predicate)
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph,
                                                 getTransitiveDeps)
import HsBindgen.Frontend.AST.Coerce (CoercePass (coercePass))
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.NonSelectedDecls (NonSelectedDecls, insert)
import HsBindgen.Frontend.Pass.Parse.IsPass (DeclId (..), QualDeclId (..),
                                             declQualDeclId)
import HsBindgen.Frontend.Pass.Slice.IsPass (Slice)
import HsBindgen.Frontend.Pass.Sort.IsPass (DeclMeta (declNonSelected, declUsage),
                                            Sort)
import HsBindgen.Language.C.Name qualified as C
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint ((><))

data ProgramSlicing =
  -- | Enable program slicing: Select declarations using the selection predicate
  -- /and/ their transitive dependencies.
  EnableProgramSlicing
  | DisableProgramSlicing
  deriving stock (Show, Eq)

sliceDecls ::
     ProgramSlicing
  -> Predicate
  -> IsMainFile
  -> C.TranslationUnit Sort
  -> (C.TranslationUnit Slice, [SliceMsg])
sliceDecls programSlicing _selectionPredicate isMainFile unitSort = case programSlicing of
  DisableProgramSlicing -> (unitSlice, [])
  -- When program slicing is enabled, we select all declarations while parsing.
  -- Instead, we apply the selection predicate here, and also select all
  -- transitive dependencies.
  --
  -- TODO: https://github.com/well-typed/hs-bindgen/issues/784. However, at the
  -- moment, we only pick all declarations in the main header files and their
  -- transitive dependencies.
  EnableProgramSlicing ->
    let useDeclGraph :: UseDeclGraph
        useDeclGraph = declUsage $ C.unitAnn $ unitSlice

        decls :: [C.Decl Slice]
        decls = C.unitDecls unitSlice

        mainDecls :: [C.Decl Slice]
        mainDecls = filter (isMainFile . C.declLoc . C.declInfo) decls

        getTransitives :: C.Decl Slice -> Set QualDeclId
        getTransitives = getTransitiveDeps useDeclGraph . declQualDeclId

        transitiveDeps :: Set QualDeclId
        transitiveDeps = Foldable.fold $ map getTransitives mainDecls

        slicedDecls, nonSelectedDecls :: [C.Decl Slice]
        (slicedDecls, nonSelectedDecls) =
          partition ((`Set.member` transitiveDeps) . declQualDeclId) decls

        unavailableTransitiveDeps :: Set QualDeclId
        unavailableTransitiveDeps = transitiveDeps `Set.difference` (Set.fromList $ map declQualDeclId slicedDecls)

        nonSelectedDecls' :: NonSelectedDecls
        nonSelectedDecls' = Foldable.foldl' insertNonSelected
          (declNonSelected $ C.unitAnn unitSort) nonSelectedDecls

        declMeta' :: DeclMeta
        declMeta' = (C.unitAnn unitSlice) { declNonSelected = nonSelectedDecls'}

        errors :: [SliceMsg]
        errors = map TransitiveDependencyUnavailable $ Set.toList unavailableTransitiveDeps
     in
      (unitSlice { C.unitDecls = slicedDecls, C.unitAnn = declMeta' }, errors)
  where
    unitSlice :: C.TranslationUnit Slice
    unitSlice = coercePass unitSort

    insertNonSelected :: NonSelectedDecls -> C.Decl Slice -> NonSelectedDecls
    insertNonSelected nonSelectedDecls decl =
      let (QualDeclId declId nameKind) = declQualDeclId decl
          sourcePath = singleLocPath (C.declLoc $ C.declInfo decl)
       in case declId of
         DeclNamed cName ->
           insert (C.QualName cName nameKind) sourcePath nonSelectedDecls
         -- Refer to 'recordNonSelectedDecl'.
         _anonymous      -> nonSelectedDecls

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data SliceMsg = TransitiveDependencyUnavailable QualDeclId
  deriving stock (Show, Eq)

instance PrettyForTrace SliceMsg where
  prettyForTrace (TransitiveDependencyUnavailable qualId) =
    "Program slicing: Transitive dependency unavailable: " >< prettyForTrace qualId

instance HasDefaultLogLevel SliceMsg where
  getDefaultLogLevel = const Error

instance HasSource SliceMsg where
  getSource = const HsBindgen
