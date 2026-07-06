-- | Zip reparsed declarations with their pre-reparse representations
module HsBindgen.Frontend.Pass.ReparseMacroExpansions.Intermediate.Zip (
    zip
  ) where

import Prelude hiding (zip)

import Data.Either
import Data.Foldable qualified as Foldable
import Data.Set qualified as Set

import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph)
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.DeclMeta
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.Intermediate.LanC.IsPass
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.Intermediate.Zip.IsPass
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.Intermediate.Zip.Zip (zipEither)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass (ReparseMacroExpansions)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass.Msg (DelayedReparseMacroExpansionsMsg)
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Macro.Flip
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Type qualified as Macro

-- | Zip reparsed declarations with their original definitions
zip ::
     forall l. Macro.HasTypes l
  => Macro.Lang l
  -> C.TranslationUnit l LanC
  -> C.TranslationUnit l Zip
zip macroLang unit =
    let (failures, successes) = zipDecls unit.decls
    in C.TranslationUnit{
           decls        = successes
         , includeGraph = unit.includeGraph
         , meta         = updateMeta macroLang failures successes unit.meta
         }

updateMeta ::
     forall l. Macro.HasTypes l
  => Macro.Lang l
  -> [(C.DeclId, [DelayedReparseMacroExpansionsMsg])]
  -> [C.Decl l Zip]
  -> DeclMeta l
  -> DeclMeta l
updateMeta macroLang failures successes meta = DeclMeta{
      declIndex    = updatedDeclIndex
    , useDeclGraph = UseDeclGraph.fromDeclUseGraph updatedDeclUseGraph
    , declUseGraph = updatedDeclUseGraph
    }
  where
    updatedDeclIndex :: DeclIndex l
    updatedDeclIndex =
      -- We use @foldr@ here to establish the original order of reparse
      -- warnings.
      foldr
        DeclIndex.registerDelayedReparseMacroExpansionsMsg
        meta.declIndex
        [ (declId, msg)
        | (declId, msgs) <- failures
        , msg            <- msgs
        ]

    updatedDeclUseGraph :: DeclUseGraph
    updatedDeclUseGraph =
      Foldable.foldl'
        (flip (updateDeps macroLang))
        meta.declUseGraph
        (map coercePass successes)

-- | Dependencies before reparse may point to underlying types. These have to be
-- replaced with their actual dependencies after reparse+zip.
--
-- For example,
--
-- @
-- // c_header.h
-- typedef int A;
-- #define B A
-- void foo(B x);
-- @
--
-- Before reparse, @foo@ directly depends on @A@. After reparse+zip, we know
-- that @foo@ depends on @B@. We have to cut the dependency to @A@ and
-- replace it with the dependency to @B@.
updateDeps ::
     Macro.HasTypes l
  => Macro.Lang l
  -> C.Decl l ReparseMacroExpansions
  -> DeclUseGraph
  -> DeclUseGraph
updateDeps macroLang decl graph =
    DeclUseGraph.insertDepsOfDecl macroLang decl $
      DeclUseGraph.deleteDeps (Set.singleton decl.info.id) graph

{-------------------------------------------------------------------------------
  Per-decl zipping
-------------------------------------------------------------------------------}

zipDecls ::
     forall l.
     [C.Decl l LanC]
  -> ([(C.DeclId, [DelayedReparseMacroExpansionsMsg])], [C.Decl l Zip])
zipDecls decls = partitionEithers $ map zipDecl decls

zipDecl ::
     C.Decl l LanC
  -> (Either (C.DeclId, [DelayedReparseMacroExpansionsMsg]) (C.Decl l Zip))
zipDecl decl = reconstruct $ case decl.kind of
    C.DeclStruct           x -> C.DeclStruct           <$> zipEither x
    C.DeclUnion            x -> C.DeclUnion            <$> zipEither x
    C.DeclTypedef          x -> C.DeclTypedef          <$> zipEither x
    C.DeclEnum             x -> C.DeclEnum             <$> zipEither x
    C.DeclAnonEnumConstant x -> C.DeclAnonEnumConstant <$> zipEither x
    C.DeclOpaque mSize       -> Right (C.DeclOpaque mSize)
    C.DeclMacro            x -> C.DeclMacro            <$> flipM zipEither x
    C.DeclFunction         x -> C.DeclFunction         <$> zipEither x
    C.DeclGlobal           x -> C.DeclGlobal           <$> zipEither x
  where
    reconstruct ::
         forall l.
         Either [DelayedReparseMacroExpansionsMsg] (C.DeclKind l Zip)
      -> Either (C.DeclId, [DelayedReparseMacroExpansionsMsg]) (C.Decl l Zip)
    reconstruct = \case
      Left xs     ->
        Left (decl.info.id, xs)
      Right kind' ->
        let decl' :: C.Decl l Zip
            decl' = C.Decl{
                 info = coercePass decl.info
               , kind = kind'
               , ann  = NoAnn
              }
        in  Right decl'
