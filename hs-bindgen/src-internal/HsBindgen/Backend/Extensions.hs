module HsBindgen.Backend.Extensions (
    requiredExtensions,
) where

import Data.Set qualified as Set
import Language.Haskell.TH qualified as TH

import HsBindgen.Imports
import HsBindgen.SHs.AST
import HsBindgen.Hs.AST (Strategy (..))

-- | Which GHC language extensions this declarations needs.
requiredExtensions :: SDecl -> Set TH.Extension
requiredExtensions = \case
    DComment {} ->
        Set.empty
    DVar _name ty _expr ->
        typeExtensions ty
    DInst x -> Set.fromList $ catMaybes [
          if length (instanceArgs x) >= 2
            then Just TH.MultiParamTypeClasses
            else Nothing
        , if null (instanceTypes x)
            then Nothing
            else Just TH.TypeFamilies
        ]
    DRecord r ->
        recordExtensions r
    DNewtype{} ->
        Set.empty
    DEmptyData{} ->
        Set.singleton TH.EmptyDataDecls
    DDerivingInstance strategy ty ->
        Set.fromList [ TH.DerivingStrategies , TH.StandaloneDeriving ]
        <> strategyExtensions strategy
        <> typeExtensions ty
    DForeignImport{} ->
        -- Note: GHC doesn't require CApiFFI in TH: https://gitlab.haskell.org/ghc/ghc/-/issues/25774
        Set.singleton TH.CApiFFI
    DPatternSynonym{} ->
        Set.singleton TH.PatternSynonyms
    DCSource{} ->
        Set.singleton TH.TemplateHaskell

recordExtensions :: Record -> Set TH.Extension
recordExtensions r = foldMap fieldExtensions (dataFields r)

fieldExtensions :: Field -> Set TH.Extension
fieldExtensions f = typeExtensions (fieldType f)

-- Note: We don't recognise whether we need RankNTypes.
-- We probably don't generate such types
typeExtensions :: SType ctx -> Set TH.Extension
typeExtensions = \case
    TGlobal _ -> Set.empty
    TCon _    -> Set.empty
    TFun a b  -> typeExtensions a <> typeExtensions b
    TLit _    -> Set.singleton TH.DataKinds
    TExt _ _  -> Set.empty
    TBound _  -> Set.empty
    TApp f b  -> typeExtensions f <> typeExtensions b
    TForall _names _add preds b ->
        -- Note: GHC doesn't require ExplicitForAll for type signatures
        Set.singleton TH.ExplicitForAll <>
        foldMap typeExtensions preds <>
        foldMap predicateExtensions preds <>
        typeExtensions b

-- | Whether type constraints need extra language extensions.
--
-- For now, we over-approximate this by always requiring FlexibleContexts
predicateExtensions :: SType ctx' -> Set TH.Extension
predicateExtensions _ = Set.singleton TH.FlexibleContexts

strategyExtensions :: Strategy ClosedType -> Set TH.Extension
strategyExtensions = \case
    DeriveNewtype -> Set.singleton TH.GeneralizedNewtypeDeriving
    DeriveStock   -> Set.empty
    DeriveVia t   -> Set.insert TH.DerivingVia (typeExtensions t)
