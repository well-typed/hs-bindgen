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
    DComment {} -> mconcat [
      ]
    DVar _name ty _expr -> mconcat [
        typeExtensions ty
      ]
    DInst x -> mconcat . concat $ [
        [ext TH.MultiParamTypeClasses | length (instanceArgs x) >= 2]
      , [ext TH.TypeFamilies          | not (null (instanceTypes x))]
      ]
    DRecord r -> mconcat [
        recordExtensions r
      , nestedDeriving (dataDeriv r)
      ]
    DNewtype n -> mconcat [
        nestedDeriving (newtypeDeriv n)
      ]
    DEmptyData{} -> mconcat [
        ext TH.EmptyDataDecls
      ]
    DDerivingInstance strategy ty -> mconcat [
        Set.fromList [
            TH.DerivingStrategies
          , TH.StandaloneDeriving
          ]
      , strategyExtensions strategy
      , typeExtensions ty
      ]
    DForeignImport ForeignImport{foreignImportType} -> mconcat [
        -- Note: GHC doesn't require CApiFFI in TH: https://gitlab.haskell.org/ghc/ghc/-/issues/25774
        ext TH.CApiFFI
      , typeExtensions foreignImportType
      ]
    DPatternSynonym{} -> mconcat [
        ext TH.PatternSynonyms
      ]
    DCSource{} -> mconcat [
        ext TH.TemplateHaskell
      ]
  where
    ext :: TH.Extension -> Set TH.Extension
    ext = Set.singleton

-- | Extensions for deriving clauses that are part of the datatype declaration
nestedDeriving :: [(Strategy ClosedType, [Global])] -> Set TH.Extension
nestedDeriving deriv =
       Set.singleton TH.DerivingStrategies
    <> mconcat (map (strategyExtensions . fst) deriv)

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
