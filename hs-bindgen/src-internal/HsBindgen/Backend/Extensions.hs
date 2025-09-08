module HsBindgen.Backend.Extensions (
    requiredExtensions,
) where

import Data.Set qualified as Set
import Language.Haskell.TH qualified as TH

import HsBindgen.Backend.Hs.AST (Strategy (..))
import HsBindgen.Backend.Hs.AST.Type (extractResultType)
import HsBindgen.Backend.SHs.AST
import HsBindgen.Imports

-- | Which GHC language extensions this declarations needs.
requiredExtensions :: SDecl -> Set TH.Extension
requiredExtensions = \case
    DVar Var {..} -> mconcat $ [
        typeExtensions varType
      , Set.fromList [TH.MagicHash | isECString varExpr]
      ]
    DInst x -> mconcat . concat $ [
        [ext TH.MultiParamTypeClasses | length (instanceArgs x) >= 2]
      , [ext TH.TypeFamilies          | not (null (instanceTypes x))]
      , typeExtensions <$> instanceArgs x
      ]
    DRecord r -> mconcat [
        recordExtensions r
      , nestedDeriving (dataDeriv r)
      ]
    DNewtype n -> mconcat [
        nestedDeriving (newtypeDeriv n)
      , typeExtensions $ fieldType $ newtypeField n
      ]
    DEmptyData{} -> mconcat [
        ext TH.EmptyDataDecls
      ]
    DDerivingInstance DerivingInstance {..} -> mconcat [
        Set.fromList [
            TH.DerivingStrategies
          , TH.StandaloneDeriving
          ]
      , strategyExtensions derivingInstanceStrategy
      , typeExtensions derivingInstanceType
      ]
    DForeignImport ForeignImport {..} -> mconcat [
        -- Note: GHC doesn't require CApiFFI in TH: https://gitlab.haskell.org/ghc/ghc/-/issues/25774
        ext TH.CApiFFI
      ,    foldMap (typeExtensions . functionParameterType)
                   foreignImportParameters
        <> typeExtensions (extractResultType foreignImportResultType)
      ]
    DPatternSynonym{} -> mconcat [
        ext TH.PatternSynonyms
      ]
    DPragma{} -> mconcat [
      ]
  where
    ext :: TH.Extension -> Set TH.Extension
    ext = Set.singleton

    isECString :: SExpr ctx -> Bool
    isECString = \case
      (ECString _) -> True
      _otherExpr   -> False

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
