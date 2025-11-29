{-# LANGUAGE MagicHash #-}
module HsBindgen.Backend.Extensions (
    requiredExtensions,
) where

import Data.Set qualified as Set
import Language.Haskell.TH qualified as TH

import HsBindgen.Backend.Hs.AST (Strategy (..))
import HsBindgen.Backend.SHs.AST
import HsBindgen.Imports

-- | Which GHC language extensions this declarations needs.
requiredExtensions :: SDecl -> Set TH.Extension
requiredExtensions = \case
    DVar Var {..} ->
         typeExtensions varType
      <> exprExtensions varExpr
    DInst x -> mconcat . concat $ [
        [ext TH.MultiParamTypeClasses | length (instanceArgs x) >= 2]
      , [ext TH.TypeFamilies          | not (null (instanceTypes x))]
      , [globalExtensions $ instanceClass x]
      , concat [ globalExtensions c : map typeExtensions ts
          | (c, ts) <- instanceSuperClasses x
          ]
      , typeExtensions <$> instanceArgs x
      , concat [
            globalExtensions t : typeExtensions r : map typeExtensions as
          | (t, as, r) <- instanceTypes x
          ]
      , concat [
            [globalExtensions f, exprExtensions e]
          | (f, e) <- instanceDecs x
          ]
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
        <> typeExtensions foreignImportResultType
      ]
    DFunction Function {..} ->
         foldMap (typeExtensions . functionParameterType) functionParameters
      <> typeExtensions functionResultType
      <> exprExtensions functionBody
    DPatternSynonym{} -> mconcat [
        ext TH.PatternSynonyms
      ]
    DPragma{} -> mconcat [
      ]
  where
    ext :: TH.Extension -> Set TH.Extension
    ext = Set.singleton

-- | Extensions for deriving clauses that are part of the datatype declaration
nestedDeriving :: [(Strategy ClosedType, [Global])] -> Set TH.Extension
nestedDeriving deriv =
       Set.singleton TH.DerivingStrategies
    <> mconcat [
          strategyExtensions s <> foldMap globalExtensions gs
        | (s, gs) <- deriv
        ]

recordExtensions :: Record -> Set TH.Extension
recordExtensions r = foldMap fieldExtensions (dataFields r)

fieldExtensions :: Field -> Set TH.Extension
fieldExtensions f = typeExtensions (fieldType f)

globalExtensions :: Global -> Set TH.Extension
globalExtensions = \case
    HasCField_offset# -> Set.singleton TH.MagicHash
    HasCBitfield_bitOffset# -> Set.singleton TH.MagicHash
    HasCBitfield_bitWidth# -> Set.singleton TH.MagicHash
    NomEq_class -> Set.singleton TH.TypeOperators
    HasField_class -> Set.singleton TH.UndecidableInstances
    _ -> mempty

exprExtensions :: SExpr ctx -> Set TH.Extension
exprExtensions = \case
    EGlobal g -> globalExtensions g
    EBound{} -> mempty
    EFree{} -> mempty
    ECon{} -> mempty
    EIntegral{} -> mempty
    EChar {} -> mempty
    EString {} -> mempty
    ECString {} -> Set.singleton TH.MagicHash
    EFloat{} -> mempty
    EDouble{} -> mempty
    EApp f x -> exprExtensions f <> exprExtensions x
    EInfix _op x y ->
      exprExtensions x <> exprExtensions y
    ELam _mPat body -> exprExtensions body
    EUnusedLam body -> exprExtensions body
    ECase x alts -> mconcat $
        exprExtensions x
      : [ exprExtensions body
        | SAlt _con _add _hints body <- alts
        ]
    ETup xs -> foldMap exprExtensions xs
    EList xs -> foldMap exprExtensions xs
    ETypeApp f t -> Set.singleton TH.TypeApplications <> exprExtensions f <> typeExtensions t

-- Note: We don't recognise whether we need RankNTypes.
-- We probably don't generate such types
typeExtensions :: SType ctx -> Set TH.Extension
typeExtensions = \case
    TGlobal g  -> globalExtensions g
    TCon _     -> Set.empty
    TFree _    -> Set.singleton TH.FlexibleContexts -- include like in 'predicateExtensions'
    TFun a b   -> typeExtensions a <> typeExtensions b
    TLit _     -> Set.singleton TH.DataKinds
    TStrLit _  -> Set.singleton TH.DataKinds
    TExt{}     -> Set.empty
    TBound _   -> Set.empty
    TApp f b   -> typeExtensions f <> typeExtensions b
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
