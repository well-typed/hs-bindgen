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
    DInst inst -> mconcat . concat $ [
        [ext TH.MultiParamTypeClasses | length inst.args >= 2]
      , [ext TH.TypeFamilies          | not (null inst.types)]
      , [globalExtensions inst.clss]
      , concat [
            globalExtensions c : map typeExtensions ts
          | (c, ts) <- inst.super
          ]
      , typeExtensions <$> inst.args
      , concat [
            globalExtensions t : typeExtensions r : map typeExtensions as
          | (t, as, r) <- inst.types
          ]
      , concat [
            [globalExtensions f, exprExtensions e]
          | (f, e) <- inst.decs
          ]
      ]
    DRecord record -> mconcat [
        recordExtensions record
      , nestedDeriving record.deriv
      ]
    DNewtype newtyp -> mconcat [
        nestedDeriving newtyp.deriv
      , typeExtensions newtyp.field.typ
      ]
    DEmptyData{} -> mconcat [
        ext TH.EmptyDataDecls
      ]
    DDerivingInstance DerivingInstance{..} -> mconcat [
        Set.fromList [
            TH.DerivingStrategies
          , TH.StandaloneDeriving
          ]
      , strategyExtensions derivingInstanceStrategy
      , typeExtensions derivingInstanceType
      ]
    DForeignImport ForeignImport{..} -> mconcat [
        -- Note: GHC doesn't require CApiFFI in TH: https://gitlab.haskell.org/ghc/ghc/-/issues/25774
        ext TH.CApiFFI
      ,    foldMap (typeExtensions . (.typ)) foreignImportParameters
        <> typeExtensions foreignImportResult.typ
      ]
    DBinding Binding{..} ->
         foldMap (typeExtensions . (.typ)) parameters
      <> typeExtensions (result.typ)
      <> exprExtensions body
    DPatternSynonym{} -> mconcat [
        ext TH.PatternSynonyms
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
recordExtensions record = foldMap fieldExtensions record.fields

fieldExtensions :: Field -> Set TH.Extension
fieldExtensions field = typeExtensions field.typ

globalExtensions :: Global -> Set TH.Extension
globalExtensions = \case
    HasCField_offset# -> Set.singleton TH.MagicHash
    HasCBitfield_bitOffset# -> Set.singleton TH.MagicHash
    HasCBitfield_bitWidth# -> Set.singleton TH.MagicHash
    NomEq_class -> Set.singleton TH.TypeOperators
    HasField_class -> Set.singleton TH.UndecidableInstances
    HasBaseForeignType_class -> Set.singleton TH.UndecidableInstances
    Prim_class -> Set.singleton TH.UnboxedTuples
    _ -> mempty

exprExtensions :: SExpr ctx -> Set TH.Extension
exprExtensions = \case
    EGlobal g -> globalExtensions g
    EBound{} -> mempty
    EFree{} -> mempty
    ECon{} -> mempty
    EIntegral{} -> mempty
    EUnboxedIntegral{} -> mempty
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
      : [ case alt of
            SAlt _con _add _hints body ->
              exprExtensions body
            SAltNoConstr _hints body ->
              exprExtensions body
            SAltUnboxedTuple _add _hints body ->
              Set.fromList [TH.UnboxedTuples, TH.MagicHash] <> exprExtensions body
        | alt <- alts
        ]
    ETup xs -> foldMap exprExtensions xs
    EUnboxedTup xs -> Set.fromList [TH.UnboxedTuples, TH.MagicHash]
                   <> foldMap exprExtensions xs
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
