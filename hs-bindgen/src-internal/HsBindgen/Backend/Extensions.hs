{-# LANGUAGE MagicHash #-}

module HsBindgen.Backend.Extensions (
    requiredExtensions,
) where

import Data.Set qualified as Set
import Language.Haskell.TH qualified as TH

import HsBindgen.Backend.Hs.AST (Strategy (..))
import HsBindgen.Backend.SHs.AST
import HsBindgen.Config.Prelims (FieldNamingStrategy (..))
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst

-- | Which GHC language extensions this declarations needs.
requiredExtensions :: FieldNamingStrategy -> SDecl -> Set TH.Extension
requiredExtensions fieldNaming = \case
    DTypSyn typSyn -> mconcat [
        typeExtensions typSyn.typ
      ]
    DInst inst -> mconcat . concat $ [
        [ext TH.MultiParamTypeClasses | length inst.args >= 2]
      , [ext TH.TypeFamilies          | not (null inst.types)]
      , [typeClassExtensions inst.clss]
      , map typeExtensions inst.args
      , map typeExtensions inst.super
      , concat [ map typeExtensions tyVars ++ [typeExtensions tySyn]
               | (_, tyVars, tySyn) <- inst.types ]
      , map (exprExtensions . snd) inst.decs
      ]
    DRecord record -> mconcat [
        recordExtensions record
      , nestedDeriving record.deriv
      , enableRecordDotExtensions fieldNaming
      , Set.singleton TH.DeriveGeneric
      ]
    DNewtype newtyp -> mconcat [
        nestedDeriving newtyp.deriv
      , typeExtensions newtyp.field.typ
      , enableRecordDotExtensions fieldNaming
      , Set.singleton TH.DeriveGeneric
      ]
    DEmptyData{} -> mconcat [
        ext TH.EmptyDataDecls
      ]
    DDerivingInstance deriv -> mconcat [
        Set.fromList [
            TH.DerivingStrategies
          , TH.StandaloneDeriving
          ]
      , strategyExtensions deriv.strategy
      , typeClassExtensions deriv.cls
      ]
    DForeignImport foreignImport -> mconcat [
        -- Note: GHC doesn't require CApiFFI in TH: https://gitlab.haskell.org/ghc/ghc/-/issues/25774
        ext TH.CApiFFI
      , foldMap (typeExtensions . (.typ)) foreignImport.parameters
      , typeExtensions foreignImport.result.typ
      ]
    DBinding binding -> mconcat [
        foldMap (typeExtensions . (.typ)) binding.parameters
      , typeExtensions (binding.result.typ)
      , exprExtensions binding.body
      ]
    DPatternSynonym{} -> mconcat [
        ext TH.PatternSynonyms
      ]
  where
    ext :: TH.Extension -> Set TH.Extension
    ext = Set.singleton

-- | Extensions for deriving clauses that are part of the datatype declaration
nestedDeriving :: [(Strategy ClosedType, [Inst.TypeClass])] -> Set TH.Extension
nestedDeriving deriv =
       Set.singleton TH.DerivingStrategies
    <> mconcat [
          strategyExtensions s <> foldMap typeClassExtensions gs
        | (s, gs) <- deriv
        ]

recordExtensions :: Record -> Set TH.Extension
recordExtensions record = foldMap fieldExtensions record.fields

-- | Extensions required when using enable record dot flag.
enableRecordDotExtensions :: FieldNamingStrategy -> Set TH.Extension
enableRecordDotExtensions = \case
    PrefixedFieldNames -> mempty
    EnableRecordDot    -> Set.singleton TH.DuplicateRecordFields

fieldExtensions :: Field -> Set TH.Extension
fieldExtensions field = typeExtensions field.typ

typeClassExtensions :: Inst.TypeClass -> Set TH.Extension
typeClassExtensions = \case
    Inst.HasCField    -> Set.singleton TH.MagicHash
    Inst.HasCBitfield -> Set.singleton TH.MagicHash
    Inst.HasField     -> Set.singleton TH.UndecidableInstances
    Inst.HasFFIType   -> Set.singleton TH.UndecidableInstances
    Inst.Prim         -> Set.fromList [TH.MagicHash, TH.UnboxedTuples]
    _ -> mempty

exprExtensions :: SExpr ctx -> Set TH.Extension
exprExtensions = \case
    EGlobal{} -> mempty
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
    EBoxedOpenTup{} -> mempty
    EBoxedClosedTup xs -> foldMap exprExtensions xs
    EUnboxedTup xs -> Set.fromList [TH.UnboxedTuples, TH.MagicHash]
                   <> foldMap exprExtensions xs
    EList xs -> foldMap exprExtensions xs
    ETypeApp f t -> Set.singleton TH.TypeApplications <> exprExtensions f <> typeExtensions t

-- Note: We don't recognise whether we need RankNTypes.
-- We probably don't generate such types
typeExtensions :: SType ctx -> Set TH.Extension
typeExtensions = \case
    TGlobal{}         -> Set.empty
    TCon _            -> Set.empty
    TFree _           -> Set.singleton TH.FlexibleContexts -- include like in 'predicateExtensions'
    TFun a b          -> typeExtensions a <> typeExtensions b
    TLit _            -> Set.singleton TH.DataKinds
    TStrLit _         -> Set.singleton TH.DataKinds
    TExt{}            -> Set.empty
    TBound _          -> Set.empty
    TApp f b          -> typeExtensions f <> typeExtensions b
    TBoxedOpenTup{}   -> Set.empty
    TEq               -> Set.singleton TH.TypeOperators
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
