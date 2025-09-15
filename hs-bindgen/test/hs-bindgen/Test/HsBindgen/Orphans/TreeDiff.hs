{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.HsBindgen.Orphans.TreeDiff () where

import Data.Foldable (toList)
import Data.TreeDiff.Class (ToExpr (..))
import Data.TreeDiff.Expr qualified as Expr
import Data.TreeDiff.OMap qualified as OMap
import Data.Vec.Lazy (Vec)
import Data.Vec.Lazy qualified as Vec
import Foreign.C
import System.FilePath qualified as FilePath

import C.Char qualified as CExpr
import C.Type qualified as CExpr

import Clang.Enum.Simple
import Clang.HighLevel.Documentation qualified as C
import Clang.HighLevel.Types qualified as C
import Clang.Paths qualified as Paths

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type qualified as HsType
import HsBindgen.Backend.Hs.CallConv qualified as Hs
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.Macro qualified as Macro
import HsBindgen.Frontend.RootHeader qualified as RootHeader
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.NameHint

import DeBruijn (Add, Idx, S, Size, addToInt, idxToInt, sizeToInt)

{-------------------------------------------------------------------------------
  base
-------------------------------------------------------------------------------}

instance ToExpr CInt where
  toExpr = toExpr . (fromIntegral :: CInt -> Int)

{-------------------------------------------------------------------------------
  hs-bindgen
-------------------------------------------------------------------------------}

-- Avoid references to system-dependent file locations
instance ToExpr Paths.SourcePath where
  toExpr = toExpr . FilePath.takeBaseName . Paths.getSourcePath

instance ToExpr RootHeader.HashIncludeArg where
  toExpr = toExpr . RootHeader.getHashIncludeArg

instance ToExpr C.AnonId
instance ToExpr C.CheckedMacro
instance ToExpr C.CheckedMacroExpr
instance ToExpr C.CheckedMacroType
instance ToExpr C.CXCommentParamPassDirection
instance ToExpr ref => ToExpr (C.CommentInlineContent ref)
instance ToExpr C.CXCommentInlineCommandRenderKind
instance ToExpr ref => ToExpr (C.CommentBlockContent ref)
instance ToExpr ref => ToExpr (C.Comment ref)
instance ToExpr C.Decl
instance ToExpr C.DeclInfo
instance ToExpr C.FieldInfo
instance ToExpr C.DeclKind
instance ToExpr C.DeclSpec
instance ToExpr C.Enum
instance ToExpr C.EnumConstant
instance ToExpr C.Function
instance ToExpr C.FunctionAttributes
instance ToExpr C.FunctionPurity
instance ToExpr C.HeaderInfo
instance ToExpr C.Name
instance ToExpr C.NameKind
instance ToExpr C.NameOrigin
instance ToExpr C.NamePair
instance ToExpr C.NewtypeNames
instance ToExpr C.PrimFloatType
instance ToExpr C.PrimIntType
instance ToExpr C.PrimSign
instance ToExpr C.PrimSignChar
instance ToExpr C.PrimType
instance ToExpr C.QualName
instance ToExpr C.RecordNames
instance ToExpr C.ResolvedExtBinding
instance ToExpr C.Reference
instance ToExpr C.Struct
instance ToExpr C.StructField
instance ToExpr C.TagKind
instance ToExpr C.TranslationUnit
instance ToExpr C.Type
instance ToExpr C.Typedef
instance ToExpr C.TypedefRef
instance ToExpr C.Union
instance ToExpr C.UnionField

instance ToExpr BindingSpec.TypeSpec
instance ToExpr BindingSpec.InstanceSpec
instance ToExpr BindingSpec.StrategySpec
instance ToExpr BindingSpec.ConstraintSpec
instance ToExpr a => ToExpr (BindingSpec.Omittable a)

instance ToExpr CExpr.CharValue

instance ToExpr C.IntegerLiteral
instance ToExpr C.FloatingLiteral
instance ToExpr C.CharLiteral
instance ToExpr C.StringLiteral

-- do not use record syntax, as it's very verbose
instance ToExpr C.SingleLoc where
  toExpr (C.SingleLoc path line column _offset) = toExpr $
    let filename = FilePath.takeFileName $ Paths.getSourcePath path
    in  filename ++ ":" ++ show line ++ ":" ++ show column

instance ToExpr Hs.HsModuleName
instance ToExpr Hs.HsIdentifier
instance ToExpr Hs.HsTypeClass
instance ToExpr Hs.ExtHsRef

instance ToExpr Hs.UserlandCapiWrapper
instance ToExpr Hs.CallConv
instance ToExpr Hs.ImportStyle

instance ToExpr Hs.Comment where

instance ToExpr Hs.ListType
instance ToExpr Hs.HeaderLevel
instance ToExpr Hs.CommentBlockContent
instance ToExpr Hs.CommentInlineContent
instance ToExpr Hs.CommentMeta


instance ToExpr CExpr.IntegralType where
instance ToExpr CExpr.CharLikeType where
instance ToExpr CExpr.IntLikeType where
instance ToExpr CExpr.Sign where
instance ToExpr CExpr.FloatingType where

instance ToExpr HsType.HsType
instance ToExpr HsType.HsPrimType

instance ToExpr Hs.EmptyData
instance ToExpr Hs.Field
instance ToExpr Hs.ForeignImportDecl
instance ToExpr SHs.Safety
instance ToExpr Hs.FunctionParameter
instance ToExpr a => ToExpr (HsType.ResultType a)
instance ToExpr Hs.Newtype
instance ToExpr Hs.PatSyn
instance ToExpr Hs.StorableInstance
instance ToExpr t => ToExpr (Hs.Strategy t)
instance ToExpr Hs.VarDecl
instance ToExpr Hs.UnionGetter
instance ToExpr Hs.UnionSetter
instance ToExpr Hs.DefineInstance
instance ToExpr Hs.DeriveInstance

instance ToExpr a => ToExpr (Origin.Decl a)
instance ToExpr Origin.EmptyData
instance ToExpr Origin.Field
instance ToExpr Origin.Newtype
instance ToExpr Origin.Struct
instance ToExpr Origin.PatSyn
instance ToExpr Origin.ForeignImport

instance ToExpr (Hs.PeekByteOff ctx)
instance ToExpr (Hs.PhiType ctx)
instance ToExpr (Hs.PokeByteOff ctx)
instance ToExpr (Hs.Struct n)
instance ToExpr (Hs.TauType ctx)
instance ToExpr (Hs.VarDeclRHS ctx)
instance ToExpr (t ctx) => ToExpr (Hs.Seq t ctx)

instance ToExpr Hs.Decl where
  toExpr = \case
    Hs.DeclData struct ->
      Expr.App "DeclData" [toExpr struct]
    Hs.DeclEmpty name ->
      Expr.App "DeclEmpty" [toExpr name]
    Hs.DeclNewtype nt ->
      Expr.App "DeclNewtype" [toExpr nt]
    Hs.DeclPatSyn patSyn ->
      Expr.App "DeclPatSyn" [toExpr patSyn]
    Hs.DeclDefineInstance inst ->
      Expr.App "DeclInstance" [toExpr inst]
    Hs.DeclDeriveInstance di ->
      Expr.App "DeclNewtypeInstance" [toExpr di]
    Hs.DeclForeignImport foreignImport ->
      Expr.App "DeclForeignImport" [toExpr foreignImport]
    Hs.DeclVar v ->
      Expr.App "DeclVar" [toExpr v]
    Hs.DeclUnionGetter ug ->
      Expr.App "DeclUnionGetter" [toExpr ug]
    Hs.DeclUnionSetter us ->
      Expr.App "DeclUnionSetter" [toExpr us]
    Hs.DeclSimple _d ->
      Expr.App "DeclSimple" [] -- TODO: no ToExpr SDecl

instance ToExpr a => ToExpr (Vec n a) where
  toExpr = Expr.Lst . map toExpr . Vec.toList

instance Hs.SingNamespace ns => ToExpr (Hs.HsName ns) where
  toExpr name = Expr.App "HsName" [
      toExpr ('@' : show (Hs.namespaceOf (Hs.singNamespace @ns)))
    , toExpr (Hs.getHsName name)
    ]

instance ToExpr Hs.InstanceDecl where
  toExpr = \case
    Hs.InstanceStorable struct inst ->
      Expr.App "InstanceStorable" [toExpr struct, toExpr inst]
    Hs.InstanceHasFLAM struct fty i ->
      Expr.App "InstanceHasFLAM" [toExpr struct, toExpr fty, toExpr i]
    Hs.InstanceCEnum struct typ vMap isSeq ->
      Expr.App "InstanceCEnum" [toExpr struct, toExpr typ, toExpr vMap, toExpr isSeq]
    Hs.InstanceSequentialCEnum struct minV maxV ->
      Expr.App "InstanceSequentialCEnum" [toExpr struct, toExpr minV, toExpr maxV]
    Hs.InstanceCEnumShow struct ->
      Expr.App "InstanceCEnumShow" [toExpr struct]
    Hs.InstanceCEnumRead struct ->
      Expr.App "InstanceCEnumRead" [toExpr struct]

instance ToExpr (t (S ctx)) => ToExpr (Hs.Lambda t ctx) where
  toExpr (Hs.Lambda name body) = Expr.App "Lambda" [toExpr name, toExpr body]

instance (ToExpr (pure ctx), ToExpr (xs ctx)) => ToExpr (Hs.Ap pure xs ctx)

instance (forall ctx'. ToExpr (t ctx')) => ToExpr (Hs.ElimStruct t ctx) where
  toExpr (Hs.ElimStruct idx struct add t) =
    Expr.App "ElimStruct" [toExpr idx, toExpr struct, toExpr add, toExpr t]

instance ToExpr NameHint where
  toExpr (NameHint s) = Expr.App "NameHint" [toExpr s]

instance ToExpr (Hs.StructCon ctx) where
  toExpr (Hs.StructCon struct) = Expr.App "StructCon" [toExpr struct]

instance ToExpr Hs.SigmaType where
  toExpr Hs.ForallTy {..} = Expr.Rec "ForallTy" $ OMap.fromList [
      ("forallTyBinders", toExpr forallTyBinders)
    , ("forallTy", toExpr forallTy)
    ]

instance ToExpr Hs.VarDeclRHSAppHead where
  toExpr = \case
    Hs.InfixAppHead mfun ->
      Expr.App "InfixAppHead" [toExpr mfun]
    Hs.VarAppHead name ->
      Expr.App "VarAppHead" [toExpr name]

instance ToExpr (Hs.PredType ctx) where
  toExpr = \case
    Hs.DictTy cls args ->
      Expr.App "ClassTy" [toExpr cls, toExpr args]
    Hs.NomEqTy l r ->
      Expr.App "NomEqTy" [toExpr l, toExpr r]

instance ToExpr Hs.ATyCon where
  toExpr (Hs.ATyCon tc) =
    Expr.App "ATyCon" [toExpr tc]

instance ToExpr Hs.AClass where
  toExpr (Hs.AClass tc) =
    Expr.App "AClass" [toExpr tc]

{-------------------------------------------------------------------------------
  hs-bindgen-runtime
-------------------------------------------------------------------------------}

instance ToExpr (SimpleEnum hs)

{-------------------------------------------------------------------------------
  Macro expressions
-------------------------------------------------------------------------------}

instance ToExpr (Macro.MTerm Macro.Ps)
instance ToExpr (Macro.XVar Macro.Ps)

instance ToExpr (Macro.MExpr Macro.Ps) where
  toExpr = \case
    Macro.MTerm tm ->
      Expr.App "MTerm" [toExpr tm]
    Macro.MApp _xapp fun args ->
      Expr.App "MApp" [toExpr fun, toExpr (toList args)]

instance ToExpr ( Macro.MFun arity ) where
  toExpr f = Expr.App (show f) []

instance Show e => ToExpr ( Macro.Quant e ) where
  toExpr quantTy = toExpr $ show quantTy

instance ToExpr (Macro.ClassTyCon arity) where
  toExpr = \case
    Macro.NotTyCon        -> Expr.App "NotTyCon"        []
    Macro.LogicalTyCon    -> Expr.App "LogicalTyCon"    []
    Macro.RelEqTyCon      -> Expr.App "RelEqTyCon"      []
    Macro.RelOrdTyCon     -> Expr.App "RelOrdTyCon"     []
    Macro.PlusTyCon       -> Expr.App "PlusTyCon"       []
    Macro.MinusTyCon      -> Expr.App "MinusTyCon"      []
    Macro.AddTyCon        -> Expr.App "AddTyCon"        []
    Macro.SubTyCon        -> Expr.App "SubTyCon"        []
    Macro.MultTyCon       -> Expr.App "MultTyCon"       []
    Macro.DivTyCon        -> Expr.App "DivTyCon"        []
    Macro.RemTyCon        -> Expr.App "RemTyCon"        []
    Macro.ComplementTyCon -> Expr.App "ComplementTyCon" []
    Macro.BitwiseTyCon    -> Expr.App "BitwiseTyCon"    []
    Macro.ShiftTyCon      -> Expr.App "ShiftTyCon"      []

instance ToExpr (Macro.TyCon args resKi) where
  toExpr = \case
    Macro.GenerativeTyCon tc  -> Expr.App "GenerativeTyCon" [toExpr tc]
    Macro.FamilyTyCon     fam -> Expr.App "FamilyTyCon"     [toExpr fam]

instance ToExpr (Macro.GenerativeTyCon args resKi) where
  toExpr = \case
    Macro.DataTyCon  dc  -> Expr.App "DataTyCon"  [toExpr dc]
    Macro.ClassTyCon cls -> Expr.App "ClassTyCon" [toExpr cls]

instance ToExpr (Macro.DataTyCon n) where
  toExpr = \case
    Macro.TupleTyCon n            -> Expr.App "TupleTyCon"     [toExpr n]
    Macro.VoidTyCon               -> Expr.App "VoidTyCon"      []
    Macro.PtrTyCon                -> Expr.App "PtrTyCon"       []
    Macro.CharLitTyCon            -> Expr.App "CharLitTyCon"   []
    Macro.IntLikeTyCon            -> Expr.App "IntLikeTyCon"   []
    Macro.FloatLikeTyCon          -> Expr.App "FloatLikeTyCon" []
    Macro.PrimIntInfoTyCon   info -> Expr.App "IntLikeTyCon"   [toExpr info]
    Macro.PrimFloatInfoTyCon info -> Expr.App "FloatLikeTyCon" [toExpr info]

instance ToExpr Macro.IntegralType -- Note: different from CExpr.IntegralType

instance ToExpr (Macro.FamilyTyCon n) where
  toExpr = \case
    Macro.PlusResTyCon       -> Expr.App "PlusResTyCon"       []
    Macro.MinusResTyCon      -> Expr.App "MinusResTyCon"      []
    Macro.AddResTyCon        -> Expr.App "AddResTyCon"        []
    Macro.SubResTyCon        -> Expr.App "SubResTyCon"        []
    Macro.MultResTyCon       -> Expr.App "MultResTyCon"       []
    Macro.DivResTyCon        -> Expr.App "DivResTyCon"        []
    Macro.RemResTyCon        -> Expr.App "RemResTyCon"        []
    Macro.ComplementResTyCon -> Expr.App "ComplementResTyCon" []
    Macro.BitsResTyCon       -> Expr.App "BitsResTyCon"       []
    Macro.ShiftResTyCon      -> Expr.App "ShiftResTyCon"      []

{-------------------------------------------------------------------------------
  DeBruijn
-------------------------------------------------------------------------------}

instance ToExpr (Add n m p) where
  toExpr add = Expr.App "Add" [toExpr (addToInt add)]

instance ToExpr (Idx j) where
  toExpr idx = Expr.App "Idx" [toExpr (idxToInt idx)]

instance ToExpr (Size ctx) where
  toExpr size = Expr.App "Size" [toExpr (sizeToInt size)]

