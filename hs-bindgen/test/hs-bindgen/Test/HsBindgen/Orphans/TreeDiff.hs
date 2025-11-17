{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.HsBindgen.Orphans.TreeDiff () where

import Data.Foldable (toList)
import Data.TreeDiff.Class (ToExpr (..), defaultExprViaShow)
import Data.TreeDiff.Expr qualified as Expr
import Data.Vec.Lazy (Vec)
import Data.Vec.Lazy qualified as Vec
import Foreign.C
import System.FilePath qualified as FilePath

import C.Char qualified as CExpr.Runtime
import C.Type qualified as CExpr.Runtime

import C.Expr.Syntax qualified as CExpr.DSL
import C.Expr.Typecheck.Expr qualified as CExpr.DSL

import Clang.Enum.Simple
import Clang.HighLevel.Documentation qualified as CDoc
import Clang.HighLevel.Types qualified as C
import Clang.Paths qualified as Paths

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type qualified as HsType
import HsBindgen.Backend.Hs.CallConv qualified as Hs
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.AST.External qualified as C
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
instance ToExpr CDoc.CXCommentParamPassDirection
instance ToExpr ref => ToExpr (CDoc.CommentInlineContent ref)
instance ToExpr CDoc.CXCommentInlineCommandRenderKind
instance ToExpr ref => ToExpr (CDoc.CommentBlockContent ref)
instance ToExpr ref => ToExpr (CDoc.Comment ref)
instance ToExpr C.CommentRef
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
instance ToExpr C.Struct
instance ToExpr C.StructField
instance ToExpr C.TagKind
instance ToExpr C.TranslationUnit
instance ToExpr C.Type
instance ToExpr C.TypeQualifier
instance ToExpr C.Typedef
instance ToExpr C.TypedefRef
instance ToExpr C.Union
instance ToExpr C.UnionField

instance ToExpr BindingSpec.CTypeSpec
instance ToExpr BindingSpec.HsTypeSpec
instance ToExpr BindingSpec.InstanceSpec
instance ToExpr BindingSpec.StrategySpec
instance ToExpr BindingSpec.ConstraintSpec
instance ToExpr a => ToExpr (BindingSpec.Omittable a)

instance ToExpr CExpr.Runtime.CharValue

instance ToExpr CExpr.DSL.IntegerLiteral
instance ToExpr CExpr.DSL.FloatingLiteral
instance ToExpr CExpr.DSL.CharLiteral
instance ToExpr CExpr.DSL.StringLiteral

-- do not use record syntax, as it's very verbose
instance ToExpr C.SingleLoc where
  toExpr (C.SingleLoc path line column _offset) = toExpr $
    let filename = FilePath.takeFileName $ Paths.getSourcePath path
    in  filename ++ ":" ++ show line ++ ":" ++ show column

instance ToExpr Hs.ExtRef
instance ToExpr Hs.Identifier
instance ToExpr Hs.ModuleName
instance ToExpr Hs.TypeClass

instance ToExpr Hs.UserlandCapiWrapper
instance ToExpr Hs.CallConv
instance ToExpr Hs.ImportStyle

instance ToExpr HsDoc.Comment where

instance ToExpr HsDoc.ListType
instance ToExpr HsDoc.HeaderLevel
instance ToExpr HsDoc.CommentBlockContent
instance ToExpr HsDoc.CommentInlineContent
instance ToExpr HsDoc.CommentMeta

instance ToExpr CExpr.Runtime.IntegralType where
instance ToExpr CExpr.Runtime.CharLikeType where
instance ToExpr CExpr.Runtime.IntLikeType where
instance ToExpr CExpr.Runtime.Sign where
instance ToExpr CExpr.Runtime.FloatingType where

instance ToExpr HsType.HsType
instance ToExpr HsType.HsPrimType

instance ToExpr Hs.EmptyData
instance ToExpr Hs.Field
instance ToExpr Hs.ForeignImportDecl
instance ToExpr SHs.Safety
instance ToExpr Hs.FunctionParameter

-- | Note: We can't write a proper instance here because 'ClosedExpr' has
-- existential types
--
instance ToExpr SHs.ClosedExpr where
  toExpr = defaultExprViaShow
instance ToExpr Hs.FunctionDecl where

instance ToExpr a => ToExpr (HsType.ResultType a)
instance ToExpr Hs.Newtype
instance ToExpr Hs.PatSyn
instance ToExpr t => ToExpr (Hs.Strategy t)
instance ToExpr Hs.MacroExpr
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

instance ToExpr (Hs.Struct n)
instance ToExpr (Hs.VarDeclRHS ctx)
instance ToExpr (t ctx) => ToExpr (Hs.Seq t ctx)

instance ToExpr Hs.ToFunPtrInstance
instance ToExpr Hs.FromFunPtrInstance

instance ToExpr Hs.StorableInstance
instance ToExpr (Hs.PeekCField ctx)
instance ToExpr (Hs.PokeCField ctx)

instance ToExpr Hs.HasCFieldInstance

instance ToExpr Hs.HasCBitfieldInstance

instance ToExpr Hs.HasFieldInstance
instance ToExpr Hs.HasFieldInstanceVia

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
    Hs.DeclFunction functionDecl ->
      Expr.App "DeclFunction" [toExpr functionDecl]
    Hs.DeclMacroExpr v ->
      Expr.App "DeclVar" [toExpr v]
    Hs.DeclUnionGetter ug ->
      Expr.App "DeclUnionGetter" [toExpr ug]
    Hs.DeclUnionSetter us ->
      Expr.App "DeclUnionSetter" [toExpr us]
    Hs.DeclSimple _d ->
      Expr.App "DeclSimple" [] -- TODO: no ToExpr SDecl

instance ToExpr a => ToExpr (Vec n a) where
  toExpr = Expr.Lst . map toExpr . Vec.toList

instance Hs.SingNamespace ns => ToExpr (Hs.Name ns) where
  toExpr name = Expr.App "Name" [
      toExpr ('@' : show (Hs.namespaceOf (Hs.singNamespace @ns)))
    , toExpr (Hs.getName name)
    ]

instance ToExpr Hs.InstanceDecl where
  toExpr = \case
    Hs.InstanceStorable struct inst ->
      Expr.App "InstanceStorable" [toExpr struct, toExpr inst]
    Hs.InstanceHasCField inst ->
      Expr.App "InstanceHasCField" [toExpr inst]
    Hs.InstanceHasCBitfield inst ->
      Expr.App "InstanceHasCBitfield" [toExpr inst]
    Hs.InstanceHasField inst ->
      Expr.App "InstanceHasField" [toExpr inst]
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
    Hs.InstanceToFunPtr inst ->
      Expr.App "InstanceToFunPtr" [toExpr inst]
    Hs.InstanceFromFunPtr inst ->
      Expr.App "InstanceFromFunPtr" [toExpr inst]

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

instance ToExpr Hs.VarDeclRHSAppHead where
  toExpr = \case
    Hs.InfixAppHead mfun ->
      Expr.App "InfixAppHead" [toExpr mfun]
    Hs.VarAppHead name ->
      Expr.App "VarAppHead" [toExpr name]

{-------------------------------------------------------------------------------
  hs-bindgen-runtime
-------------------------------------------------------------------------------}

instance ToExpr (SimpleEnum hs)

{-------------------------------------------------------------------------------
  Macro expressions
-------------------------------------------------------------------------------}

instance ToExpr CExpr.DSL.Name
instance ToExpr (CExpr.DSL.MTerm CExpr.DSL.Ps)
instance ToExpr (CExpr.DSL.XVar CExpr.DSL.Ps)

instance ToExpr (CExpr.DSL.MExpr CExpr.DSL.Ps) where
  toExpr = \case
    CExpr.DSL.MTerm tm ->
      Expr.App "MTerm" [toExpr tm]
    CExpr.DSL.MApp _xapp fun args ->
      Expr.App "MApp" [toExpr fun, toExpr (toList args)]

instance ToExpr ( CExpr.DSL.MFun arity ) where
  toExpr f = Expr.App (show f) []

instance Show e => ToExpr ( CExpr.DSL.Quant e ) where
  toExpr quantTy = toExpr $ show quantTy

instance ToExpr (CExpr.DSL.ClassTyCon arity) where
  toExpr = \case
    CExpr.DSL.NotTyCon        -> Expr.App "NotTyCon"        []
    CExpr.DSL.LogicalTyCon    -> Expr.App "LogicalTyCon"    []
    CExpr.DSL.RelEqTyCon      -> Expr.App "RelEqTyCon"      []
    CExpr.DSL.RelOrdTyCon     -> Expr.App "RelOrdTyCon"     []
    CExpr.DSL.PlusTyCon       -> Expr.App "PlusTyCon"       []
    CExpr.DSL.MinusTyCon      -> Expr.App "MinusTyCon"      []
    CExpr.DSL.AddTyCon        -> Expr.App "AddTyCon"        []
    CExpr.DSL.SubTyCon        -> Expr.App "SubTyCon"        []
    CExpr.DSL.MultTyCon       -> Expr.App "MultTyCon"       []
    CExpr.DSL.DivTyCon        -> Expr.App "DivTyCon"        []
    CExpr.DSL.RemTyCon        -> Expr.App "RemTyCon"        []
    CExpr.DSL.ComplementTyCon -> Expr.App "ComplementTyCon" []
    CExpr.DSL.BitwiseTyCon    -> Expr.App "BitwiseTyCon"    []
    CExpr.DSL.ShiftTyCon      -> Expr.App "ShiftTyCon"      []

instance ToExpr (CExpr.DSL.TyCon args resKi) where
  toExpr = \case
    CExpr.DSL.GenerativeTyCon tc  -> Expr.App "GenerativeTyCon" [toExpr tc]
    CExpr.DSL.FamilyTyCon     fam -> Expr.App "FamilyTyCon"     [toExpr fam]

instance ToExpr (CExpr.DSL.GenerativeTyCon args resKi) where
  toExpr = \case
    CExpr.DSL.DataTyCon  dc  -> Expr.App "DataTyCon"  [toExpr dc]
    CExpr.DSL.ClassTyCon cls -> Expr.App "ClassTyCon" [toExpr cls]

instance ToExpr (CExpr.DSL.DataTyCon n) where
  toExpr = \case
    CExpr.DSL.TupleTyCon n            -> Expr.App "TupleTyCon"     [toExpr n]
    CExpr.DSL.VoidTyCon               -> Expr.App "VoidTyCon"      []
    CExpr.DSL.PtrTyCon                -> Expr.App "PtrTyCon"       []
    CExpr.DSL.CharLitTyCon            -> Expr.App "CharLitTyCon"   []
    CExpr.DSL.IntLikeTyCon            -> Expr.App "IntLikeTyCon"   []
    CExpr.DSL.FloatLikeTyCon          -> Expr.App "FloatLikeTyCon" []
    CExpr.DSL.PrimIntInfoTyCon   info -> Expr.App "IntLikeTyCon"   [toExpr info]
    CExpr.DSL.PrimFloatInfoTyCon info -> Expr.App "FloatLikeTyCon" [toExpr info]

instance ToExpr CExpr.DSL.IntegralType -- Note: different from CExpr.IntegralType

instance ToExpr (CExpr.DSL.FamilyTyCon n) where
  toExpr = \case
    CExpr.DSL.PlusResTyCon       -> Expr.App "PlusResTyCon"       []
    CExpr.DSL.MinusResTyCon      -> Expr.App "MinusResTyCon"      []
    CExpr.DSL.AddResTyCon        -> Expr.App "AddResTyCon"        []
    CExpr.DSL.SubResTyCon        -> Expr.App "SubResTyCon"        []
    CExpr.DSL.MultResTyCon       -> Expr.App "MultResTyCon"       []
    CExpr.DSL.DivResTyCon        -> Expr.App "DivResTyCon"        []
    CExpr.DSL.RemResTyCon        -> Expr.App "RemResTyCon"        []
    CExpr.DSL.ComplementResTyCon -> Expr.App "ComplementResTyCon" []
    CExpr.DSL.BitsResTyCon       -> Expr.App "BitsResTyCon"       []
    CExpr.DSL.ShiftResTyCon      -> Expr.App "ShiftResTyCon"      []

{-------------------------------------------------------------------------------
  DeBruijn
-------------------------------------------------------------------------------}

instance ToExpr (Add n m p) where
  toExpr add = Expr.App "Add" [toExpr (addToInt add)]

instance ToExpr (Idx j) where
  toExpr idx = Expr.App "Idx" [toExpr (idxToInt idx)]

instance ToExpr (Size ctx) where
  toExpr size = Expr.App "Size" [toExpr (sizeToInt size)]

