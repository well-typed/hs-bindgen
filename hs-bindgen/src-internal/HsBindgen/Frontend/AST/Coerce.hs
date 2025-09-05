module HsBindgen.Frontend.AST.Coerce (CoercePass(..)) where

import Prelude hiding (Enum)

import Data.Bifunctor (bimap)

import Clang.HighLevel.Documentation qualified as C

import HsBindgen.Frontend.AST.Internal
import HsBindgen.Frontend.Pass

{-------------------------------------------------------------------------------
  Coercing between passes
-------------------------------------------------------------------------------}

class CoercePass a p p' where
  coercePass :: a p -> a p'

instance (
      CoercePass Decl p p'
    , Ann "TranslationUnit" p ~ Ann "TranslationUnit" p'
    ) => CoercePass TranslationUnit p p' where
  coercePass TranslationUnit{..} = TranslationUnit{
        unitDecls = map coercePass unitDecls
      , unitIncludeGraph
      , unitAnn
      }

instance (
      Id p ~ Id p'
    ) => CoercePass Reference p p' where
  coercePass (ById t) = ById t

instance (
      Id p ~ Id p'
    ) => CoercePass C.Comment (Reference p) (Reference p') where
  coercePass comment = fmap coercePass comment

instance (
      CoercePass C.Comment (Reference p) (Reference p')
    ) => CoercePass Comment p p' where
  coercePass (Comment c) =
    Comment (coercePass c)

instance (
      CoercePass DeclInfo p p'
    , CoercePass DeclKind p p'
    , Ann "Decl" p ~ Ann "Decl" p'
    ) => CoercePass Decl p p' where
  coercePass Decl{..} = Decl{
        declInfo = coercePass declInfo
      , declKind = coercePass declKind
      , declAnn
      }

instance (
      Id p ~ Id p'
    , CoercePass Comment p p'
    ) => CoercePass DeclInfo p p' where
  coercePass info = DeclInfo{ declComment = fmap coercePass declComment
                            , ..
                            }
    where
      DeclInfo{declLoc, declId, declAliases, declHeader, declComment} = info

instance (
      CoercePass Comment p p'
    , FieldName p ~ FieldName p'
    ) => CoercePass FieldInfo p p' where
  coercePass info = FieldInfo{ fieldComment = fmap coercePass fieldComment
                             , ..
                             }
    where
      FieldInfo{fieldLoc, fieldName, fieldComment} = info

instance (
      CoercePass Struct   p p'
    , CoercePass Enum     p p'
    , CoercePass Union    p p'
    , CoercePass Typedef  p p'
    , CoercePass Function p p'
    , CoercePass Type     p p'
    , MacroBody p ~ MacroBody p'
    ) => CoercePass DeclKind p p' where
  coercePass = \case
    DeclStruct struct     -> DeclStruct (coercePass struct)
    DeclStructOpaque      -> DeclStructOpaque
    DeclUnion union       -> DeclUnion (coercePass union)
    DeclUnionOpaque       -> DeclUnionOpaque
    DeclTypedef typedef   -> DeclTypedef (coercePass typedef)
    DeclEnum enum         -> DeclEnum (coercePass enum)
    DeclEnumOpaque        -> DeclEnumOpaque
    DeclMacro macro       -> DeclMacro macro
    DeclFunction function -> DeclFunction (coercePass function)
    DeclGlobal ty         -> DeclGlobal (coercePass ty)

instance (
      CoercePass StructField p p'
    , Ann "Struct" p ~ Ann "Struct" p'
    ) => CoercePass Struct p p' where
  coercePass Struct{..} = Struct {
        structSizeof
      , structAlignment
      , structFields = map coercePass structFields
      , structAnn
      }

instance (
      CoercePass Type p p'
    , CoercePass Comment p p'
    , FieldName p ~ FieldName p'
    , Ann "StructField" p ~ Ann "StructField" p'
    ) => CoercePass StructField p p' where
  coercePass StructField{..} = StructField {
        structFieldInfo = coercePass structFieldInfo
      , structFieldType = coercePass structFieldType
      , structFieldOffset
      , structFieldWidth
      , structFieldAnn
      }

instance (
      CoercePass UnionField p p'
    , Ann "Union" p ~ Ann "Union" p'
    ) => CoercePass Union p p' where
  coercePass Union{..} = Union {
        unionSizeof
      , unionAlignment
      , unionFields = map coercePass unionFields
      , unionAnn
      }

instance (
      CoercePass Type p p'
    , FieldName p ~ FieldName p'
    , CoercePass Comment p p'
    , Ann "UnionField" p ~ Ann "UnionField" p'
    ) => CoercePass UnionField p p' where
  coercePass UnionField{..} = UnionField {
        unionFieldInfo = coercePass unionFieldInfo
      , unionFieldType = coercePass unionFieldType
      , unionFieldAnn
      }

instance (
      CoercePass Type p p'
    , Ann "Typedef" p ~ Ann "Typedef" p'
    ) => CoercePass Typedef p p' where
  coercePass Typedef{..} = Typedef {
        typedefType = coercePass typedefType
      , typedefAnn
      }

instance (
      CoercePass Type p p'
    , CoercePass EnumConstant p p'
    , Ann "Enum" p ~ Ann "Enum" p'
    ) => CoercePass Enum p p' where
  coercePass Enum{..} = Enum {
        enumType = coercePass enumType
      , enumSizeof
      , enumAlignment
      , enumConstants = map coercePass enumConstants
      , enumAnn
      }

instance (
      FieldName p ~ FieldName p'
    , CoercePass C.Comment (Reference p) (Reference p')
    ) => CoercePass EnumConstant p p' where
  coercePass EnumConstant{..} = EnumConstant {
        enumConstantInfo = coercePass enumConstantInfo
      , enumConstantValue
      }

instance (
      CoercePass Type p p'
    , ArgumentName p ~ ArgumentName p'
    , Ann "Function" p ~ Ann "Function" p'
    ) => CoercePass Function p p' where
  coercePass Function{..} = Function {
        functionArgs = map (bimap id coercePass) functionArgs
      , functionRes  = coercePass functionRes
      , functionAttrs
      , functionAnn
      }

instance (
      CoercePass CheckedMacroType p p'
    ) => CoercePass CheckedMacro p p' where
  coercePass (MacroType typ)  = MacroType (coercePass typ)
  coercePass (MacroExpr expr) = MacroExpr expr

instance (
      CoercePass Type p p'
    , Ann "CheckedMacroType" p ~ Ann "CheckedMacroType" p'
    ) => CoercePass CheckedMacroType p p' where
  coercePass CheckedMacroType{..} = CheckedMacroType {
        macroType = coercePass macroType
      , macroTypeAnn
      }

instance (
      Id p ~ Id p'
    , ArgumentName p ~ ArgumentName p'
    , TypedefRef p ~ TypedefRef p'
    , ExtBinding p ~ ExtBinding p'
    ) => CoercePass Type p p' where
  coercePass (TypePrim prim)           = TypePrim prim
  coercePass (TypeStruct uid)          = TypeStruct uid
  coercePass (TypeUnion uid)           = TypeUnion uid
  coercePass (TypeEnum uid)            = TypeEnum uid
  coercePass (TypeTypedef typedef)     = TypeTypedef typedef
  coercePass (TypeMacroTypedef uid)    = TypeMacroTypedef uid
  coercePass (TypePointer typ)         = TypePointer (coercePass typ)
  coercePass (TypeFun args res)        = TypeFun (map coercePass args) (coercePass res)
  coercePass  TypeVoid                 = TypeVoid
  coercePass (TypeConstArray n typ)    = TypeConstArray n (coercePass typ)
  coercePass (TypeIncompleteArray typ) = TypeIncompleteArray (coercePass typ)
  coercePass (TypeExtBinding ext)      = TypeExtBinding ext
  coercePass (TypeBlock typ)           = TypeBlock (coercePass typ)
  coercePass (TypeConst typ)           = TypeConst (coercePass typ)
