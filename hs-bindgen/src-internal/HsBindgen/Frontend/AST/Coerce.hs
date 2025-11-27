module HsBindgen.Frontend.AST.Coerce (
    CoercePass(..)
  , CoercePassId(..)
  , CoercePassTypedefRef(..)
  ) where

import Prelude hiding (Enum)

import Clang.HighLevel.Documentation qualified as CDoc

import HsBindgen.Frontend.AST.Internal
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Type families
-------------------------------------------------------------------------------}

class CoercePassId (p :: Pass) (p' :: Pass) where
  coercePassId :: Proxy '(p, p') -> Id p -> Id p'

class CoercePassTypedefRef (p :: Pass) (p' :: Pass) where
  coercePassTypedefRef :: Proxy '(p, p') -> TypedefRef p -> TypedefRef p'

{-------------------------------------------------------------------------------
  Coercing between passes
-------------------------------------------------------------------------------}

class CoercePass a p p' where
  coercePass :: a p -> a p'

instance CoercePass DeclId p p' where
  coercePass = \case
      DeclIdNamed   named   -> DeclIdNamed   (coercePass named)
      DeclIdBuiltin builtin -> DeclIdBuiltin (coercePass builtin)

instance CoercePass NamedDeclId p p' where
  coercePass named = NamedDeclId{
        name   = named.name
      , origin = named.origin
      }

instance CoercePass BuiltinDeclId p p' where
  coercePass builtin = BuiltinDeclId{
        name = builtin.name
      }

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
      CoercePassId p p'
    ) => CoercePass CommentRef p p' where
  coercePass (ById t) = ById (coercePassId (Proxy @'(p, p')) t)

instance (
      CoercePassId p p'
    ) => CoercePass CDoc.Comment (CommentRef p) (CommentRef p') where
  coercePass comment = fmap coercePass comment

instance (
      CoercePass CDoc.Comment (CommentRef p) (CommentRef p')
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
      CoercePassId p p'
    , CoercePass Comment p p'
    ) => CoercePass DeclInfo p p' where
  coercePass info = DeclInfo{
        declId      = coercePassId (Proxy @'(p, p')) declId
      , declComment = fmap coercePass declComment
      , ..
      }
    where
      DeclInfo{..} = info

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
    DeclUnion union       -> DeclUnion (coercePass union)
    DeclTypedef typedef   -> DeclTypedef (coercePass typedef)
    DeclEnum enum         -> DeclEnum (coercePass enum)
    DeclOpaque cNameKind  -> DeclOpaque cNameKind
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
    , CoercePass CDoc.Comment (CommentRef p) (CommentRef p')
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
      CoercePassId p p'
    , ArgumentName p ~ ArgumentName p'
    , CoercePassTypedefRef p p'
    , ExtBinding p ~ ExtBinding p'
    ) => CoercePass Type p p' where
  coercePass = \case
      TypePrim prim           -> TypePrim prim
      TypeStruct uid          -> TypeStruct (goId uid)
      TypeUnion uid           -> TypeUnion (goId uid)
      TypeEnum uid            -> TypeEnum (goId uid)
      TypeTypedef typedef     -> TypeTypedef (goTypedefRef typedef)
      TypeMacroTypedef uid    -> TypeMacroTypedef (goId uid)
      TypePointer typ         -> TypePointer (coercePass typ)
      TypeFun args res        -> TypeFun (map coercePass args) (coercePass res)
      TypeVoid                -> TypeVoid
      TypeConstArray n typ    -> TypeConstArray n (coercePass typ)
      TypeIncompleteArray typ -> TypeIncompleteArray (coercePass typ)
      TypeExtBinding ext      -> TypeExtBinding ext
      TypeBlock typ           -> TypeBlock (coercePass typ)
      TypeConst typ           -> TypeConst (coercePass typ)
      TypeComplex prim        -> TypeComplex prim
    where
      goId :: Id p -> Id p'
      goId = coercePassId (Proxy @'(p, p'))

      goTypedefRef :: TypedefRef p -> TypedefRef p'
      goTypedefRef = coercePassTypedefRef (Proxy @'(p, p'))
