module HsBindgen.Frontend.AST.Coerce (
    CoercePass(..)
  , CoercePassId(..)
  , CoercePassHaskellId(..)
  , CoercePassMacroBody(..)
  ) where

import Prelude hiding (Enum)

import Clang.HighLevel.Documentation qualified as CDoc

import HsBindgen.Frontend.AST.Internal
import HsBindgen.Frontend.Pass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Type families
-------------------------------------------------------------------------------}

class CoercePassId (p :: Pass) (p' :: Pass) where
  coercePassId :: Proxy '(p, p') -> Id p -> Id p'

  default coercePassId ::
       (Id p ~ Id p')
    => Proxy '(p, p') -> Id p -> Id p'
  coercePassId _ = id

class CoercePassHaskellId (p :: Pass) (p' :: Pass) where
  coercePassHaskellId :: Proxy '(p, p') -> HaskellId p -> HaskellId p'

  default coercePassHaskellId ::
       (HaskellId p ~ HaskellId p')
    => Proxy '(p, p') -> HaskellId p -> HaskellId p'
  coercePassHaskellId _ = id

class CoercePassMacroBody (p :: Pass) (p' :: Pass) where
  coercePassMacroBody :: Proxy '(p, p') -> MacroBody p -> MacroBody p'

  default coercePassMacroBody ::
       (MacroBody p ~ MacroBody p')
    => Proxy '(p, p') -> MacroBody p -> MacroBody p'
  coercePassMacroBody _ = id

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
      CoercePassHaskellId p p'
    ) => CoercePass CommentRef p p' where
  coercePass (CommentRef c hs) =
      CommentRef c (coercePassHaskellId (Proxy @'(p, p')) <$> hs)

instance (
      CoercePassHaskellId p p'
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
    , CoercePassMacroBody p p'
    ) => CoercePass DeclKind p p' where
  coercePass = \case
      DeclStruct   x -> DeclStruct   $ coercePass x
      DeclUnion    x -> DeclUnion    $ coercePass x
      DeclTypedef  x -> DeclTypedef  $ coercePass x
      DeclEnum     x -> DeclEnum     $ coercePass x
      DeclFunction x -> DeclFunction $ coercePass x
      DeclGlobal   x -> DeclGlobal   $ coercePass x
      DeclMacro    x -> DeclMacro    $ coercePassMacroBody (Proxy @'(p, p')) x
      DeclOpaque     -> DeclOpaque

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
    , ExtBinding p ~ ExtBinding p'
    ) => CoercePass Type p p' where
  coercePass = \case
      TypePrim prim           -> TypePrim prim
      TypeRef uid             -> TypeRef (goId uid)
      TypeTypedef uid uTy     -> TypeTypedef (goId uid) (coercePass uTy)
      TypePointers n typ      -> TypePointers n (coercePass typ)
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
