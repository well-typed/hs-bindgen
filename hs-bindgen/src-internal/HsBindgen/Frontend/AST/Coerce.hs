module HsBindgen.Frontend.AST.Coerce (
    CoercePass(..)
  , CoercePassId(..)
  , CoercePassMacroBody(..)
  ) where

import Prelude hiding (Enum)

import Clang.HighLevel.Documentation qualified as CDoc

import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
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
      CoercePass C.Decl p p'
    , Ann "TranslationUnit" p ~ Ann "TranslationUnit" p'
    ) => CoercePass C.TranslationUnit p p' where
  coercePass unit = C.TranslationUnit{
        decls        = map coercePass unit.decls
      , includeGraph = unit.includeGraph
      , ann          = unit.ann
      }

instance (
      CoercePassId p p'
    ) => CoercePass C.CommentRef p p' where
  coercePass (C.CommentRef c hs) =
      C.CommentRef c (coercePassId (Proxy @'(p, p')) <$> hs)

instance (
      CoercePassId p p'
    ) => CoercePass CDoc.Comment (C.CommentRef p) (C.CommentRef p') where
  coercePass comment = fmap coercePass comment

instance (
      CoercePass CDoc.Comment (C.CommentRef p) (C.CommentRef p')
    ) => CoercePass C.Comment p p' where
  coercePass (C.Comment c) = C.Comment (coercePass c)

instance (
      CoercePass C.DeclInfo p p'
    , CoercePass C.DeclKind p p'
    , Ann "Decl" p ~ Ann "Decl" p'
    ) => CoercePass C.Decl p p' where
  coercePass C.Decl{..} = C.Decl{
        declInfo = coercePass declInfo
      , declKind = coercePass declKind
      , declAnn
      }

instance (
      CoercePassId p p'
    , CoercePass C.Comment p p'
    ) => CoercePass C.DeclInfo p p' where
  coercePass info = C.DeclInfo{
        declId      = coercePassId (Proxy @'(p, p')) declId
      , declComment = fmap coercePass declComment
      , ..
      }
    where
      C.DeclInfo{..} = info

instance (
      CoercePass C.Comment p p'
    , ScopedName p ~ ScopedName p'
    ) => CoercePass C.FieldInfo p p' where
  coercePass info = C.FieldInfo{
        fieldComment = fmap coercePass fieldComment
      , ..
      }
    where
      C.FieldInfo{fieldLoc, fieldName, fieldComment} = info

instance (
      CoercePass C.Struct   p p'
    , CoercePass C.Enum     p p'
    , CoercePass C.Union    p p'
    , CoercePass C.Typedef  p p'
    , CoercePass C.Function p p'
    , CoercePass C.Type     p p'
    , CoercePassMacroBody p p'
    ) => CoercePass C.DeclKind p p' where
  coercePass = \case
      C.DeclStruct   x -> C.DeclStruct   $ coercePass x
      C.DeclUnion    x -> C.DeclUnion    $ coercePass x
      C.DeclTypedef  x -> C.DeclTypedef  $ coercePass x
      C.DeclEnum     x -> C.DeclEnum     $ coercePass x
      C.DeclFunction x -> C.DeclFunction $ coercePass x
      C.DeclGlobal   x -> C.DeclGlobal   $ coercePass x
      C.DeclMacro    x -> C.DeclMacro    $ coercePassMacroBody (Proxy @'(p, p')) x
      C.DeclOpaque     -> C.DeclOpaque

instance (
      CoercePass C.StructField p p'
    , Ann "Struct" p ~ Ann "Struct" p'
    ) => CoercePass C.Struct p p' where
  coercePass C.Struct{..} = C.Struct{
        structSizeof
      , structAlignment
      , structFields = map coercePass structFields
      , structFlam = fmap coercePass structFlam
      , structAnn
      }

instance (
      CoercePass C.Type p p'
    , CoercePass C.Comment p p'
    , ScopedName p ~ ScopedName p'
    , Ann "StructField" p ~ Ann "StructField" p'
    ) => CoercePass C.StructField p p' where
  coercePass C.StructField{..} = C.StructField{
        structFieldInfo = coercePass structFieldInfo
      , structFieldType = coercePass structFieldType
      , structFieldOffset
      , structFieldWidth
      , structFieldAnn
      }

instance (
      CoercePass C.UnionField p p'
    , Ann "Union" p ~ Ann "Union" p'
    ) => CoercePass C.Union p p' where
  coercePass C.Union{..} = C.Union{
        unionSizeof
      , unionAlignment
      , unionFields = map coercePass unionFields
      , unionAnn
      }

instance (
      CoercePass C.Type p p'
    , ScopedName p ~ ScopedName p'
    , CoercePass C.Comment p p'
    , Ann "UnionField" p ~ Ann "UnionField" p'
    ) => CoercePass C.UnionField p p' where
  coercePass C.UnionField{..} = C.UnionField{
        unionFieldInfo = coercePass unionFieldInfo
      , unionFieldType = coercePass unionFieldType
      , unionFieldAnn
      }

instance (
      CoercePass C.Type p p'
    , Ann "Typedef" p ~ Ann "Typedef" p'
    ) => CoercePass C.Typedef p p' where
  coercePass C.Typedef{..} = C.Typedef{
        typedefType = coercePass typedefType
      , typedefAnn
      }

instance (
      CoercePass C.Type p p'
    , CoercePass C.EnumConstant p p'
    , Ann "Enum" p ~ Ann "Enum" p'
    ) => CoercePass C.Enum p p' where
  coercePass C.Enum{..} = C.Enum{
        enumType = coercePass enumType
      , enumSizeof
      , enumAlignment
      , enumConstants = map coercePass enumConstants
      , enumAnn
      }

instance (
      ScopedName p ~ ScopedName p'
    , CoercePass CDoc.Comment (C.CommentRef p) (C.CommentRef p')
    ) => CoercePass C.EnumConstant p p' where
  coercePass C.EnumConstant{..} = C.EnumConstant{
        enumConstantInfo = coercePass enumConstantInfo
      , enumConstantValue
      }

instance (
      CoercePass C.Type p p'
    , ScopedName p ~ ScopedName p'
    , Ann "Function" p ~ Ann "Function" p'
    ) => CoercePass C.Function p p' where
  coercePass C.Function{..} = C.Function{
        functionArgs = map (bimap id coercePass) functionArgs
      , functionRes  = coercePass functionRes
      , functionAttrs
      , functionAnn
      }

instance (
      CoercePassId p p'
    , ScopedName p ~ ScopedName p'
    , ExtBinding p ~ ExtBinding p'
    ) => CoercePass C.Type p p' where
  coercePass = \case
      C.TypePrim prim           -> C.TypePrim prim
      C.TypeRef uid             -> C.TypeRef (goId uid)
      C.TypeTypedef ref         -> C.TypeTypedef (coercePass ref)
      C.TypePointers n typ      -> C.TypePointers n (coercePass typ)
      C.TypeFun args res        -> C.TypeFun (map coercePass args) (coercePass res)
      C.TypeVoid                -> C.TypeVoid
      C.TypeConstArray n typ    -> C.TypeConstArray n (coercePass typ)
      C.TypeIncompleteArray typ -> C.TypeIncompleteArray (coercePass typ)
      C.TypeExtBinding ext      -> C.TypeExtBinding ext
      C.TypeBlock typ           -> C.TypeBlock (coercePass typ)
      C.TypeQual qual typ       -> C.TypeQual qual (coercePass typ)
      C.TypeComplex prim        -> C.TypeComplex prim
    where
      goId :: Id p -> Id p'
      goId = coercePassId (Proxy @'(p, p'))

instance (
      CoercePassId p p'
    , CoercePass C.Type p p'
    ) => CoercePass C.TypedefRef p p' where
  coercePass C.TypedefRef{ref, underlying} = C.TypedefRef{
        ref        = coercePassId (Proxy @'(p, p')) ref
      , underlying = coercePass underlying
      }
