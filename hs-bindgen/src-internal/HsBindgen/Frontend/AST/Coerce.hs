module HsBindgen.Frontend.AST.Coerce (
    CoercePass(..)
  , CoercePassId(..)
  , CoercePassMacroId(..)
  , CoercePassMacroBody(..)
  ) where

import Prelude hiding (Enum)

import Clang.HighLevel.Documentation qualified as CDoc

import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Result (ParseClassification (..),
                                             ParseResult (..),
                                             ParseSuccess (..))
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

class CoercePassMacroId (p :: Pass) (p' :: Pass) where
  coercePassMacroId :: Proxy '(p, p') -> MacroId p -> MacroId p'

  default coercePassMacroId ::
       (MacroId p ~ MacroId p')
    => Proxy '(p, p') -> MacroId p -> MacroId p'
  coercePassMacroId _ = id

{-------------------------------------------------------------------------------
  Coercing between passes
-------------------------------------------------------------------------------}

class CoercePass a p p' where
  coercePass :: a p -> a p'

instance (
      CoercePassId p p'
    , CoercePass C.Decl p p'
    , Ann "TranslationUnit" p ~ Ann "TranslationUnit" p'
    ) => CoercePass ParseResult p p' where
  coercePass pr = ParseResult{
        id             = coercePassId (Proxy @'(p, p')) pr.id
      , loc            = pr.loc
      , classification = coercePass pr.classification
      }

instance (
      CoercePass C.Decl p p'
    , Ann "TranslationUnit" p ~ Ann "TranslationUnit" p'
    ) => CoercePass ParseClassification p p' where
  coercePass = \case
    ParseResultSuccess s      -> ParseResultSuccess (coercePass s)
    ParseResultNotAttempted x -> ParseResultNotAttempted x
    ParseResultFailure x      -> ParseResultFailure x

instance (
      CoercePass C.Decl p p'
    , Ann "TranslationUnit" p ~ Ann "TranslationUnit" p'
    ) => CoercePass ParseSuccess p p' where
  coercePass ps = ParseSuccess{
        decl = coercePass ps.decl
      , delayedParseMsgs = ps.delayedParseMsgs
      }

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
  coercePass decl = C.Decl{
        info = coercePass decl.info
      , kind = coercePass decl.kind
      , ann  = decl.ann
      }

instance (
      CoercePassId p p'
    , CoercePass C.Comment p p'
    ) => CoercePass C.DeclInfo p p' where
  coercePass info = C.DeclInfo{
        id           = coercePassId (Proxy @'(p, p')) info.id
      , comment      = fmap coercePass info.comment
      , loc          = info.loc
      , headerInfo   = info.headerInfo
      , availability = info.availability
      }

instance (
      CoercePass C.Comment p p'
    , ScopedName p ~ ScopedName p'
    ) => CoercePass C.FieldInfo p p' where
  coercePass info = C.FieldInfo{
        comment = fmap coercePass info.comment
      , name    = info.name
      , loc     = info.loc
      }

instance (
       CoercePass C.Struct   p p'
     , CoercePass C.Enum     p p'
     , CoercePass C.Union    p p'
     , CoercePass C.Typedef  p p'
     , CoercePass C.Function p p'
     , CoercePass C.Type     p p'
     , CoercePass C.AnonEnumConstant p p'
     , CoercePassMacroBody p p'
     ) => CoercePass C.DeclKind p p' where
  coercePass = \case
      C.DeclStruct           x -> C.DeclStruct           $ coercePass x
      C.DeclUnion            x -> C.DeclUnion            $ coercePass x
      C.DeclTypedef          x -> C.DeclTypedef          $ coercePass x
      C.DeclEnum             x -> C.DeclEnum             $ coercePass x
      C.DeclAnonEnumConstant x -> C.DeclAnonEnumConstant $ coercePass x
      C.DeclFunction         x -> C.DeclFunction         $ coercePass x
      C.DeclGlobal           x -> C.DeclGlobal           $ coercePass x
      C.DeclMacro            x -> C.DeclMacro            $ coercePassMacroBody (Proxy @'(p, p')) x
      C.DeclOpaque             -> C.DeclOpaque

instance (
      CoercePass C.StructField p p'
    , Ann "Struct" p ~ Ann "Struct" p'
    ) => CoercePass C.Struct p p' where
  coercePass struct = C.Struct{
        fields    = coercePass <$> struct.fields
      , flam      = coercePass <$> struct.flam
      , sizeof    = struct.sizeof
      , alignment = struct.alignment
      , ann       = struct.ann
      }

instance (
      CoercePass C.Type p p'
    , CoercePass C.Comment p p'
    , ScopedName p ~ ScopedName p'
    , Ann "StructField" p ~ Ann "StructField" p'
    ) => CoercePass C.StructField p p' where
  coercePass field = C.StructField{
        info   = coercePass field.info
      , typ    = coercePass field.typ
      , offset = field.offset
      , width  = field.width
      , ann    = field.ann
      }

instance (
      CoercePass C.UnionField p p'
    , Ann "Union" p ~ Ann "Union" p'
    ) => CoercePass C.Union p p' where
  coercePass union = C.Union{
        fields    = coercePass <$> union.fields
      , sizeof    = union.sizeof
      , alignment = union.alignment
      , ann       = union.ann
      }

instance (
      CoercePass C.Type p p'
    , ScopedName p ~ ScopedName p'
    , CoercePass C.Comment p p'
    , Ann "UnionField" p ~ Ann "UnionField" p'
    ) => CoercePass C.UnionField p p' where
  coercePass field = C.UnionField{
        info = coercePass field.info
      , typ  = coercePass field.typ
      , ann  = field.ann
      }

instance (
      CoercePass C.Type p p'
    , Ann "Typedef" p ~ Ann "Typedef" p'
    ) => CoercePass C.Typedef p p' where
  coercePass typedef = C.Typedef{
        typ = coercePass typedef.typ
      , ann = typedef.ann
      }

instance (
       CoercePass C.Type p p'
     , CoercePass C.EnumConstant p p'
     , Ann "Enum" p ~ Ann "Enum" p'
     ) => CoercePass C.Enum p p' where
  coercePass enum = C.Enum{
        typ       = coercePass enum.typ
      , constants = coercePass <$> enum.constants
      , sizeof    = enum.sizeof
      , alignment = enum.alignment
      , ann       = enum.ann
      }

instance (
       CoercePass C.Type p p'
     , CoercePass C.EnumConstant p p'
     , Ann "PatternSynonym" p ~ Ann "PatternSynonym" p'
     ) => CoercePass C.AnonEnumConstant p p' where
  coercePass patternSynonym = C.AnonEnumConstant{
        typ      = coercePass patternSynonym.typ
      , constant = coercePass patternSynonym.constant
      }

instance (
      ScopedName p ~ ScopedName p'
    , CoercePass CDoc.Comment (C.CommentRef p) (C.CommentRef p')
    ) => CoercePass C.EnumConstant p p' where
  coercePass constant = C.EnumConstant{
        info  = coercePass constant.info
      , value = constant.value
      }

instance (
      CoercePass C.Type p p'
    , ScopedName p ~ ScopedName p'
    , Ann "Function" p ~ Ann "Function" p'
    ) => CoercePass C.Function p p' where
  coercePass function = C.Function{
        args  = map (bimap id coercePass) function.args
      , res   = coercePass function.res
      , attrs = function.attrs
      , ann   = function.ann
      }

instance (
      CoercePassId p p'
    , CoercePassMacroId p p'
    , ScopedName p ~ ScopedName p'
    , ExtBinding p ~ ExtBinding p'
    ) => CoercePass C.Type p p' where
  coercePass = \case
      C.TypePrim prim           -> C.TypePrim prim
      C.TypeRef uid             -> C.TypeRef (goId uid)
      C.TypeMacro ref           -> C.TypeMacro (C.Ref (coercePassMacroId (Proxy @'(p, p')) ref.name) (coercePass ref.underlying))
      C.TypeTypedef ref         -> C.TypeTypedef (C.Ref (coercePassId (Proxy @'(p, p')) ref.name) (coercePass ref.underlying))
      C.TypePointers n typ      -> C.TypePointers n (coercePass typ)
      C.TypeFun args res        -> C.TypeFun (map coercePass args) (coercePass res)
      C.TypeVoid                -> C.TypeVoid
      C.TypeConstArray n typ    -> C.TypeConstArray n (coercePass typ)
      C.TypeIncompleteArray typ -> C.TypeIncompleteArray (coercePass typ)
      C.TypeExtBinding ref      -> C.TypeExtBinding (C.Ref ref.name (coercePass ref.underlying))
      C.TypeBlock typ           -> C.TypeBlock (coercePass typ)
      C.TypeQual qual typ       -> C.TypeQual qual (coercePass typ)
      C.TypeComplex prim        -> C.TypeComplex prim
    where
      goId :: Id p -> Id p'
      goId = coercePassId (Proxy @'(p, p'))
