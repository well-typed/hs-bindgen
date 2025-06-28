-- | Construct the final (external) form of the AST
module HsBindgen.Frontend.AST.Finalize (finalize) where

import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.AST.External qualified as Ext
import HsBindgen.Frontend.AST.Internal qualified as Int
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.HandleTypedefs.IsPass qualified as Int
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

class Finalize (a :: Pass -> Star) where
  type Finalized a :: Star
  finalize :: a Final -> Finalized a

-- The final phase in the frontend
type Final = MangleNames

{-------------------------------------------------------------------------------
  Instances

  NOTE: We're intentionally not using RecordDotSyntax here, so that we consider
  each field, all annotations, etc. Fields that we don't want to include in
  the external AST we explicitly ignore.
-------------------------------------------------------------------------------}

instance Finalize Int.TranslationUnit where
  type Finalized Int.TranslationUnit = Ext.TranslationUnit

  finalize unit = Ext.TranslationUnit{
        unitDecls = map finalize unitDecls
      , unitDeps  = IncludeGraph.toSortedList unitIncludeGraph
      }
    where
      Int.TranslationUnit{
          unitDecls
        , unitIncludeGraph
        , unitAnn = _useDeclGraph
        } = unit

instance Finalize Int.Decl where
  type Finalized Int.Decl = Ext.Decl

  finalize decl = Ext.Decl{
        declInfo = finalize declInfo
      , declKind = finalize declKind
      , declSpec = declAnn
      }
    where
      Int.Decl {
          declInfo
        , declKind
        , declAnn
        } = decl

instance Finalize Int.DeclInfo where
  type Finalized Int.DeclInfo = Ext.DeclInfo

  finalize info = Ext.DeclInfo{
        declLoc
      , declId
      , declOrigin
      , declAliases
      , declHeader
      }
    where
      Int.DeclInfo{
          declLoc
        , declId
        , declOrigin
        , declAliases
        , declHeader
        } = info

instance Finalize Int.DeclKind where
  type Finalized Int.DeclKind = Ext.DeclKind

  finalize (Int.DeclStruct struct)   = Ext.DeclStruct (finalize struct)
  finalize (Int.DeclStructOpaque)    = Ext.DeclStructOpaque
  finalize (Int.DeclUnion union)     = Ext.DeclUnion (finalize union)
  finalize (Int.DeclUnionOpaque)     = Ext.DeclUnionOpaque
  finalize (Int.DeclEnum enum)       = Ext.DeclEnum (finalize enum)
  finalize (Int.DeclEnumOpaque)      = Ext.DeclEnumOpaque
  finalize (Int.DeclTypedef typedef) = Ext.DeclTypedef (finalize typedef)
  finalize (Int.DeclMacro macro)     = Ext.DeclMacro (finalize macro)
  finalize (Int.DeclFunction func)   = Ext.DeclFunction (finalize func)
  finalize (Int.DeclExtern ty)       = Ext.DeclExtern (finalize ty)
  finalize (Int.DeclConst ty)        = Ext.DeclConst (finalize ty)

instance Finalize Int.Struct where
  type Finalized Int.Struct = Ext.Struct

  finalize struct = Ext.Struct{
        structNames = structAnn
      , structSizeof
      , structAlignment
      , structFields = map finalize regularFields
      , structFlam = finalize <$> mFlam
      }
    where
      Int.Struct {
          structSizeof
        , structAlignment
        , structFields = allFields
        , structAnn
        } = struct

      (regularFields, mFlam) = partitionFields allFields

instance Finalize Int.StructField where
  type Finalized Int.StructField = Ext.StructField

  finalize field = Ext.StructField{
        structFieldLoc
      , structFieldName
      , structFieldType = finalize structFieldType
      , structFieldOffset
      , structFieldWidth
      }
    where
      Int.StructField {
          structFieldLoc
        , structFieldName
        , structFieldType
        , structFieldOffset
        , structFieldWidth
        , structFieldAnn = NoAnn
        } = field

instance Finalize Int.Union where
  type Finalized Int.Union = Ext.Union

  finalize union = Ext.Union{
        unionNames = unionAnn
      , unionSizeof
      , unionAlignment
      , unionFields = map finalize unionFields
      }
    where
      Int.Union {
          unionSizeof
        , unionAlignment
        , unionFields
        , unionAnn
        } = union

instance Finalize Int.UnionField where
  type Finalized Int.UnionField = Ext.UnionField

  finalize field = Ext.UnionField{
        unionFieldLoc
      , unionFieldName
      , unionFieldType = finalize unionFieldType
      }
    where
      Int.UnionField {
          unionFieldLoc
        , unionFieldName
        , unionFieldType
        , unionFieldAnn = NoAnn
        } = field

instance Finalize Int.Enum where
  type Finalized Int.Enum = Ext.Enum

  finalize enum = Ext.Enum{
        enumNames = enumAnn
      , enumType = finalize enumType
      , enumSizeof
      , enumAlignment
      , enumConstants = map finalize enumConstants
      }
    where
      Int.Enum {
          enumType
        , enumSizeof
        , enumAlignment
        , enumConstants
        , enumAnn
        } = enum

instance Finalize Int.EnumConstant where
  type Finalized Int.EnumConstant = Ext.EnumConstant

  finalize constant = Ext.EnumConstant{
        enumConstantLoc
      , enumConstantName
      , enumConstantValue
      }
    where
      Int.EnumConstant {
          enumConstantLoc
        , enumConstantName
        , enumConstantValue
        } = constant

instance Finalize Int.Typedef where
  type Finalized Int.Typedef = Ext.Typedef

  finalize typedef = Ext.Typedef{
        typedefNames = typedefAnn
      , typedefType  = finalize typedefType
      }
    where
      Int.Typedef{
          typedefType
        , typedefAnn
        } = typedef

instance Finalize Int.Function where
  type Finalized Int.Function = Ext.Function

  finalize function = Ext.Function{
        functionArgs = map finalize functionArgs
      , functionRes  = finalize functionRes
      }
    where
      Int.Function {
          functionArgs
        , functionRes
        , functionAnn = NoAnn
        } = function

instance Finalize Int.CheckedMacro where
  type Finalized Int.CheckedMacro = Ext.CheckedMacro

  finalize (Int.MacroType typ)  = Ext.MacroType (finalize typ)
  finalize (Int.MacroExpr expr) = Ext.MacroExpr expr

instance Finalize Int.CheckedMacroType where
  type Finalized Int.CheckedMacroType = Ext.CheckedMacroType

  finalize checkedMacroType = Ext.CheckedMacroType{
        macroTypeNames = macroTypeAnn
      , macroType      = finalize macroType
      }
    where
      Int.CheckedMacroType{
          macroType
        , macroTypeAnn
        } = checkedMacroType

instance Finalize Int.Type where
  type Finalized Int.Type = Ext.Type

  finalize (Int.TypePrim prim)                = Ext.TypePrim prim
  finalize (Int.TypeStruct name origin)       = Ext.TypeStruct name origin
  finalize (Int.TypeUnion name origin)        = Ext.TypeUnion name origin
  finalize (Int.TypeEnum name origin)         = Ext.TypeEnum name origin
  finalize (Int.TypeTypedef ref)              = Ext.TypeTypedef (finalize ref)
  finalize (Int.TypePointer typ)              = Ext.TypePointer (finalize typ)
  finalize (Int.TypeFun args res)             = Ext.TypeFun (map finalize args) (finalize res)
  finalize (Int.TypeVoid)                     = Ext.TypeVoid
  finalize (Int.TypeConstArray n typ)         = Ext.TypeConstArray n (finalize typ)
  finalize (Int.TypeIncompleteArray typ)      = Ext.TypeIncompleteArray (finalize typ)
  finalize (Int.TypeExtBinding ext)           = Ext.TypeExtBinding ext
  finalize (Int.TypeMacroTypedef name origin) = Ext.TypeMacroTypedef name origin

instance Finalize Int.RenamedTypedefRef where
  type Finalized Int.RenamedTypedefRef = Ext.TypedefRef

  finalize (Int.TypedefRegular  nm   ) = Ext.TypedefRegular  nm
  finalize (Int.TypedefSquashed nm ty) = Ext.TypedefSquashed nm (finalize ty)

{-------------------------------------------------------------------------------
  Internal: FLAMs
-------------------------------------------------------------------------------}

partitionFields ::
     [Int.StructField Final]
  -> ([Int.StructField Final], Maybe (Int.StructField Final))
partitionFields = go []
  where
    go ::
         [Int.StructField Final]
      -> [Int.StructField Final]
      -> ([Int.StructField Final], Maybe (Int.StructField Final))
    go acc []     = (reverse acc, Nothing)
    go acc (f:fs) = case Int.structFieldType f of
                      Int.TypeIncompleteArray ty ->
                        let f' = f{Int.structFieldType = ty}
                        in (reverse acc ++ fs, Just f')
                      _otherwise->
                        go (f:acc) fs
