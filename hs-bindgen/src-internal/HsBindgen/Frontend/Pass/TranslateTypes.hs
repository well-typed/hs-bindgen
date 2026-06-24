module HsBindgen.Frontend.Pass.TranslateTypes (
    translateTypes
  ) where

import HsBindgen.Frontend.Pass.AdjustTypes.IsPass
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.IR.Translation

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

translateTypes ::
     C.TranslationUnit l AdjustTypes
  -> C.TranslationUnit l TranslateTypes
translateTypes unit = C.TranslationUnit{
      decls        = map processDecl unit.decls
    , includeGraph = unit.includeGraph
    , meta         = unit.meta
    }

{-------------------------------------------------------------------------------
  Decls
-------------------------------------------------------------------------------}

processDecl :: C.Decl l AdjustTypes -> C.Decl l TranslateTypes
processDecl decl = C.Decl{
      info = coercePass decl.info
    , kind = processDeclKind decl.kind
    , ann  = decl.ann
    }

processDeclKind :: C.DeclKind l AdjustTypes -> C.DeclKind l TranslateTypes
processDeclKind = \case
    C.DeclStruct               struct   -> C.DeclStruct               $ processStruct           struct
    C.DeclUnion                union    -> C.DeclUnion                $ processUnion            union
    C.DeclTypedef              typedef  -> C.DeclTypedef              $ processTypedef          typedef
    C.DeclEnum                 enum     -> C.DeclEnum                 $ processEnum             enum
    C.DeclUntaggedEnumConstant cnst     -> C.DeclUntaggedEnumConstant $ processUntaggedEnumConstant cnst
    C.DeclOpaque               mSize    -> C.DeclOpaque mSize
    C.DeclMacro                macro    -> C.DeclMacro                $ processMacro            macro
    C.DeclFunction             function -> C.DeclFunction             $ processFunction         function
    C.DeclGlobal               global   -> C.DeclGlobal               $ processGlobal           global

processStruct :: C.Struct AdjustTypes -> C.Struct TranslateTypes
processStruct struct = C.Struct{
      sizeof    = struct.sizeof
    , alignment = struct.alignment
    , fields    = map processField struct.fields
    , flam      = C.mapFlamField processRegularField struct.flam
    , ann       = struct.ann
    }

processUnion :: C.Union AdjustTypes -> C.Union TranslateTypes
processUnion union = C.Union{
      sizeof    = union.sizeof
    , alignment = union.alignment
    , fields    = map processField union.fields
    , ann       = union.ann
    }

processField :: C.Field AdjustTypes -> C.Field TranslateTypes
processField = \case
    C.FieldRegular  field -> C.FieldRegular  $ processRegularField field
    C.FieldImplicit field -> C.FieldImplicit $ processImplicitField field

processRegularField :: C.RegularField AdjustTypes -> C.RegularField TranslateTypes
processRegularField field = C.RegularField{
      info   = coercePass field.info
    , typ    = processType field.typ
    , offset = field.offset
    , width  = field.width
    , ann    = field.ann
    }

processImplicitField :: C.ImplicitField AdjustTypes -> C.ImplicitField TranslateTypes
processImplicitField field = C.ImplicitField{
      info     = coercePass field.info
    , typRef   = processAnonRef field.typRef
    , offset   = field.offset
    , indirect = map processIndirectField field.indirect
    , ann      = field.ann
    }

processIndirectField :: C.IndirectField AdjustTypes -> C.IndirectField TranslateTypes
processIndirectField field = C.IndirectField{
      info   = coercePass field.info
    , typ    = processType field.typ
    , offset = field.offset
    , width  = field.width
    , path   = map processAnonRef field.path
    , ann    = coercePassAnn (Proxy @'("IndirectField", AdjustTypes, TranslateTypes)) field.ann
    }

processTypedef :: C.Typedef AdjustTypes -> C.Typedef TranslateTypes
processTypedef typedef = C.Typedef{
      typ = processType typedef.typ
    , ann = typedef.ann
    }

processEnum :: C.Enum AdjustTypes -> C.Enum TranslateTypes
processEnum enum = C.Enum{
      typ       = processType enum.typ
    , sizeof    = enum.sizeof
    , alignment = enum.alignment
    , constants = map coercePass enum.constants
    , ann       = enum.ann
    }

processUntaggedEnumConstant ::
     C.UntaggedEnumConstant AdjustTypes
  -> C.UntaggedEnumConstant TranslateTypes
processUntaggedEnumConstant = coercePass

processMacro :: MacroBody AdjustTypes l -> MacroBody TranslateTypes l
processMacro = \case
    MacroType  typ -> MacroType  $ coercePass typ
    MacroValue val -> MacroValue $ coercePass val

processFunction :: C.Function AdjustTypes -> C.Function TranslateTypes
processFunction fun = C.Function{
      args  = map processFunctionArg fun.args
    , res   = processType fun.res
    , attrs = fun.attrs
    , ann   = fun.ann
    }

processFunctionArg :: C.FunctionArg AdjustTypes -> C.FunctionArg TranslateTypes
processFunctionArg arg = C.FunctionArg{
      name = arg.name
    , typ  = processType arg.typ
    , ann  =
        coercePassAnn
          (Proxy @'("TypeFunArg", AdjustTypes, TranslateTypes))
          arg.ann
    }

processGlobal :: C.Global AdjustTypes -> C.Global TranslateTypes
processGlobal global = C.Global{
      typ = processType global.typ
    , ann = global.ann
    }

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

processType :: C.Type AdjustTypes -> TranslatedTypes TranslateTypes
processType typ = TranslatedTypes{
      c = coercePass typ
      -- TODO
    }

processAnonRef :: C.AnonRef AdjustTypes -> TranslatedAnonRef TranslateTypes
processAnonRef typ = TranslatedAnonRef{
      c = coercePass typ
      -- TODO
    }
