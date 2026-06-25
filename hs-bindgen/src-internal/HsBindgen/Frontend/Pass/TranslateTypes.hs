module HsBindgen.Frontend.Pass.TranslateTypes (
    translateTypes
  ) where

import HsBindgen.Frontend.Pass.AdjustTypes.IsPass
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass
import HsBindgen.Frontend.Pass.TranslateTypes.Translation qualified as Translation
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
    C.DeclStruct           struct   -> C.DeclStruct           $ processStruct           struct
    C.DeclUnion            union    -> C.DeclUnion            $ processUnion            union
    C.DeclTypedef          typedef  -> C.DeclTypedef          $ processTypedef          typedef
    C.DeclEnum             enum     -> C.DeclEnum             $ processEnum             enum
    C.DeclAnonEnumConstant cnst     -> C.DeclAnonEnumConstant $ processAnonEnumConstant cnst
    C.DeclOpaque           mSize    -> C.DeclOpaque mSize
    C.DeclMacro            macro    -> C.DeclMacro            $ processMacro            macro
    C.DeclFunction         function -> C.DeclFunction         $ processFunction         function
    C.DeclGlobal           global   -> C.DeclGlobal           $ processGlobal           global

processStruct :: C.Struct AdjustTypes -> C.Struct TranslateTypes
processStruct struct = C.Struct{
      sizeof    = struct.sizeof
    , alignment = struct.alignment
    , fields    = map processStructField struct.fields
    , flam      = C.mapFlamField processStructField struct.flam
    , ann       = struct.ann
    }

processStructField :: C.StructField AdjustTypes -> C.StructField TranslateTypes
processStructField field = C.StructField{
      info   = coercePass field.info
    , typ    = processType Translation.Top field.typ
    , offset = field.offset
    , width  = field.width
    , ann    = field.ann
    }

processUnion :: C.Union AdjustTypes -> C.Union TranslateTypes
processUnion union = C.Union{
      sizeof    = union.sizeof
    , alignment = union.alignment
    , fields    = map processUnionField union.fields
    , ann       = union.ann
    }

processUnionField :: C.UnionField AdjustTypes -> C.UnionField TranslateTypes
processUnionField field = C.UnionField{
      info = coercePass field.info
    , typ  = processType Translation.Top field.typ
    , ann  = field.ann
    }

processTypedef :: C.Typedef AdjustTypes -> C.Typedef TranslateTypes
processTypedef typedef = C.Typedef{
      typ = processType Translation.Top typedef.typ
    , ann = typedef.ann
    }

processEnum :: C.Enum AdjustTypes -> C.Enum TranslateTypes
processEnum enum = C.Enum{
      typ       = processType Translation.Top enum.typ
    , sizeof    = enum.sizeof
    , alignment = enum.alignment
    , constants = map coercePass enum.constants
    , ann       = enum.ann
    }

processAnonEnumConstant ::
     C.AnonEnumConstant AdjustTypes
  -> C.AnonEnumConstant TranslateTypes
processAnonEnumConstant = coercePass

processMacro :: MacroBody AdjustTypes l -> MacroBody TranslateTypes l
processMacro = \case
    MacroType  typ -> MacroType  $ coercePass typ
    MacroValue val -> MacroValue $ coercePass val

processFunction :: C.Function AdjustTypes -> C.Function TranslateTypes
processFunction fun = C.Function{
      args  = map processFunctionArg fun.args
    , res   = processType Translation.FunRes fun.res
    , attrs = fun.attrs
    , ann   = fun.ann
    }

processFunctionArg :: C.FunctionArg AdjustTypes -> C.FunctionArg TranslateTypes
processFunctionArg arg = C.FunctionArg{
      name = arg.name
    , typ  = TranslatedTypes{
          c  = coercePass arg.typ
        , hs = Translation.inContext Translation.FunArg arg
        }
    , ann  =
        coercePassAnn
          (Proxy @'("TypeFunArg", AdjustTypes, TranslateTypes))
          arg.ann
    }

processGlobal :: C.Global AdjustTypes -> C.Global TranslateTypes
processGlobal global = C.Global{
      typ = processType Translation.Top global.typ
    , ann = global.ann
    }

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

processType ::
     Translation.TypeContext
  -> C.Type AdjustTypes
  -> TranslatedTypes TranslateTypes
processType ctx typ = TranslatedTypes{
      c  = coercePass typ
    , hs = Translation.inContext ctx typ
    }
