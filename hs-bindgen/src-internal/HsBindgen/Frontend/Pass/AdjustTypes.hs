module HsBindgen.Frontend.Pass.AdjustTypes (
    adjustTypes
  ) where

import Numeric.Natural (Natural)

import HsBindgen.Frontend.AST.Coerce (CoercePass (coercePass))
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass (AdjustTypes,
                                                   AdjustedFrom (..))
import HsBindgen.Frontend.Pass.MangleNames.IsPass (MangleNames)
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass (CheckedMacro (MacroExpr, MacroType),
                                                       CheckedMacroExpr,
                                                       CheckedMacroType (..))

-- | Adjust function argument types
--
-- The C reference describes that function parameters of some specific types are
-- adjusted. See:
--
-- <https://en.cppreference.com/w/c/language/function_declaration.html#Explanation>
--
-- We apply some of these adjustments to reduce ambiguity about the
-- representation of function parameter types. These adjustments are applied
-- everywhere that a C type occurs in the AST, even recursively. The adjustments
-- we apply are:
--
-- > * any parameter of function type is adjusted to the corresponding pointer
-- >  type
--
-- > * any parameter of array type is adjusted to the corresponding pointer
-- >   type
--
adjustTypes ::
     C.TranslationUnit MangleNames
  -> C.TranslationUnit AdjustTypes
adjustTypes unit =
      let
        decls' = map processDecl unit.decls
        unit' =  C.TranslationUnit {
              decls        = decls'
            , includeGraph = unit.includeGraph
            , ann          = unit.ann
            }
      in
        unit'

{-------------------------------------------------------------------------------
  Decls
-------------------------------------------------------------------------------}

processDecl :: C.Decl MangleNames -> C.Decl AdjustTypes
processDecl decl =
    C.Decl {
        info = coercePass decl.info
      , ann  = decl.ann
      , kind = processDeclKind decl.kind
      }

processDeclKind :: C.DeclKind MangleNames -> C.DeclKind AdjustTypes
processDeclKind kind =
    case kind of
      C.DeclStruct struct                  -> C.DeclStruct           $ processStruct struct
      C.DeclUnion union                    -> C.DeclUnion            $ processUnion union
      C.DeclTypedef typedef                -> C.DeclTypedef          $ processTypedef typedef
      C.DeclEnum enum                      -> C.DeclEnum             $ processEnum enum
      C.DeclAnonEnumConstant anonEnumConst -> C.DeclAnonEnumConstant $ processAnonEnumConstant anonEnumConst
      C.DeclOpaque                         -> C.DeclOpaque
      C.DeclMacro macro                    -> C.DeclMacro            $ processMacro macro
      C.DeclFunction function              -> C.DeclFunction         $ processFunction function
      C.DeclGlobal global                  -> C.DeclGlobal           $ processGlobal global

processStruct :: C.Struct MangleNames -> C.Struct AdjustTypes
processStruct struct =
    C.Struct {
        fields    = map processStructField struct.fields
      , flam      = processStructField <$> struct.flam
      , sizeof    = struct.sizeof
      , alignment = struct.alignment
      , ann       = struct.ann
      }

processStructField :: C.StructField MangleNames -> C.StructField AdjustTypes
processStructField field =
    C.StructField {
        info   = coercePass field.info
      , typ    = processType field.typ
      , offset = field.offset
      , width  = field.width
      , ann    = field.ann
      }

processUnion :: C.Union MangleNames -> C.Union AdjustTypes
processUnion union =
    C.Union {
        sizeof    = union.sizeof
      , alignment = union.alignment
      , fields    = map processUnionField union.fields
      , ann       = union.ann
      }

processUnionField :: C.UnionField MangleNames -> C.UnionField AdjustTypes
processUnionField field =
    C.UnionField {
        info = coercePass field.info
      , typ  = processType field.typ
      , ann  = field.ann
      }

processTypedef :: C.Typedef MangleNames -> C.Typedef AdjustTypes
processTypedef typedef =
    C.Typedef {
        typ = processType typedef.typ
      , ann = typedef.ann
      }

processEnum :: C.Enum MangleNames -> C.Enum AdjustTypes
processEnum enum =
    C.Enum {
        typ       = processType enum.typ
      , sizeof    = enum.sizeof
      , alignment = enum.alignment
      , constants = map processEnumConstant enum.constants
      , ann       = enum.ann
      }

processEnumConstant :: C.EnumConstant MangleNames -> C.EnumConstant AdjustTypes
processEnumConstant enumConstant =
    C.EnumConstant {
        info  = coercePass enumConstant.info
      , value = enumConstant.value
      }

processAnonEnumConstant ::
     C.AnonEnumConstant MangleNames
  -> C.AnonEnumConstant AdjustTypes
processAnonEnumConstant anonEnumConstant =
    C.AnonEnumConstant {
        typ = anonEnumConstant.typ
      , constant = processEnumConstant anonEnumConstant.constant
      }

processMacro :: MacroBody MangleNames -> MacroBody AdjustTypes
processMacro macro =
    case macro of
      MacroType typ -> MacroType $ processMacroType typ
      MacroExpr expr -> MacroExpr $ processMacroExpr expr

processMacroType :: CheckedMacroType MangleNames -> CheckedMacroType AdjustTypes
processMacroType macroType =
    CheckedMacroType {
        typ = processType macroType.typ
      , ann = macroType.ann
      }

processMacroExpr :: CheckedMacroExpr MangleNames -> CheckedMacroExpr AdjustTypes
processMacroExpr macroExpr =
    -- NOTE: currently macro expressions don't support function/array type
    -- parameters, if they do in the future, then we might have to recurse into
    -- the type of the macro expression?
    coercePass macroExpr

processFunction :: C.Function MangleNames -> C.Function AdjustTypes
processFunction function =
    C.Function {
        args  = map processFunctionArg function.args
      , res   = processType function.res
      , attrs = function.attrs
      , ann   = function.ann
      }

processFunctionArg :: C.FunctionArg MangleNames -> C.FunctionArg AdjustTypes
processFunctionArg functionArg =
    C.FunctionArg {
        name = functionArg.name
      , argTyp = processTypeFunArg functionArg.argTyp
      }

processGlobal :: C.Global MangleNames -> C.Global AdjustTypes
processGlobal global = C.Global{
      typ = processType global.typ
    , ann = global.ann
    }

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

processType ::
     C.Type MangleNames
  -> C.Type AdjustTypes
processType = \case
    C.TypePrim primTy ->
      C.TypePrim primTy
    C.TypeComplex primTy ->
      C.TypeComplex primTy
    C.TypeRef name ->
      C.TypeRef name
    C.TypeEnum ref ->
      C.TypeEnum $ C.Ref {
          name       = ref.name
        , underlying = processType ref.underlying
        }
    C.TypeMacro ref ->
      C.TypeMacro $ C.Ref {
          name       = ref.name
        , underlying = processType ref.underlying
        }
    C.TypeTypedef ref ->
      C.TypeTypedef $ C.Ref {
          name       = ref.name
        , underlying = processType ref.underlying
        }
    C.TypePointers n ty ->
      C.TypePointers n      $ processType ty
    C.TypeConstArray n ty ->
      C.TypeConstArray n    $ processType ty
    C.TypeIncompleteArray ty ->
      C.TypeIncompleteArray $ processType ty
    C.TypeFun args res ->
      C.TypeFun (map processTypeFunArg args) (processType res)
    C.TypeVoid ->
      C.TypeVoid
    C.TypeBlock ty ->
      C.TypeBlock $ processType ty
    C.TypeQual qual ty ->
      C.TypeQual qual $ processType ty
    C.TypeExtBinding ref ->
      C.TypeExtBinding $ C.Ref {
          name       = ref.name
        , underlying = processType ref.underlying
        }

processTypeFunArg :: C.TypeFunArg MangleNames -> C.TypeFunArg AdjustTypes
processTypeFunArg arg =
    C.TypeFunArgF {
        typ = typ'
      , ann = ann'
      }
  where
    (typ', ann') = adjustFunArg $ processType arg.typ

-- | This is where the actual adjustments take place.
--
-- Function arguments of function type are changed to *pointer* to function
-- type. For example, the former is adjusted to the latter:
--
-- > int foo (char f (float))
-- > int foo (char (*f)(float))
--
-- Function arguments of array type are changed to pointer to the array element
-- type. For example, the former is adjusted to the latter:
--
-- > int foo (char xs[])
-- > int foo (char * xs)
--
-- The original type before adjustment is recorded in an an @Ann@otation.
--
adjustFunArg :: C.Type AdjustTypes -> (C.Type AdjustTypes, AdjustedFrom AdjustTypes)
adjustFunArg ty
  | Just cls <- classifyCanonicalTypeArray ty
  , let elemTy = getArrayElementType cls
  , let constQual
          | C.isErasedTypeConstQualified ty
          , not (C.isErasedTypeConstQualified elemTy)
          = C.TypeQual C.QualConst
          | otherwise
          = id
  = (C.TypePointers 1 $ constQual elemTy, AdjustedFromArray ty)
  | C.isCanonicalTypeFunction ty
  = (C.TypePointers 1 ty, AdjustedFromFunction ty)
  | otherwise
  = (ty, NotAdjusted)

-- | An array of known size or unknown size
data ArrayClassification p =
    -- | Array of known size
    ConstantArrayClassification
      Natural     -- ^ Array size
      (C.Type p)  -- ^ Array element type

    -- | Array of unkown size
  | IncompleteArrayClassification
      (C.Type p)  -- ^ Array element type

getArrayElementType :: ArrayClassification p -> C.Type p
getArrayElementType (ConstantArrayClassification _ ty) = ty
getArrayElementType (IncompleteArrayClassification ty) = ty

-- | Is the canonical type an array type?
--
-- If so, is it an array of known size or unknown size? And what is the /full
-- type/ of the array elements?
classifyCanonicalTypeArray :: C.Type p -> Maybe (ArrayClassification p)
classifyCanonicalTypeArray ty =
    -- We do not use getCanonicalType here, because we do not want to
    -- canonicalize the array /element/ type.
    case ty of
      C.TypePrim _pt          -> Nothing
      C.TypeRef _declId       -> Nothing
      C.TypeEnum _ref         -> Nothing
      C.TypeMacro ref         -> classifyCanonicalTypeArray ref.underlying
      C.TypeTypedef ref       -> classifyCanonicalTypeArray ref.underlying
      C.TypePointers _n _t    -> Nothing
      C.TypeConstArray n t    -> Just (ConstantArrayClassification n t)
      C.TypeFun _args _res    -> Nothing
      C.TypeVoid              -> Nothing
      C.TypeIncompleteArray t -> Just (IncompleteArrayClassification t)
      C.TypeBlock _t          -> Nothing
      C.TypeQual _q t         -> classifyCanonicalTypeArray t
      C.TypeExtBinding ref    -> classifyCanonicalTypeArray ref.underlying
      C.TypeComplex _pt       -> Nothing
