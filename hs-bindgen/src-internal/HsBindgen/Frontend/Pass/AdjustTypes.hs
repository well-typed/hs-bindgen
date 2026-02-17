module HsBindgen.Frontend.Pass.AdjustTypes (
    adjustTypes
  ) where

import Control.Monad.State (MonadState, State, StateT (StateT), modify',
                            runState)
import Numeric.Natural (Natural)

import HsBindgen.Frontend.AST.Coerce (CoercePass (coercePass))
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass (IsPass (MacroBody, Msg))
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass (AdjustTypes,
                                                   AdjustedFrom (..))
import HsBindgen.Frontend.Pass.HandleMacros.IsPass (CheckedMacro (MacroExpr, MacroType),
                                                    CheckedMacroExpr,
                                                    CheckedMacroType (..))
import HsBindgen.Frontend.Pass.Select.IsPass (Select)
import HsBindgen.Imports (Identity (Identity))

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
     C.TranslationUnit Select
  -> (C.TranslationUnit AdjustTypes, [Msg AdjustTypes])
adjustTypes unit =
      let
        (decls', msgs) = runM $ mapM processDecl unit.decls
        unit' =  C.TranslationUnit {
              decls        = decls'
            , includeGraph = unit.includeGraph
            , ann          = unit.ann
            }
      in
        (unit', msgs)

{-------------------------------------------------------------------------------
  Monad
-------------------------------------------------------------------------------}

newtype M a = M (State [Msg AdjustTypes] a)
  deriving newtype (Functor, Applicative, Monad)

deriving newtype instance msg ~ Msg AdjustTypes
                       => MonadState [msg] M

runM :: M a -> (a, [Msg AdjustTypes])
runM (M f) =
    let (x, msgs) = runState f []
    in  (x, reverse msgs)

_emitMsg :: Msg AdjustTypes -> M ()
_emitMsg msg = modify' (msg :)

{-------------------------------------------------------------------------------
  Decls
-------------------------------------------------------------------------------}

processDecl ::
     C.Decl Select
  -> M (C.Decl AdjustTypes)
processDecl decl = do
    kind' <- processDeclKind decl.kind
    pure C.Decl {
        info = coercePass decl.info
      , ann = decl.ann
      , kind = kind'
      }

processDeclKind ::
     C.DeclKind Select
  -> M (C.DeclKind AdjustTypes)
processDeclKind kind =
    case kind of
      C.DeclStruct struct                  -> C.DeclStruct <$> processStruct struct
      C.DeclUnion union                    -> C.DeclUnion <$> processUnion union
      C.DeclTypedef typedef                -> C.DeclTypedef <$> processTypedef typedef
      C.DeclEnum enum                      -> C.DeclEnum <$> processEnum enum
      C.DeclAnonEnumConstant anonEnumConst -> C.DeclAnonEnumConstant <$> processAnonEnumConstant anonEnumConst
      C.DeclOpaque                         -> pure C.DeclOpaque
      C.DeclMacro macro                    -> C.DeclMacro <$> processMacro macro
      C.DeclFunction function              -> C.DeclFunction <$> processFunction function
      C.DeclGlobal global                  -> C.DeclGlobal <$> processGlobal global

processStruct ::
     C.Struct Select
  -> M (C.Struct AdjustTypes)
processStruct struct = do
    fields' <- mapM processStructField struct.fields
    flam' <- mapM processStructField struct.flam
    pure C.Struct {
        fields    = fields'
      , flam      = flam'
      , sizeof    = struct.sizeof
      , alignment = struct.alignment
      , ann       = struct.ann
      }

processStructField ::
     C.StructField Select
  -> M (C.StructField AdjustTypes)
processStructField field = do
    typ' <- processType field.typ
    pure C.StructField {
        info = coercePass field.info
      , typ  = typ'
      , offset = field.offset
      , width = field.width
      , ann = field.ann
      }

processUnion ::
     C.Union Select
  -> M (C.Union AdjustTypes)
processUnion union = do
    fields' <- mapM processUnionField union.fields
    pure C.Union {
        sizeof = union.sizeof
      , alignment = union.alignment
      , fields = fields'
      , ann = union.ann
      }

processUnionField ::
     C.UnionField Select
  -> M (C.UnionField AdjustTypes)
processUnionField field = do
    typ' <- processType field.typ
    pure C.UnionField {
        info = coercePass field.info
      , typ  = typ'
      , ann = field.ann
      }

processTypedef ::
     C.Typedef Select
  -> M (C.Typedef AdjustTypes)
processTypedef typedef = do
    typ' <- processType typedef.typ
    pure C.Typedef {
        typ = typ'
      , ann = typedef.ann
      }

processEnum ::
     C.Enum Select
  -> M (C.Enum AdjustTypes)
processEnum enum = do
    typ' <- processType enum.typ
    constants' <- mapM processEnumConstant enum.constants
    pure C.Enum {
        typ = typ'
      , sizeof = enum.sizeof
      , alignment = enum.alignment
      , constants = constants'
      , ann = enum.ann
      }

processEnumConstant ::
     C.EnumConstant Select
  -> M (C.EnumConstant AdjustTypes)
processEnumConstant enumConstant = do
    pure C.EnumConstant {
        info = coercePass enumConstant.info
      , value = enumConstant.value
      }

processAnonEnumConstant ::
     C.AnonEnumConstant Select
  -> M (C.AnonEnumConstant AdjustTypes)
processAnonEnumConstant anonEnumConstant = do
    constant' <- processEnumConstant anonEnumConstant.constant
    pure C.AnonEnumConstant {
        typ = anonEnumConstant.typ
      , constant = constant'
      }

processMacro ::
     MacroBody Select
  -> M (MacroBody AdjustTypes)
processMacro macro = do
    case macro of
      MacroType typ -> MacroType <$> processMacroType typ
      MacroExpr expr -> MacroExpr <$> processMacroExpr expr

processMacroType ::
     CheckedMacroType Select
  -> M (CheckedMacroType AdjustTypes)
processMacroType macroType = do
    typ' <- processType macroType.typ
    pure CheckedMacroType {
        typ = typ'
      , ann = macroType.ann
      }

processMacroExpr ::
     CheckedMacroExpr
  -> M CheckedMacroExpr
processMacroExpr macroExpr = do
    -- NOTE: currently macro expressions don't support function/array type
    -- parameters, if they do in the future, then we might have to recurse into
    -- the type of the macro expression?
    pure macroExpr

processFunction ::
     C.Function Select
  -> M (C.Function AdjustTypes)
processFunction function = do
    args' <- mapM processFunctionArg function.args
    res' <- processType function.res
    pure C.Function {
        args = args'
      , res = res'
      , attrs = function.attrs
      , ann = function.ann
      }

processFunctionArg ::
     C.FunctionArg Select
  -> M (C.FunctionArg AdjustTypes)
processFunctionArg functionArg = do
    argTyp' <- processTypeFunArg functionArg.argTyp
    pure C.FunctionArg {
        name = functionArg.name
      , argTyp = argTyp'
      }

processGlobal ::
     C.Type Select
  -> M (C.Type AdjustTypes)
processGlobal global = do
    processType global

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

processType ::
     C.Type Select
  -> M (C.Type AdjustTypes)
processType = \case
    C.TypePrim primTy        -> pure $ C.TypePrim primTy
    C.TypeComplex primTy     -> pure $ C.TypeComplex primTy
    C.TypeRef name           -> pure $ C.TypeRef name
    C.TypeEnum ref           -> C.TypeEnum <$> do
        underlying' <- processType ref.underlying
        pure C.Ref {
            name = ref.name
          , underlying = underlying'
          }
    C.TypeMacro ref          -> C.TypeMacro <$> do
        underlying' <- processType ref.underlying
        pure C.Ref {
            name = ref.name
          , underlying = underlying'
          }
    C.TypeTypedef ref        -> C.TypeTypedef <$> do
        underlying' <- processType ref.underlying
        pure C.Ref {
            name = ref.name
          , underlying = underlying'
          }
    C.TypePointers n ty      -> C.TypePointers n <$> processType ty
    C.TypeConstArray n ty    -> C.TypeConstArray n <$> processType ty
    C.TypeIncompleteArray ty -> C.TypeIncompleteArray <$> processType ty
    C.TypeFun args res       -> do
        args' <- mapM processTypeFunArg args
        res' <- processType res
        pure $ C.TypeFun args' res'
    C.TypeVoid               -> pure C.TypeVoid
    C.TypeBlock ty           -> C.TypeBlock <$> processType ty
    C.TypeQual qual ty       -> C.TypeQual qual <$> processType ty
    C.TypeExtBinding ref     -> C.TypeExtBinding <$> do
        underlying' <- processType ref.underlying
        pure C.Ref {
            name = ref.name
          , underlying = underlying'
          }

processTypeFunArg :: C.TypeFunArg Select -> M (C.TypeFunArg AdjustTypes)
processTypeFunArg arg = do
    typ' <- processType arg.typ
    (typ'', ann') <- adjustFunArg typ'
    pure C.TypeFunArgF {
        typ = typ''
      , ann = ann'
      }

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
-- The original type before adjustment is recorded in an an 'Ann'otation.
--
adjustFunArg :: C.Type AdjustTypes -> M (C.Type AdjustTypes, AdjustedFrom AdjustTypes)
adjustFunArg ty
  | Just cls <- classifyCanonicalTypeArray ty
  , let elemTy = getArrayElementType cls
  , let constQual
          | C.isErasedTypeConstQualified ty
          , not (C.isErasedTypeConstQualified elemTy)
          = C.TypeQual C.QualConst
          | otherwise
          = id
  = pure (C.TypePointers 1 $ constQual elemTy, AdjustedFromArray ty)
  | C.isCanonicalTypeFunction ty
  = pure (C.TypePointers 1 ty, AdjustedFromFunction ty)
  | otherwise
  = pure (ty, NotAdjusted)

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
