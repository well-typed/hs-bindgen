module HsBindgen.Frontend.Pass.AdjustTypes (
    adjustTypes
  ) where

import Control.Monad.State (MonadState, State, StateT (StateT), modify',
                            runState)

import HsBindgen.Frontend.AST.Coerce (CoercePass (coercePass))
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass (IsPass (MacroBody, Msg))
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass (AdjustTypes)
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
-- > type
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
      C.DeclGlobal ext global              -> C.DeclGlobal ext <$> processGlobal global

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
    -- NOTE: currently macro expressions don't support function type parameters,
    -- if they do in the future, then we might have to recurse into the type of
    -- the macro expression?
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
    typ' <- processTypeFunArg functionArg.typ
    pure C.FunctionArg {
        name = functionArg.name
      , typ = typ'
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

processTypeFunArg :: C.Type Select -> M (C.Type AdjustTypes)
processTypeFunArg arg = do
    typ' <- processType arg
    adjustFunArg typ'

-- | This is where the actual adjustments take place.
--
-- Function arguments of function type are changed to *pointer* to function
-- type. For example, the former is adjusted to the latter:
--
-- > int foo (char f (float))
-- > int foo (char (*f)(float))
--
adjustFunArg :: C.Type AdjustTypes -> M (C.Type AdjustTypes)
adjustFunArg ty
  | C.isCanonicalTypeFunction ty
  = pure $ C.TypePointers 1 ty
  | otherwise
  = pure ty
