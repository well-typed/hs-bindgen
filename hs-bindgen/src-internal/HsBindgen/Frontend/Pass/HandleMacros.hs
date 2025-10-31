module HsBindgen.Frontend.Pass.HandleMacros (
    handleMacros
  ) where

import Control.Monad.State
import Data.Either (partitionEithers)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Vec.Lazy qualified as Vec

import C.Expr.Parse.Expr qualified as CExpr.DSL
import C.Expr.Parse.Infra qualified as CExpr.DSL
import C.Expr.Syntax qualified as CExpr.DSL
import C.Expr.Typecheck.Expr qualified as CExpr.DSL
import C.Expr.Typecheck.Type qualified as CExpr.DSL

import Clang.HighLevel.Types

import HsBindgen.Errors
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.LanguageC qualified as LanC
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Sort and typecheck macros, and reparse declarations
handleMacros ::
      C.TranslationUnit ConstructTranslationUnit
  -> (C.TranslationUnit HandleMacros, Set C.Name, [Msg HandleMacros])
handleMacros C.TranslationUnit{unitDecls, unitIncludeGraph, unitAnn} =
    reassemble $ runM . fmap partitionEithers $ mapM processDecl unitDecls
  where
    reassemble ::
         (([C.Name], [C.Decl HandleMacros]), [Msg HandleMacros])
      -> (C.TranslationUnit HandleMacros, Set C.Name, [Msg HandleMacros])
    reassemble ((failedMacros, decls'), msgs) =
      let unit = C.TranslationUnit{
              unitDecls = decls'
            , unitIncludeGraph
            , unitAnn
            }
      in  (unit, Set.fromList failedMacros, msgs)

processDecl ::
     C.Decl ConstructTranslationUnit
  -> M (Either C.Name (C.Decl HandleMacros))
processDecl C.Decl{declInfo, declKind} =
    case declKind of
      C.DeclMacro macro      -> processMacro info' macro
      C.DeclTypedef typedef  -> Right <$> processTypedef info' typedef
      C.DeclStruct struct    -> Right <$> processStruct info' struct
      C.DeclUnion union      -> Right <$> processUnion info' union
      C.DeclEnum enum        -> Right <$> processEnum info' enum
      C.DeclOpaque cNameKind -> Right <$> processOpaque (C.DeclOpaque cNameKind) info'
      C.DeclFunction fun     -> Right <$> processFunction info' fun
      C.DeclGlobal ty        -> Right <$> processGlobal info' C.DeclGlobal ty
  where
    info' :: C.DeclInfo HandleMacros
    info' = coercePass declInfo

{-------------------------------------------------------------------------------
  Function for each kind of declaration
-------------------------------------------------------------------------------}

processStruct ::
     C.DeclInfo HandleMacros
  -> C.Struct ConstructTranslationUnit
  -> M (C.Decl HandleMacros)
processStruct info C.Struct{..} =
    mkDecl <$> mapM processStructField structFields
  where
    mkDecl :: [C.StructField HandleMacros] -> C.Decl HandleMacros
    mkDecl fields = C.Decl{
          declInfo = info
        , declKind = C.DeclStruct C.Struct{structFields = fields, ..}
        , declAnn  = NoAnn
        }

processStructField :: C.StructField ConstructTranslationUnit -> M (C.StructField HandleMacros)
processStructField C.StructField{..} =
    case structFieldAnn of
      ReparseNotNeeded ->
        withoutReparse
      ReparseNeeded tokens ->
        reparseWith LanC.reparseField tokens withoutReparse withReparse
  where
    C.FieldInfo{..} = structFieldInfo

    withoutReparse :: M (C.StructField HandleMacros)
    withoutReparse = return C.StructField{
          structFieldInfo =
            C.FieldInfo {
              fieldComment = fmap coercePass fieldComment
            , ..
            }
        , structFieldType = coercePass structFieldType
        , structFieldAnn  = NoAnn
        , ..
        }

    withReparse ::
         (C.Type HandleMacros, FieldName HandleMacros)
      -> M (C.StructField HandleMacros)
    withReparse (ty, name) = return C.StructField{
          structFieldInfo =
            C.FieldInfo {
              fieldName    = name
            , fieldComment = fmap coercePass fieldComment
            , ..
            }
        , structFieldType = ty
        , structFieldAnn  = NoAnn
        , ..
        }

processUnion ::
     C.DeclInfo HandleMacros
  -> C.Union ConstructTranslationUnit
  -> M (C.Decl HandleMacros)
processUnion info C.Union{..} =
    combineFields <$> mapM processUnionField unionFields
  where
    combineFields :: [C.UnionField HandleMacros] -> C.Decl HandleMacros
    combineFields fields = C.Decl{
          declInfo = info
        , declKind = C.DeclUnion C.Union{unionFields = fields, ..}
        , declAnn  = NoAnn
        }

processUnionField :: C.UnionField ConstructTranslationUnit -> M (C.UnionField HandleMacros)
processUnionField C.UnionField{..} =
    case unionFieldAnn of
      ReparseNotNeeded ->
        withoutReparse
      ReparseNeeded tokens ->
        reparseWith LanC.reparseField tokens withoutReparse withReparse
  where
    C.FieldInfo{..} = unionFieldInfo
    withoutReparse :: M (C.UnionField HandleMacros)
    withoutReparse = return $ C.UnionField{
          unionFieldInfo =
            C.FieldInfo {
              fieldComment = fmap coercePass fieldComment
            , ..
            }
        , unionFieldType = coercePass unionFieldType
        , unionFieldAnn  = NoAnn
        , ..
        }

    withReparse ::
         (C.Type HandleMacros, FieldName HandleMacros)
      -> M (C.UnionField HandleMacros)
    withReparse (ty, name) = return $ C.UnionField{
          unionFieldInfo =
            C.FieldInfo {
              fieldName    = name
            , fieldComment = fmap coercePass fieldComment
            , ..
            }
        , unionFieldType = ty
        , unionFieldAnn  = NoAnn
        , ..
        }

processOpaque ::
     C.DeclKind HandleMacros
  -> C.DeclInfo HandleMacros
  -> M (C.Decl HandleMacros)
processOpaque kind info =
    return C.Decl{
        declInfo = info
      , declKind = kind
      , declAnn  = NoAnn
      }

processEnum ::
     C.DeclInfo HandleMacros
  -> C.Enum ConstructTranslationUnit
  -> M (C.Decl HandleMacros)
processEnum info C.Enum{..} =
    mkDecl <$> mapM processEnumConstant enumConstants
  where
    mkDecl :: [C.EnumConstant HandleMacros] -> C.Decl HandleMacros
    mkDecl enumerators = C.Decl{
          declInfo = info
        , declKind = C.DeclEnum C.Enum{
                          enumType      = coercePass enumType
                        , enumConstants = enumerators
                        , ..
                        }
        , declAnn  = NoAnn
        }

processEnumConstant ::
     C.EnumConstant ConstructTranslationUnit
  -> M (C.EnumConstant HandleMacros)
processEnumConstant C.EnumConstant{..} = return
  C.EnumConstant {
    enumConstantInfo =
      C.FieldInfo {
        fieldComment = fmap coercePass fieldComment
      , ..
      }
  , ..
  }
  where
    C.FieldInfo{..} = enumConstantInfo

processTypedef ::
     C.DeclInfo HandleMacros
  -> C.Typedef ConstructTranslationUnit
  -> M (C.Decl HandleMacros)
processTypedef info C.Typedef{typedefType, typedefAnn} = do
    modify $ \st -> st{
        stateReparseEnv = updateEnv (stateReparseEnv st)
      }
    case typedefAnn of
      ReparseNotNeeded ->
        withoutReparse
      -- HACK: If the @typedef@ refers to a @enum@ or a @struct@, we do not
      -- reparse the complete declaration, which will fail due to
      --
      -- "unsupported member declaration list in struct specifier", or
      -- "unsupported enumerator list in enum specifier".
      --
      -- Instead, we defer the reparse to the @enumerator@ or @field@, which is
      -- also labeled as 'ReparseNeeded'.
      --
      -- See https://github.com/well-typed/hs-bindgen/issues/707.
      ReparseNeeded tokens -> case typedefType of
        C.TypeEnum _   -> withoutReparse
        C.TypeStruct _ -> withoutReparse
        _otherwise     -> reparseWith LanC.reparseTypedef tokens withoutReparse withReparse
  where
    name :: C.Name
    name = case C.declId info of
      C.PrelimDeclIdNamed n -> n
      _otherwise            -> panicPure "unexpected anonymous typedef"

    updateEnv :: LanC.ReparseEnv HandleMacros -> LanC.ReparseEnv HandleMacros
    updateEnv = Map.insert name (C.TypeTypedef (OrigTypedefRef name (coercePass typedefType)))

    withoutReparse :: M (C.Decl HandleMacros)
    withoutReparse = return C.Decl{
          declInfo = info
        , declKind = C.DeclTypedef C.Typedef {
              typedefType = coercePass typedefType
            , typedefAnn  = NoAnn
            }
        , declAnn  = NoAnn
        }

    withReparse :: C.Type HandleMacros -> M (C.Decl HandleMacros)
    withReparse ty = return C.Decl{
          declInfo = info
        , declKind = C.DeclTypedef C.Typedef{
              typedefType = ty
            , typedefAnn  = NoAnn
            }
        , declAnn  = NoAnn
        }

processMacro ::
     C.DeclInfo HandleMacros
  -> UnparsedMacro -> M (Either C.Name (C.Decl HandleMacros))
processMacro info (UnparsedMacro tokens) = do
    -- Simply omit macros from the AST that we cannot parse
    fmap aux <$> parseMacro name tokens
  where
    name :: C.Name
    name = case C.declId info of
      C.PrelimDeclIdNamed n -> n
      _otherwise            -> panicPure "unexpected anonymous macro"

    aux :: C.CheckedMacro HandleMacros -> C.Decl HandleMacros
    aux checked = C.Decl{
          declInfo = info
        , declKind = C.DeclMacro checked
        , declAnn  = NoAnn
        }

processFunction ::
     C.DeclInfo HandleMacros
  -> C.Function ConstructTranslationUnit
  -> M (C.Decl HandleMacros)
processFunction info C.Function {..} =
    case functionAnn of
      ReparseNotNeeded ->
        withoutReparse
      ReparseNeeded tokens ->
        reparseWith LanC.reparseFunDecl tokens withoutReparse withReparse
  where
    withoutReparse :: M (C.Decl HandleMacros)
    withoutReparse = return C.Decl{
          declInfo = info
        , declKind = C.DeclFunction C.Function{
              functionArgs = map (bimap id coercePass) functionArgs
            , functionRes = coercePass functionRes
            , functionAnn = NoAnn
            , ..
            }
        , declAnn = NoAnn
        }

    withReparse ::
         (([(ArgumentName HandleMacros, C.Type HandleMacros)], C.Type HandleMacros), C.Name)
      -> M (C.Decl HandleMacros)
    withReparse ((tys, ty), _name) = do
       -- TODO: We should assert that the name is the name we were expecting
       return $ C.Decl{
           declInfo = info
         , declKind = C.DeclFunction C.Function{
               functionArgs = tys
             , functionRes  = ty
             , functionAnn  = NoAnn
             , ..
             }
         , declAnn = NoAnn
         }

-- | Globals (externs or constants)
--
-- TODO <https://github.com/well-typed/hs-bindgen/issues/831>
-- We don't yet reparse these.
processGlobal ::
     C.DeclInfo HandleMacros
  -> (C.Type HandleMacros -> C.DeclKind HandleMacros)
  -> C.Type ConstructTranslationUnit
  -> M (C.Decl HandleMacros)
processGlobal info f ty =
    return $ C.Decl{
        declInfo = info
      , declKind = f (coercePass ty)
      , declAnn  = NoAnn
      }

{-------------------------------------------------------------------------------
  Internal: monad used for parsing macros
-------------------------------------------------------------------------------}

newtype M a = WrapM {
      unwrapM :: State MacroState a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadState MacroState
    )

data MacroState = MacroState {
      stateErrors :: [HandleMacrosMsg]  -- ^ Stored in reverse order

      -- | Types of macro expressions
    , stateMacroEnv :: CExpr.DSL.TypeEnv

      -- | Newtypes and macro-defined types in scope
    , stateReparseEnv :: LanC.ReparseEnv HandleMacros
    }

initMacroState :: MacroState
initMacroState = MacroState{
      stateErrors     = []
    , stateMacroEnv   = Map.empty
    , stateReparseEnv = LanC.initReparseEnv
    }

runM :: M a -> (a, [Msg HandleMacros])
runM = fmap stateErrors . flip runState initMacroState . unwrapM

{-------------------------------------------------------------------------------
  Auxiliary: parsing (macro /def/ sites) and reparsing (macro /use/ sites)
-------------------------------------------------------------------------------}

-- | Parse macro
--
-- We also return the new macro type environment
parseMacro ::
     C.Name
  -> [Token TokenSpelling]
  -> M (Either C.Name (C.CheckedMacro HandleMacros))
parseMacro name tokens = state $ \st ->
    -- In the case that the same macro could be interpreted both as a type or
    -- as an expression, we choose to interpret it as a type.
    case LanC.parseMacroType (stateReparseEnv st) tokens of
      Right typ -> (
          Right $ C.MacroType $ C.CheckedMacroType typ NoAnn
        , st{stateReparseEnv = updateReparseEnv (stateReparseEnv st)}
        )
      Left errType ->
        case CExpr.DSL.runParser CExpr.DSL.parseExpr tokens of
          Right CExpr.DSL.Macro{macroName, macroArgs, macroBody} ->
            Vec.reifyList macroArgs $ \args -> do
              case CExpr.DSL.tcMacro (stateMacroEnv st) macroName args macroBody of
                Right inf -> (
                    Right $ C.MacroExpr $ C.CheckedMacroExpr{
                        macroExprArgs = macroArgs
                      , macroExprBody = macroBody
                      , macroExprType = dropEval inf
                      }
                  , st{stateMacroEnv = Map.insert macroName inf (stateMacroEnv st)}
                  )
                Left errTc -> (
                    Left name
                  , st{stateErrors = HandleMacrosErrorTc errTc : stateErrors st}
                  )
          Left errExpr -> (
              Left name
            , st{stateErrors =
                    HandleMacrosErrorParse errType errExpr
                  : stateErrors st
                }
            )

  where
    updateReparseEnv ::
         LanC.ReparseEnv HandleMacros
      -> LanC.ReparseEnv HandleMacros
    updateReparseEnv =
        Map.insert name (C.TypeMacroTypedef $ C.PrelimDeclIdNamed name)

    dropEval ::
         CExpr.DSL.Quant (CExpr.DSL.FunValue, CExpr.DSL.Type 'CExpr.DSL.Ty)
      -> CExpr.DSL.Quant (CExpr.DSL.Type 'CExpr.DSL.Ty)
    dropEval = fmap snd

-- | Run reparser
--
-- Failing to parse macros results in warnings, but never irrecoverable errors;
-- we therefore always want a fallback.
reparseWith ::
     LanC.Parser HandleMacros a  -- ^ Parser
  -> [Token TokenSpelling]       -- ^ Raw tokens
  -> M r                         -- ^ If parsing fails
  -> (a -> M r)                  -- ^ If parsing succeeds
  -> M r
reparseWith p tokens onFailure onSuccess = state $ \st ->
    case p (stateReparseEnv st) tokens of
      Right a -> runState (unwrapM $ onSuccess a) st
      Left  e -> runState (unwrapM $ onFailure  ) st{
            stateErrors = HandleMacrosErrorReparse e : stateErrors st
          }
