module HsBindgen.Frontend.Pass.HandleMacros (
    handleMacros
  ) where

import Control.Monad.State
import Data.Either (partitionEithers)
import Data.Map qualified as Map
import Data.Vec.Lazy qualified as Vec

import C.Expr.Parse.Expr qualified as CExpr.DSL
import C.Expr.Parse.Infra qualified as CExpr.DSL
import C.Expr.Syntax qualified as CExpr.DSL
import C.Expr.Typecheck.Expr qualified as CExpr.DSL
import C.Expr.Typecheck.Type qualified as CExpr.DSL

import Clang.Args (CStandard)
import Clang.HighLevel.Types

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.LanguageC qualified as LanC
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.HandleMacros.Error
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Sort and typecheck macros, and reparse declarations
handleMacros ::
      CStandard
  ->  C.TranslationUnit ConstructTranslationUnit
  -> (C.TranslationUnit HandleMacros, [Msg HandleMacros])
handleMacros standard C.TranslationUnit{unitDecls, unitIncludeGraph, unitAnn} =
    reconstruct $ runM standard .
      fmap partitionEithers $ mapM processDecl unitDecls
  where
    reconstruct ::
         (([FailedMacro] , [C.Decl HandleMacros]) , [Msg HandleMacros])
      -> (C.TranslationUnit HandleMacros, [Msg HandleMacros])
    reconstruct ((failedMacros, decls'), msgs) =
      let index' :: DeclIndex
          index' = DeclIndex.registerMacroFailures failedMacros unitAnn.declIndex

          unit = C.TranslationUnit{
              unitDecls = decls'
            , unitIncludeGraph
            , unitAnn = unitAnn {
                declIndex = index'
              }
            }
      in  (unit, msgs)

processDecl ::
     C.Decl ConstructTranslationUnit
  -> M (Either FailedMacro (C.Decl HandleMacros))
processDecl C.Decl{declInfo, declKind} =
    case declKind of
      C.DeclMacro macro      -> processMacro info' macro
      C.DeclTypedef typedef  -> Right <$> processTypedef info' typedef
      C.DeclStruct struct    -> Right <$> processStruct info' struct
      C.DeclUnion union      -> Right <$> processUnion info' union
      C.DeclEnum enum        -> Right <$> processEnum info' enum
      C.DeclOpaque           -> Right <$> processOpaque C.DeclOpaque info'
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

processStructField ::
     C.StructField ConstructTranslationUnit
  -> M (C.StructField HandleMacros)
processStructField C.StructField{..} =
    case structFieldAnn of
      ReparseNotNeeded ->
        withoutReparse
      ReparseNeeded tokens ->
        reparseWith LanC.reparseField tokens withoutReparse withReparse
        -- reparseWith LanC.reparseField tokens withoutReparse withReparse
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
         (C.Type HandleMacros, Text)
      -> M (C.StructField HandleMacros)
    withReparse (ty, name) = return C.StructField{
          structFieldInfo =
            C.FieldInfo {
              fieldName    = C.ScopedName name
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
         (C.Type HandleMacros, Text)
      -> M (C.UnionField HandleMacros)
    withReparse (ty, name) = return $ C.UnionField{
          unionFieldInfo =
            C.FieldInfo {
              fieldName    = C.ScopedName name
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
          stateReparseEnv = updateEnv info.declId.name.text (stateReparseEnv st)
        }
      case typedefAnn of
        ReparseNotNeeded -> withoutReparse

        -- If the @typedef@ refers to another type, we do not reparse the
        -- typedef, but instead defer reparsing to that other type.
        -- See https://github.com/well-typed/hs-bindgen/issues/707.
        --
        -- TODO <https://github.com/well-typed/hs-bindgen/issues/1382>
        -- We should allow for pointers.
        ReparseNeeded tokens -> case typedefType of
          C.TypeRef _ -> withoutReparse
          _otherwise  ->
            reparseWith LanC.reparseTypedef tokens withoutReparse withReparse
  where
    updateEnv :: Text -> LanC.ReparseEnv -> LanC.ReparseEnv
    updateEnv name =
        Map.insert name $
          C.TypeTypedef $ C.TypedefRef info.declId (coercePass typedefType)

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
  -> UnparsedMacro -> M (Either FailedMacro (C.Decl HandleMacros))
processMacro info (UnparsedMacro tokens) = do
    bimap addInfo toDecl <$> parseMacro info.declId.name tokens
  where
    addInfo :: HandleMacrosError -> FailedMacro
    addInfo macroError = FailedMacro{
          name = info.declId
        , loc  = info.declLoc
        , macroError
        }

    toDecl :: C.CheckedMacro HandleMacros -> C.Decl HandleMacros
    toDecl checked = C.Decl{
          declInfo = info
        , declKind = C.DeclMacro checked
        , declAnn  = NoAnn
        }

processFunction ::
     C.DeclInfo HandleMacros
  -> C.Function ConstructTranslationUnit
  -> M (C.Decl HandleMacros)
processFunction info C.Function{..} =
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
         (([(Maybe Text, C.Type HandleMacros)], C.Type HandleMacros), Text)
      -> M (C.Decl HandleMacros)
    withReparse ((tys, ty), _name) = do
       -- TODO: We should assert that the name is the name we were expecting
       return $ C.Decl{
           declInfo = info
         , declKind = C.DeclFunction C.Function{
               functionArgs = map (first (fmap C.ScopedName)) tys
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
      stateErrors :: [HandleMacrosReparseMsg]  -- ^ Stored in reverse order

      -- | Types of macro expressions
    , stateMacroEnv :: CExpr.DSL.TypeEnv

      -- | Newtypes and macro-defined types in scope
    , stateReparseEnv :: LanC.ReparseEnv
    }

initMacroState :: CStandard -> MacroState
initMacroState standard = MacroState{
      stateErrors     = []
    , stateMacroEnv   = Map.empty
    , stateReparseEnv = LanC.initReparseEnv standard
    }

runM :: CStandard -> M a -> (a, [Msg HandleMacros])
runM standard = fmap stateErrors . flip runState (initMacroState standard) . unwrapM

{-------------------------------------------------------------------------------
  Auxiliary: parsing (macro /def/ sites) and reparsing (macro /use/ sites)
-------------------------------------------------------------------------------}

-- | Parse macro
--
-- We also return the new macro type environment
parseMacro ::
     C.DeclName
  -> [Token TokenSpelling]
  -> M (Either HandleMacrosError (C.CheckedMacro HandleMacros))
parseMacro name []      = panicPure $ "macro " <> show name <> ": unexpected empty list of tokens"
parseMacro name [_]     = pure      $ Left $ HandleMacrosErrorEmpty name
parseMacro name tokens  = state     $ \st ->
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
                Left errTc -> (Left $ HandleMacrosErrorTc errTc, st)
          Left errExpr ->
              (Left $ HandleMacrosErrorParse errType errExpr, st)
  where
    updateReparseEnv :: LanC.ReparseEnv -> LanC.ReparseEnv
    updateReparseEnv =
        Map.insert name.text $
          C.TypeRef $ C.DeclId{name, isAnon = False}

    dropEval ::
         CExpr.DSL.Quant (CExpr.DSL.FunValue, CExpr.DSL.Type 'CExpr.DSL.Ty)
      -> CExpr.DSL.Quant (CExpr.DSL.Type 'CExpr.DSL.Ty)
    dropEval = fmap snd

-- | Run reparser
--
-- Failing to parse macros results in warnings, but never irrecoverable errors;
-- we therefore always want a fallback.
reparseWith ::
     LanC.Parser a          -- ^ Parser
  -> [Token TokenSpelling]  -- ^ Raw tokens
  -> M r                    -- ^ If parsing fails
  -> (a -> M r)             -- ^ If parsing succeeds
  -> M r
reparseWith p tokens onFailure onSuccess = state $ \st ->
    case p (stateReparseEnv st) tokens of
      Right a -> runState (unwrapM $ onSuccess a) st
      Left  e -> runState (unwrapM $ onFailure  ) st{
            stateErrors = HandleMacrosErrorReparse e : stateErrors st
          }
