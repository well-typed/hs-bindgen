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
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.LanguageC qualified as LanC
import HsBindgen.Frontend.Naming
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
handleMacros standard unit =
    reconstruct $ runM standard .
      fmap partitionEithers $ mapM processDecl unit.decls
  where
    reconstruct ::
         (([FailedMacro] , [C.Decl HandleMacros]) , [Msg HandleMacros])
      -> (C.TranslationUnit HandleMacros, [Msg HandleMacros])
    reconstruct ((failedMacros, decls'), msgs) =
        let unit' = C.TranslationUnit{
                decls        = decls'
              , includeGraph = unit.includeGraph
              , ann          = unit.ann{
                    declIndex = DeclIndex.registerMacroFailures
                                  failedMacros
                                  unit.ann.declIndex
                  }
              }
        in (unit', msgs)

processDecl ::
     C.Decl ConstructTranslationUnit
  -> M (Either FailedMacro (C.Decl HandleMacros))
processDecl decl =
    case decl.kind of
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
    info' = coercePass decl.info

{-------------------------------------------------------------------------------
  Function for each kind of declaration
-------------------------------------------------------------------------------}

processStruct ::
     C.DeclInfo HandleMacros
  -> C.Struct ConstructTranslationUnit
  -> M (C.Decl HandleMacros)
processStruct info C.Struct{..} =
    mkDecl
      <$> mapM processStructField structFields
      <*> mapM processStructField structFlam
  where
    mkDecl ::
         [C.StructField HandleMacros]
      -> Maybe (C.StructField HandleMacros)
      -> C.Decl HandleMacros
    mkDecl fields flam = C.Decl{
          info = info
        , kind = C.DeclStruct C.Struct{
                         structFields = fields
                       , structFlam   = flam
                       , ..
                       }
        , ann  = NoAnn
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
          info = info
        , kind = C.DeclUnion C.Union{unionFields = fields, ..}
        , ann  = NoAnn
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
        info = info
      , kind = kind
      , ann  = NoAnn
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
          info = info
        , kind = C.DeclEnum C.Enum{
                      enumType      = coercePass enumType
                    , enumConstants = enumerators
                    , ..
                    }
        , ann  = NoAnn
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
          stateReparseEnv = updateEnv info.id.name.text (stateReparseEnv st)
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
          C.TypeTypedef $ C.TypedefRef info.id (coercePass typedefType)

    withoutReparse :: M (C.Decl HandleMacros)
    withoutReparse = return C.Decl{
          info = info
        , ann  = NoAnn
        , kind = C.DeclTypedef C.Typedef {
                     typedefType = coercePass typedefType
                   , typedefAnn  = NoAnn
                   }
        }

    withReparse :: C.Type HandleMacros -> M (C.Decl HandleMacros)
    withReparse ty = return C.Decl{
          info = info
        , ann  = NoAnn
        , kind = C.DeclTypedef C.Typedef{
                     typedefType = ty
                   , typedefAnn  = NoAnn
                   }
        }

processMacro ::
     C.DeclInfo HandleMacros
  -> UnparsedMacro -> M (Either FailedMacro (C.Decl HandleMacros))
processMacro info (UnparsedMacro tokens) = do
    bimap addInfo toDecl <$> parseMacro info.id.name tokens
  where
    addInfo :: HandleMacrosError -> FailedMacro
    addInfo macroError = FailedMacro{
          name = info.id
        , loc  = info.loc
        , macroError
        }

    toDecl :: CheckedMacro HandleMacros -> C.Decl HandleMacros
    toDecl checked = C.Decl{
          info = info
        , kind = C.DeclMacro checked
        , ann  = NoAnn
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
          info = info
        , ann  = NoAnn
        , kind = C.DeclFunction C.Function{
                     functionArgs = map (bimap id coercePass) functionArgs
                   , functionRes = coercePass functionRes
                   , functionAnn = NoAnn
                   , ..
                   }
        }

    withReparse ::
         (([(Maybe Text, C.Type HandleMacros)], C.Type HandleMacros), Text)
      -> M (C.Decl HandleMacros)
    withReparse ((tys, ty), _name) = do
       -- TODO: We should assert that the name is the name we were expecting
       return $ C.Decl{
           info = info
         , ann  = NoAnn
         , kind = C.DeclFunction C.Function{
                      functionArgs = map (first (fmap C.ScopedName)) tys
                    , functionRes  = ty
                    , functionAnn  = NoAnn
                    , ..
                    }
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
        info = info
      , kind = f (coercePass ty)
      , ann  = NoAnn
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
  -> M (Either HandleMacrosError (CheckedMacro HandleMacros))
parseMacro name []      = panicPure $ "macro " <> show name <> ": unexpected empty list of tokens"
parseMacro name [_]     = pure      $ Left $ HandleMacrosErrorEmpty name
parseMacro name tokens  = state     $ \st ->
    -- In the case that the same macro could be interpreted both as a type or
    -- as an expression, we choose to interpret it as a type.
    case LanC.parseMacroType (stateReparseEnv st) tokens of
      Right typ -> (
          Right $ MacroType $ CheckedMacroType typ NoAnn
        , st{stateReparseEnv = updateReparseEnv (stateReparseEnv st)}
        )
      Left errType ->
        case CExpr.DSL.runParser CExpr.DSL.parseExpr tokens of
          Right CExpr.DSL.Macro{macroName, macroArgs, macroBody} ->
            Vec.reifyList macroArgs $ \args -> do
              case CExpr.DSL.tcMacro (stateMacroEnv st) macroName args macroBody of
                Right inf -> (
                    Right $ MacroExpr $ CheckedMacroExpr{
                        args = macroArgs
                      , body = macroBody
                      , typ  = dropEval inf
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
          C.TypeRef $ DeclId{name, isAnon = False}

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
