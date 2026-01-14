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
processStruct info struct =
    mkDecl
      <$> mapM processStructField struct.fields
      <*> mapM processStructField struct.flam
  where
    mkDecl ::
         [C.StructField HandleMacros]
      -> Maybe (C.StructField HandleMacros)
      -> C.Decl HandleMacros
    mkDecl fields flam = C.Decl{
          info = info
        , ann  = NoAnn
        , kind = C.DeclStruct C.Struct{
              fields    = fields
            , flam      = flam
            , sizeof    = struct.sizeof
            , alignment = struct.alignment
            , ann       = struct.ann
            }
        }

processStructField ::
     C.StructField ConstructTranslationUnit
  -> M (C.StructField HandleMacros)
processStructField field =
    case field.ann of
      ReparseNotNeeded ->
        withoutReparse
      ReparseNeeded tokens ->
        reparseWith LanC.reparseField tokens withoutReparse withReparse
  where
    withoutReparse :: M (C.StructField HandleMacros)
    withoutReparse = return C.StructField{
          typ    = coercePass field.typ
        , ann    = NoAnn
        , offset = field.offset
        , width  = field.width
        , info   = C.FieldInfo {
              comment = fmap coercePass field.info.comment
            , name    = field.info.name
            , loc     = field.info.loc
            }
        }

    withReparse ::
         (C.Type HandleMacros, Text)
      -> M (C.StructField HandleMacros)
    withReparse (ty, name) = return C.StructField{
          typ    = ty
        , ann    = NoAnn
        , offset = field.offset
        , width  = field.width
        , info   = C.FieldInfo {
              name    = C.ScopedName name
            , comment = fmap coercePass field.info.comment
            , loc     = field.info.loc
            }
        }

processUnion ::
     C.DeclInfo HandleMacros
  -> C.Union ConstructTranslationUnit
  -> M (C.Decl HandleMacros)
processUnion info union =
    combineFields <$> mapM processUnionField union.fields
  where
    combineFields :: [C.UnionField HandleMacros] -> C.Decl HandleMacros
    combineFields fields = C.Decl{
          info = info
        , ann  = NoAnn
        , kind = C.DeclUnion C.Union{
              fields    = fields
            , sizeof    = union.sizeof
            , alignment = union.alignment
            , ann       = union.ann
            }
        }

processUnionField ::
     C.UnionField ConstructTranslationUnit
  -> M (C.UnionField HandleMacros)
processUnionField field =
    case field.ann of
      ReparseNotNeeded ->
        withoutReparse
      ReparseNeeded tokens ->
        reparseWith LanC.reparseField tokens withoutReparse withReparse
  where
    withoutReparse :: M (C.UnionField HandleMacros)
    withoutReparse = return $ C.UnionField{
          typ  = coercePass field.typ
        , ann  = NoAnn
        , info = C.FieldInfo {
              comment = fmap coercePass field.info.comment
            , name    = field.info.name
            , loc     = field.info.loc
            }
        }

    withReparse ::
         (C.Type HandleMacros, Text)
      -> M (C.UnionField HandleMacros)
    withReparse (ty, name) = return $ C.UnionField{
          typ  = ty
        , ann  = NoAnn
        , info = C.FieldInfo {
              name    = C.ScopedName name
            , comment = fmap coercePass field.info.comment
            , loc     = field.info.loc
            }
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
processEnum info enum =
    mkDecl <$> mapM processEnumConstant enum.constants
  where
    mkDecl :: [C.EnumConstant HandleMacros] -> C.Decl HandleMacros
    mkDecl enumerators = C.Decl{
          info = info
        , ann  = NoAnn
        , kind = C.DeclEnum C.Enum{
              typ       = coercePass enum.typ
            , constants = enumerators
            , sizeof    = enum.sizeof
            , alignment = enum.alignment
            , ann       = enum.ann
            }
        }

processEnumConstant ::
     C.EnumConstant ConstructTranslationUnit
  -> M (C.EnumConstant HandleMacros)
processEnumConstant constant = return C.EnumConstant {
      value = constant.value
    , info  = C.FieldInfo {
          comment = fmap coercePass constant.info.comment
        , name    = constant.info.name
        , loc     = constant.info.loc
        }
    }

processTypedef ::
     C.DeclInfo HandleMacros
  -> C.Typedef ConstructTranslationUnit
  -> M (C.Decl HandleMacros)
processTypedef info typedef = do
    modify $ over #reparseEnv $ updateEnv info.id.name.text
    case typedef.ann of
      ReparseNotNeeded -> withoutReparse

      -- If the @typedef@ refers to another type, we do not reparse the
      -- typedef, but instead defer reparsing to that other type.
      -- See https://github.com/well-typed/hs-bindgen/issues/707.
      --
      -- TODO <https://github.com/well-typed/hs-bindgen/issues/1382>
      -- We should allow for pointers.
      ReparseNeeded tokens -> case typedef.typ of
        C.TypeRef _ -> withoutReparse
        _otherwise  ->
          reparseWith LanC.reparseTypedef tokens withoutReparse withReparse
  where
    updateEnv :: Text -> LanC.ReparseEnv -> LanC.ReparseEnv
    updateEnv name =
        Map.insert name $
          C.TypeTypedef $ C.Ref info.id (coercePass typedef.typ)

    withoutReparse :: M (C.Decl HandleMacros)
    withoutReparse = return C.Decl{
          info = info
        , ann  = NoAnn
        , kind = C.DeclTypedef C.Typedef {
                     typ = coercePass typedef.typ
                   , ann = NoAnn
                   }
        }

    withReparse :: C.Type HandleMacros -> M (C.Decl HandleMacros)
    withReparse ty = return C.Decl{
          info = info
        , ann  = NoAnn
        , kind = C.DeclTypedef C.Typedef{
                     typ = ty
                   , ann = NoAnn
                   }
        }

processMacro ::
     C.DeclInfo HandleMacros
  -> UnparsedMacro -> M (Either FailedMacro (C.Decl HandleMacros))
processMacro info (UnparsedMacro tokens) = do
    bimap addInfo toDecl <$> parseMacro info.id.name tokens
  where
    addInfo :: HandleMacrosError -> FailedMacro
    addInfo err = FailedMacro{
          name       = info.id
        , loc        = info.loc
        , macroError = err
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
processFunction info function =
    case function.ann of
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
              args  = map (bimap id coercePass) function.args
            , res   = coercePass function.res
            , attrs = function.attrs
            , ann   = NoAnn
            }
        }

    withReparse ::
         (([(Maybe Text, C.Type HandleMacros)], C.Type HandleMacros), Text)
      -> M (C.Decl HandleMacros)
    withReparse ((tys, ty), _name) = return $ C.Decl{
           info = info
         , ann  = NoAnn
         , kind = C.DeclFunction C.Function{
               args  = map (first (fmap C.ScopedName)) tys
             , res   = ty
             , attrs = function.attrs
             , ann   = NoAnn
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

newtype M a = WrapM (
      State MacroState a
    )
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadState MacroState
    )

data MacroState = MacroState {
      errors :: [HandleMacrosReparseMsg]  -- ^ Stored in reverse order

      -- | Types of macro expressions
    , macroEnv :: CExpr.DSL.TypeEnv

      -- | Newtypes and macro-defined types in scope
    , reparseEnv :: LanC.ReparseEnv
    }
  deriving stock (Generic)

initMacroState :: CStandard -> MacroState
initMacroState standard = MacroState{
      errors     = []
    , macroEnv   = Map.empty
    , reparseEnv = LanC.initReparseEnv standard
    }

runM :: CStandard -> M a -> (a, [Msg HandleMacros])
runM standard (WrapM ma) = (.errors) <$> runState ma (initMacroState standard)

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
    case LanC.parseMacroType st.reparseEnv tokens of
      Right typ -> (
          Right $ MacroType $ CheckedMacroType typ NoAnn
        , st & #reparseEnv %~ updateReparseEnv typ
        )
      Left errType ->
        case CExpr.DSL.runParser CExpr.DSL.parseExpr tokens of
          Right CExpr.DSL.Macro{
                    macroName = name'
                  , macroArgs = args
                  , macroBody = body
                  } ->
            Vec.reifyList args $ \args' -> do
              case CExpr.DSL.tcMacro st.macroEnv name' args' body of
                Right inf -> (
                    Right $ MacroExpr $ CheckedMacroExpr{
                        args = args
                      , body = body
                      , typ  = dropEval inf
                      }
                  , st & #macroEnv %~ Map.insert name' inf
                  )
                Left errTc -> (Left $ HandleMacrosErrorTc errTc, st)
          Left errExpr ->
              (Left $ HandleMacrosErrorParse errType errExpr, st)
  where
    updateReparseEnv :: C.Type HandleMacros -> LanC.ReparseEnv -> LanC.ReparseEnv
    updateReparseEnv typ =
        Map.insert name.text $
          C.TypeMacro $ C.Ref {
              name = DeclId{name = name, isAnon = False}
            , underlying = typ
            }

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
    case p st.reparseEnv tokens of
      Right a -> runState (unwrapM $ onSuccess a) st
      Left  e -> let st' = st & #errors %~ (HandleMacrosErrorReparse e :)
                 in runState (unwrapM $ onFailure  ) st'
  where
    unwrapM :: M a -> (State MacroState a)
    unwrapM (WrapM ma) = ma
