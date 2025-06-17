module HsBindgen.Frontend.Pass.HandleMacros (
    handleMacros
  , MacroError(..)
  ) where

import Control.Monad.State
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vec.Lazy qualified as Vec

import Clang.HighLevel.Types
import HsBindgen.C.Reparse (ReparseError)
import HsBindgen.C.Reparse qualified as Reparse
import HsBindgen.C.Reparse.Decl qualified as Reparse
import HsBindgen.C.Tc.Macro (TcMacroError)
import HsBindgen.C.Tc.Macro qualified as Macro
import HsBindgen.C.Tc.Macro.Type (MacroTypes)
import HsBindgen.C.Tc.Macro.Type qualified as Macro
import HsBindgen.Errors
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Macros.AST.Syntax
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Sort.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Sort and typecheck macros, and reparse declarations
handleMacros ::
      C.TranslationUnit Sort
  -> (C.TranslationUnit HandleMacros, [MacroError])
handleMacros C.TranslationUnit{unitDecls, unitIncludeGraph, unitAnn} =
    first reassemble $ runM . fmap catMaybes $ mapM processDecl unitDecls
  where
    reassemble :: [C.Decl HandleMacros] -> C.TranslationUnit HandleMacros
    reassemble decls' = C.TranslationUnit{
          unitDecls = decls'
        , unitIncludeGraph
        , unitAnn
        }

processDecl :: C.Decl Sort -> M (Maybe (C.Decl HandleMacros))
processDecl C.Decl{declInfo, declKind} =
    case declKind of
      C.DeclMacro   macro   -> processMacro info' macro
      C.DeclTypedef typedef -> Just <$> processTypedef info' typedef
      C.DeclStruct  struct  -> Just <$> processStruct info' struct
      C.DeclStructOpaque    -> Just <$> processOpaque C.DeclStructOpaque info'
      C.DeclUnion   union   -> Just <$> processUnion info' union
      C.DeclUnionOpaque     -> Just <$> processOpaque C.DeclUnionOpaque info'
      C.DeclEnum    enum    -> Just <$> processEnum info' enum
      C.DeclEnumOpaque      -> Just <$> processOpaque C.DeclEnumOpaque info'
      C.DeclFunction fun    -> Just <$> processFunction info' fun
  where
    C.DeclInfo{declId, declLoc, declOrigin, declAliases} = declInfo

    info' :: C.DeclInfo HandleMacros
    info' = C.DeclInfo{declId, declLoc, declOrigin, declAliases}

{-------------------------------------------------------------------------------
  Function for each kind of declaration
-------------------------------------------------------------------------------}

processStruct ::
     C.DeclInfo HandleMacros
  -> C.Struct Sort
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

processStructField :: C.StructField Sort -> M (C.StructField HandleMacros)
processStructField C.StructField{..} =
    case structFieldAnn of
      ReparseNotNeeded ->
        withoutReparse
      ReparseNeeded tokens ->
        reparseWith reparseField tokens withoutReparse withReparse
  where
    withoutReparse :: M (C.StructField HandleMacros)
    withoutReparse = return C.StructField{
          structFieldType = coercePass structFieldType
        , structFieldAnn  = NoAnn
        , ..
        }

    withReparse ::
         (C.Type HandleMacros, FieldName HandleMacros)
      -> M (C.StructField HandleMacros)
    withReparse (ty, name) = return C.StructField{
          structFieldName = name
        , structFieldType = ty
        , structFieldAnn  = NoAnn
        , ..
        }

processUnion ::
     C.DeclInfo HandleMacros
  -> C.Union Sort
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

processUnionField :: C.UnionField Sort -> M (C.UnionField HandleMacros)
processUnionField C.UnionField{..} =
    case unionFieldAnn of
      ReparseNotNeeded ->
        withoutReparse
      ReparseNeeded tokens ->
        reparseWith reparseField tokens withoutReparse withReparse
  where
    withoutReparse :: M (C.UnionField HandleMacros)
    withoutReparse = return $ C.UnionField{
          unionFieldType = coercePass unionFieldType
        , unionFieldAnn  = NoAnn
        , ..
        }

    withReparse ::
         (C.Type HandleMacros, FieldName HandleMacros)
      -> M (C.UnionField HandleMacros)
    withReparse (ty, name) = return $ C.UnionField{
          unionFieldName = name
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
  -> C.Enum Sort
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
     C.EnumConstant Sort
  -> M (C.EnumConstant HandleMacros)
processEnumConstant C.EnumConstant{..} = return C.EnumConstant{..}

processTypedef ::
     C.DeclInfo HandleMacros
  -> C.Typedef Sort
  -> M (C.Decl HandleMacros)
processTypedef info C.Typedef{typedefType, typedefAnn} = do
    modify $ \st -> st{
        stateTypedefs = Set.insert name (stateTypedefs st)
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
        C.TypeEnum _ _   -> withoutReparse
        C.TypeStruct _ _ -> withoutReparse
        _otherwise       -> reparseWith reparseTypedef tokens withoutReparse withReparse
  where
    name :: CName
    name = case C.declId info of
             DeclNamed n -> n
             _otherwise  -> panicPure "unexpected anonymous typedef"

    withoutReparse :: M (C.Decl HandleMacros)
    withoutReparse = return C.Decl{
          declInfo = info
        , declKind = C.DeclTypedef C.Typedef{
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
  -> UnparsedMacro -> M (Maybe (C.Decl HandleMacros))
processMacro info (UnparsedMacro tokens) =
    -- Simply omit macros from the AST that we cannot parse
    reparseWith reparseMacro tokens (return Nothing) (fmap Just . withReparse)
  where
    name :: CName
    name = case C.declId info of
             DeclNamed n -> n
             _otherwise  -> panicPure "unexpected anonymous macro"

    withReparse ::
         ( Macro.Quant (FunValue, Macro.Type Macro.Ty)
         , C.CheckedMacro HandleMacros
         )
      -> M (C.Decl HandleMacros)
    withReparse (ty, checkedMacro) = do
        modify $ \st -> st{
            stateMacroTypes = Map.insert name ty (stateMacroTypes st)
          }
        return C.Decl{
            declInfo = info
          , declKind = C.DeclMacro checkedMacro
          , declAnn  = NoAnn
          }

processFunction ::
     C.DeclInfo HandleMacros
  -> C.Function Sort
  -> M (C.Decl HandleMacros)
processFunction info C.Function {..} =
    case functionAnn of
      ReparseNotNeeded ->
        withoutReparse
      ReparseNeeded tokens ->
        reparseWith reparseFunctionDecl tokens withoutReparse withReparse
  where
    withoutReparse :: M (C.Decl HandleMacros)
    withoutReparse = return C.Decl{
          declInfo = info
        , declKind = C.DeclFunction C.Function{
              functionArgs = map coercePass functionArgs
            , functionRes = coercePass functionRes
            , functionAnn = NoAnn
            , ..
            }
        , declAnn = NoAnn
        }

    withReparse ::
         (([C.Type HandleMacros], C.Type HandleMacros), CName)
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
      stateErrors     :: [MacroError]  -- ^ Stored in reverse order
    , stateMacroTypes :: MacroTypes
    , stateTypedefs   :: Set CName
    }

initMacroState :: MacroState
initMacroState = MacroState{
      stateErrors     = []
    , stateMacroTypes = Map.empty
    , stateTypedefs   = Set.empty
    }

macroTypeEnv :: MacroState -> Macro.TypeEnv
macroTypeEnv MacroState{stateMacroTypes, stateTypedefs} = Macro.TypeEnv{
      typeEnvMacros   = stateMacroTypes
    , typeEnvTypedefs = stateTypedefs
    }

runM :: M a -> (a, [MacroError])
runM = fmap stateErrors . flip runState initMacroState . unwrapM

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- TODO: We might want source location information here
data MacroError =
    -- | We could not parse the macro
    MacroErrorReparse ReparseError

    -- | We could not type-check the macro
  | MacroErrorTc TcMacroError

    -- | Unsupported macro: empty body
  | MacroErrorEmpty

    -- | Unsupported macro: defines C compiler attribute
  | MacroErrorAttribute

    -- | Macro that defines an unsupported type
  | MacroErrorUnsupportedType String
  deriving stock (Show, Eq)

instance PrettyTrace MacroError where
  prettyTrace = \case
      MacroErrorReparse x ->
        prettyTrace x
      MacroErrorTc x ->
        Text.unpack $ Macro.pprTcMacroError x
      MacroErrorEmpty ->
        "Unsupported empty macro"
      MacroErrorAttribute ->
        "Unsupported attribute macro"
      MacroErrorUnsupportedType err ->
        "Unsupported type: " ++ err

-- | Default log level
--
-- We use 'Info' for macros that are /always/ unsupported, and 'Warning' for
-- macros that we might perhaps except to be supported but something went wrong.
instance HasDefaultLogLevel MacroError where
  getDefaultLogLevel = \case
    MacroErrorReparse{}         -> Warning
    MacroErrorTc{}              -> Warning
    MacroErrorEmpty{}           -> Info
    MacroErrorAttribute{}       -> Info
    MacroErrorUnsupportedType{} -> Info

{-------------------------------------------------------------------------------
  Internal auxiliary: convenience functions wrapping the macro infrastructure
-------------------------------------------------------------------------------}

type Reparse a =
     Macro.TypeEnv
  -> [Token TokenSpelling]
  -> Either MacroError a

-- | Run parser
--
-- Failing to parse macros results in warnings, but never irrecoverable errors;
-- we therefore always want a fallback.
reparseWith ::
     Reparse a              -- ^ Parser
  -> [Token TokenSpelling]  -- ^ Raw tokens
  -> M r                    -- ^ If parsing fails
  -> (a -> M r)             -- ^ If parsing succeeds
  -> M r
reparseWith p tokens onFailure onSuccess = state $ \st ->
    case p (macroTypeEnv st) tokens of
      Left e ->
        runState (unwrapM $ onFailure  ) $ st{stateErrors = e : stateErrors st}
      Right a ->
        runState (unwrapM $ onSuccess a) $ st

-- | Reparse macro
--
-- We also return the extension of the macro type environment.
reparseMacro :: Reparse (
    Macro.Quant (FunValue, Macro.Type Macro.Ty)
  , C.CheckedMacro HandleMacros
  )
reparseMacro typeEnv tokens = do
    Macro{macroName, macroArgs, macroBody} <- first MacroErrorReparse $
      Reparse.reparseWith (Reparse.reparseMacro typeEnv) tokens
    -- TODO: It's a bit strange that the macro type inference works for
    -- /all/ classes of macros, rather than just expressions.
    Vec.reifyList macroArgs $ \args -> do
      inf <- first MacroErrorTc $ Macro.tcMacro typeEnv macroName args macroBody
      case macroBody of
        ExpressionMacro body ->
          return (
               inf
             , C.MacroExpr C.CheckedMacroExpr{
                   macroExprArgs = macroArgs
                 , macroExprBody = body
                 , macroExprType = dropEval inf
                 }
             )
        TypeMacro typeName ->
          case Reparse.typeNameType typeName of
            Right typ ->
              return (inf, C.MacroType $ C.CheckedMacroType typ NoAnn)
            Left err ->
              Left (MacroErrorUnsupportedType err)
        EmptyMacro ->
          Left MacroErrorEmpty
        AttributeMacro _ ->
          Left MacroErrorAttribute
 where
   dropEval ::
        Macro.Quant (Macro.FunValue, Macro.Type 'Macro.Ty)
     -> Macro.Quant (Macro.Type 'Macro.Ty)
   dropEval = fmap snd

reparseTypedef :: Reparse (C.Type HandleMacros)
reparseTypedef typeEnv tokens =
    first MacroErrorReparse $
      Reparse.reparseWith (Reparse.reparseTypedef typeEnv) tokens

reparseField :: Reparse (C.Type HandleMacros, CName)
reparseField typeEnv tokens =
    first MacroErrorReparse $
      Reparse.reparseWith (Reparse.reparseFieldDecl typeEnv) tokens

reparseFunctionDecl :: Reparse (
    ([C.Type HandleMacros], C.Type HandleMacros)
  , CName
  )
reparseFunctionDecl typeEnv tokens =
    first MacroErrorReparse $
      Reparse.reparseWith (Reparse.reparseFunDecl typeEnv) tokens
