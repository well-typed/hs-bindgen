module HsBindgen.Frontend.Pass.HandleMacros (
    module HsBindgen.Frontend.Pass.HandleMacros.IsPass
  , handleMacros
  , MacroError(..)
  ) where

import Control.Monad.State
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Vec.Lazy qualified as Vec

import HsBindgen.C.AST qualified as Old
import HsBindgen.C.Reparse (ReparseError)
import HsBindgen.C.Reparse qualified as Reparse
import HsBindgen.C.Tc.Macro (TcMacroError)
import HsBindgen.C.Tc.Macro qualified as Macro
import HsBindgen.C.Tc.Macro.Type (MacroTypes)
import HsBindgen.C.Tc.Macro.Type qualified as Macro
import HsBindgen.Errors
import HsBindgen.Frontend.Adapter
import HsBindgen.Frontend.AST
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.Parse
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Parse and typecheck macros, and reparse declarations
--
-- The macro parser needs to know which things are in scope (other macros as
-- well as typedefs), so we must process declarations in the right order; that
-- is, 'handleMacros' must be done after sorting the declarations.
--
-- In principle it could run before or after renaming: macros can neither refer
-- to nor introduce new anonymous declarations, so the relative ordering of
-- these two passes does not really matter. However, as part of renaming we
-- replace typedefs around anonymous structs by named structs:
--
-- > typedef struct { .. fields .. } foo;
--
-- On the C side however @foo@ must be referred to as @foo@, not @struct foo@;
-- to avoid confusion, it is therefore cleaner to run macro parsing and
-- declaration reparsing /prior/ to this transformation.
--
-- TODO: We should remove 'MacroError' from the AST.
-- TODO: We are not using the fallback if reparsing fails
handleMacros :: TranslationUnit Parse -> (TranslationUnit HandleMacros, [MacroError])
handleMacros TranslationUnit{unitDecls, unitIncludeGraph, unitAnn} =
    first reassemble $ runM . fmap catMaybes $ mapM processDecl unitDecls
  where
    reassemble :: [Decl HandleMacros] -> TranslationUnit HandleMacros
    reassemble decls' = TranslationUnit{
          unitDecls = decls'
        , unitIncludeGraph
        , unitAnn
        }

processDecl :: Decl Parse -> M (Maybe (Decl HandleMacros))
processDecl Decl{declInfo = DeclInfo{declId, declLoc}, declKind} =
    case declKind of
      DeclMacro   macro       -> processMacro info' macro
      DeclTypedef typedef     -> processTypedef info' typedef
      DeclStruct  struct      -> Just <$> processStruct info' struct
      DeclStructOpaque        -> Just <$> processOpaqueWith DeclStructOpaque info'
      DeclUnion   union       -> Just <$> processUnion info' union
      DeclUnionOpaque         -> Just <$> processOpaqueWith DeclUnionOpaque info'
      DeclEnum    enumerators -> Just <$> processEnum info' enumerators
      DeclEnumOpaque          -> Just <$> processOpaqueWith DeclEnumOpaque info'
      DeclFunction fun        -> processFunction info' fun
  where
    info' :: DeclInfo HandleMacros
    info' = DeclInfo{declId, declLoc}

{-------------------------------------------------------------------------------
  Function for each kind of declaration
-------------------------------------------------------------------------------}

processStruct :: DeclInfo HandleMacros -> Struct Parse -> M (Decl HandleMacros)
processStruct info =
    fmap (mkDecl . catMaybes) . mapM processStructField . structFields
  where
    mkDecl :: [StructField HandleMacros] -> Decl HandleMacros
    mkDecl fields = Decl{
          declInfo = info
        , declKind = DeclStruct Struct{
              structFields = fields
            }
        , declAnn  = NoAnn
        }

processStructField :: StructField Parse -> M (Maybe (StructField HandleMacros))
processStructField StructField{..} =
    case structFieldAnn of
      ReparseNotNeeded ->
        pure . Just $ StructField{
            structFieldName
          , structFieldType = processType structFieldType
          , structFieldOffset
          , structFieldAnn  = NoAnn
          }
      ReparseNeeded tokens ->
        reparseWith reparseField tokens $ \(ty, Old.CName name) ->
          pure . Just $ StructField{
              structFieldName = name
            , structFieldType = toRaw ty
            , structFieldOffset
            , structFieldAnn  = NoAnn
            }

processUnion :: DeclInfo HandleMacros -> Union Parse -> M (Decl HandleMacros)
processUnion info =
    fmap (combineFields . catMaybes) . mapM processUnionField . unionFields
  where
    combineFields :: [UnionField HandleMacros] -> Decl HandleMacros
    combineFields fields = Decl{
          declInfo = info
        , declKind = DeclUnion Union{
              unionFields = fields
            }
        , declAnn  = NoAnn
        }

processUnionField :: UnionField Parse -> M (Maybe (UnionField HandleMacros))
processUnionField UnionField{..} =
    case unionFieldAnn of
      ReparseNotNeeded ->
        pure . Just $ UnionField{
            unionFieldName
          , unionFieldType = processType unionFieldType
          , unionFieldAnn  = NoAnn
          }
      ReparseNeeded tokens ->
        reparseWith reparseField tokens $ \(ty, Old.CName name) ->
          pure . Just $ UnionField{
              unionFieldName = name
            , unionFieldType = toRaw ty
            , unionFieldAnn  = NoAnn
            }

processOpaqueWith ::
     DeclKind HandleMacros
  -> DeclInfo HandleMacros
  -> M (Decl HandleMacros)
processOpaqueWith kind info =
    return Decl{
        declInfo = info
      , declKind = kind
      , declAnn  = NoAnn
      }

processEnum :: DeclInfo HandleMacros -> [EnumConstant] -> M (Decl HandleMacros)
processEnum info = fmap (mkDecl . catMaybes) . mapM processEnumConstant
  where
    mkDecl :: [EnumConstant] -> Decl HandleMacros
    mkDecl enumerators = Decl{
          declInfo = info
        , declKind = DeclEnum enumerators
        , declAnn  = NoAnn
        }

processEnumConstant :: EnumConstant -> M (Maybe EnumConstant)
processEnumConstant = pure . Just

processTypedef ::
     DeclInfo HandleMacros
  -> Typedef Parse -> M (Maybe (Decl HandleMacros))
processTypedef info Typedef{typedefType, typedefAnn} = do
    modify $ \st -> st{
        stateTypedefs = Set.insert name (stateTypedefs st)
      }
    case typedefAnn of
      ReparseNotNeeded -> do
        let decl :: Decl HandleMacros
            decl = Decl{
                declInfo = info
              , declKind = DeclTypedef Typedef{
                    typedefType = processType typedefType
                  , typedefAnn  = NoAnn
                  }
              , declAnn  = NoAnn
              }
        return $ Just decl
      ReparseNeeded tokens ->
        reparseWith reparseTypedef tokens $ \ty -> do
          let decl :: Decl HandleMacros
              decl = Decl{
                  declInfo = info
                , declKind = DeclTypedef Typedef{
                      typedefType = toRaw ty
                    , typedefAnn  = NoAnn
                    }
                , declAnn  = NoAnn
                }
          return $ Just decl
  where
    name :: Old.CName
    name = case declId info of
             DeclNamed n -> Old.CName n
             _otherwise  -> panicPure "unexpected anonymous typedef"

processMacro ::
     DeclInfo HandleMacros
  -> UnparsedMacro -> M (Maybe (Decl HandleMacros))
processMacro info (UnparsedMacro tokens) =
    reparseWith reparseMacro tokens $ \(parsed ,ty) -> do
      modify $ \st -> st{
          stateMacroTypes = Map.insert name ty (stateMacroTypes st)
        }
      let decl :: Decl HandleMacros
          decl = Decl{
              declInfo = info
            , declKind = DeclMacro CheckedMacro{
                  checkedMacro     = parsed
                , checkedMacroType = dropEval ty
                }
            , declAnn  = NoAnn
            }
      return $ Just decl
  where
    name :: Old.CName
    name = case declId info of
             DeclNamed n -> Old.CName n
             _otherwise  -> panicPure "unexpected anonymous macro"

processFunction ::
     DeclInfo HandleMacros
  -> Function Parse -> M (Maybe (Decl HandleMacros))
processFunction info Function {..} =
  case functionAnn of
    ReparseNotNeeded -> do
      let decl :: Decl HandleMacros
          decl = Decl{
              declInfo = info
            , declKind = DeclFunction Function{
                functionName
              , functionArgs = map processType functionArgs
              , functionRes = processType functionRes
              , functionAnn = NoAnn
              }
            , declAnn = NoAnn
            }
      pure $ Just decl
    ReparseNeeded tokens ->
      reparseWith reparseFunctionDecl tokens $ \((tys, ty), (Old.CName name)) -> do
        let decl :: Decl HandleMacros
            decl = Decl{
                declInfo = info
              , declKind = DeclFunction Function{
                  functionName = name
                , functionArgs = map toRaw tys
                , functionRes = toRaw ty
                , functionAnn = NoAnn
                }
              , declAnn = NoAnn
              }
        pure $ Just decl

processType :: Type Parse -> Type HandleMacros
processType = \case
    TypePrim    prim    -> TypePrim prim
    TypeStruct  uid     -> TypeStruct uid
    TypeUnion   uid     -> TypeUnion uid
    TypeEnum    uid     -> TypeEnum uid
    TypeTypedef uid ann -> TypeTypedef uid ann
    TypePointer ty      -> TypePointer (processType ty)
    TypeFunction tys ty -> TypeFunction (map processType tys) (processType ty)
    TypeVoid            -> TypeVoid

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

-- TODO: We might want source location information here
data MacroError =
    ReparseError ReparseError
  | TcMacroError TcMacroError
  deriving stock (Show)

data MacroState = MacroState {
      stateErrors     :: [MacroError]  -- ^ Stored in reverse order
    , stateMacroTypes :: MacroTypes
    , stateTypedefs   :: Set Old.CName
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
    , typeEnvTypedefs = fromTypedefs stateTypedefs
    }
  where
    fromTypedefs :: Set Old.CName -> Map Old.CName Macro.TypedefUnderlyingType
    fromTypedefs = Map.fromList . map (, underlying) . Set.toList

    -- TODO: This should go, the macro parser should not need this info.
    -- (the other options are 'AnonStructTypedef' and 'AnonEnumTypedef', but
    -- it really should not matter.)
    underlying :: Macro.TypedefUnderlyingType
    underlying = Macro.NormalTypedef

runM :: M a -> (a, [MacroError])
runM = fmap stateErrors . flip runState initMacroState . unwrapM

{-------------------------------------------------------------------------------
  Internal auxiliary: convenience functions wrapping the macro infrastructure
-------------------------------------------------------------------------------}

type Reparse a =
     Macro.TypeEnv
  -> [Old.Token Old.TokenSpelling]
  -> Either MacroError a

reparseWith ::
     Reparse a
  -> [Old.Token Old.TokenSpelling]
  -> (a -> M (Maybe b))
  -> M (Maybe b)
reparseWith p tokens k = state $ \st ->
    case p (macroTypeEnv st) tokens of
      Left  e -> (Nothing, st{stateErrors = e : stateErrors st})
      Right a -> runState (unwrapM $ k a) st

reparseMacro :: Reparse (
      Old.Macro Macro.Ps
    , Macro.Quant (Macro.FunValue, Macro.Type Macro.Ty)
    )
reparseMacro typeEnv tokens =
    case Reparse.reparseWith (Reparse.reparseMacro typeEnv) tokens of
      Left err -> Left (ReparseError err)
      Right macro@Old.Macro{macroName, macroArgs, macroBody} ->
        Vec.reifyList macroArgs $ \args ->
          case Macro.tcMacro typeEnv macroName args macroBody of
            Left  err -> Left (TcMacroError err)
            Right ty  -> Right (macro, ty)

dropEval ::
     Macro.Quant (Macro.FunValue, Macro.Type 'Macro.Ty)
  -> Macro.Quant (Macro.Type 'Macro.Ty)
dropEval = fmap snd

reparseTypedef :: Reparse Old.Type
reparseTypedef typeEnv tokens =
    first ReparseError $
      Reparse.reparseWith (Reparse.reparseTypedef typeEnv) tokens

reparseField :: Reparse (Old.Type, Old.CName)
reparseField typeEnv tokens =
    first ReparseError $
      Reparse.reparseWith (Reparse.reparseFieldDecl typeEnv) tokens

reparseFunctionDecl :: Reparse (([Old.Type], Old.Type), Old.CName)
reparseFunctionDecl typeEnv tokens =
    first ReparseError $
      Reparse.reparseWith (Reparse.reparseFunDecl typeEnv) tokens
