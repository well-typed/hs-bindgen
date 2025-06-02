module HsBindgen.Frontend.Pass.HandleMacros (
    module HsBindgen.Frontend.Pass.HandleMacros.IsPass
  , handleMacros
  , MacroError(..)
  ) where

import Control.Monad.State
import Data.Map qualified as Map
import Data.Set qualified as Set
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
import HsBindgen.Frontend.AST.Internal (CName)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Macros.AST.Syntax
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
-- TODO: We are not using the fallback if reparsing fails
handleMacros ::
      C.TranslationUnit Parse
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

processDecl :: C.Decl Parse -> M (Maybe (C.Decl HandleMacros))
processDecl C.Decl{declInfo = C.DeclInfo{declId, declLoc}, declKind} =
    case declKind of
      C.DeclMacro   macro   -> processMacro info' macro
      C.DeclTypedef typedef -> processTypedef info' typedef
      C.DeclStruct  struct  -> Just <$> processStruct info' struct
      C.DeclStructOpaque    -> Just <$> processOpaque C.DeclStructOpaque info'
      C.DeclUnion   union   -> Just <$> processUnion info' union
      C.DeclEnum    enum    -> Just <$> processEnum info' enum
      C.DeclEnumOpaque      -> Just <$> processOpaque C.DeclEnumOpaque info'
      C.DeclFunction fun    -> processFunction info' fun
  where
    info' :: C.DeclInfo HandleMacros
    info' = C.DeclInfo{declId, declLoc}

{-------------------------------------------------------------------------------
  Function for each kind of declaration
-------------------------------------------------------------------------------}

processStruct ::
     C.DeclInfo HandleMacros
  -> C.Struct Parse
  -> M (C.Decl HandleMacros)
processStruct info C.Struct{..} =
    mkDecl . catMaybes <$> mapM processStructField structFields
  where
    mkDecl :: [C.StructField HandleMacros] -> C.Decl HandleMacros
    mkDecl fields = C.Decl{
          declInfo = info
        , declKind = C.DeclStruct C.Struct{structFields = fields, ..}
        , declAnn  = NoAnn
        }

processStructField ::
     C.StructField Parse
  -> M (Maybe (C.StructField HandleMacros))
processStructField C.StructField{..} =
    case structFieldAnn of
      ReparseNotNeeded ->
        pure . Just $ C.StructField{
            structFieldType = processType structFieldType
          , structFieldAnn  = NoAnn
          , ..
          }
      ReparseNeeded tokens ->
        reparseWith reparseField tokens $ \(ty, name) ->
          pure . Just $ C.StructField{
              structFieldName = name
            , structFieldType = ty
            , structFieldAnn  = NoAnn
            , ..
            }

processUnion ::
     C.DeclInfo HandleMacros
  -> C.Union Parse
  -> M (C.Decl HandleMacros)
processUnion info C.Union{..} =
    combineFields . catMaybes <$> mapM processUnionField unionFields
  where
    combineFields :: [C.UnionField HandleMacros] -> C.Decl HandleMacros
    combineFields fields = C.Decl{
          declInfo = info
        , declKind = C.DeclUnion C.Union{unionFields = fields, ..}
        , declAnn  = NoAnn
        }

processUnionField :: C.UnionField Parse -> M (Maybe (C.UnionField HandleMacros))
processUnionField C.UnionField{..} =
    case unionFieldAnn of
      ReparseNotNeeded ->
        pure . Just $ C.UnionField{
            unionFieldType = processType unionFieldType
          , unionFieldAnn  = NoAnn
           ,..
          }
      ReparseNeeded tokens ->
        reparseWith reparseField tokens $ \(ty, name) ->
          pure . Just $ C.UnionField{
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
  -> C.Enum Parse
  -> M (C.Decl HandleMacros)
processEnum info C.Enum{..} =
    mkDecl <$> mapM processEnumConstant enumConstants
  where
    mkDecl :: [C.EnumConstant HandleMacros] -> C.Decl HandleMacros
    mkDecl enumerators = C.Decl{
          declInfo = info
        , declKind = C.DeclEnum C.Enum{
                          enumType      = processType enumType
                        , enumConstants = enumerators
                        , ..
                        }
        , declAnn  = NoAnn
        }

processEnumConstant ::
     C.EnumConstant Parse
  -> M (C.EnumConstant HandleMacros)
processEnumConstant C.EnumConstant{..} = return C.EnumConstant{..}

processTypedef ::
     C.DeclInfo HandleMacros
  -> C.Typedef Parse
  -> M (Maybe (C.Decl HandleMacros))
processTypedef info C.Typedef{typedefType, typedefAnn} = do
    modify $ \st -> st{
        stateTypedefs = Set.insert name (stateTypedefs st)
      }
    case typedefAnn of
      ReparseNotNeeded -> do
        let decl :: C.Decl HandleMacros
            decl = C.Decl{
                declInfo = info
              , declKind = C.DeclTypedef C.Typedef{
                    typedefType = processType typedefType
                  , typedefAnn  = NoAnn
                  }
              , declAnn  = NoAnn
              }
        return $ Just decl
      ReparseNeeded tokens ->
        reparseWith reparseTypedef tokens $ \ty -> do
          let decl :: C.Decl HandleMacros
              decl = C.Decl{
                  declInfo = info
                , declKind = C.DeclTypedef C.Typedef{
                      typedefType = ty
                    , typedefAnn  = NoAnn
                    }
                , declAnn  = NoAnn
                }
          return $ Just decl
  where
    name :: CName
    name = case C.declId info of
             DeclNamed n -> n
             _otherwise  -> panicPure "unexpected anonymous typedef"

processMacro ::
     C.DeclInfo HandleMacros
  -> UnparsedMacro -> M (Maybe (C.Decl HandleMacros))
processMacro info (UnparsedMacro tokens) =
    reparseWith reparseMacro tokens $ \(ty, checkedMacro) -> do
      modify $ \st -> st{
          stateMacroTypes = Map.insert name ty (stateMacroTypes st)
        }
      let decl :: C.Decl HandleMacros
          decl = C.Decl{
              declInfo = info
            , declKind = C.DeclMacro checkedMacro
            , declAnn  = NoAnn
            }
      return $ Just decl
  where
    name :: CName
    name = case C.declId info of
             DeclNamed n -> n
             _otherwise  -> panicPure "unexpected anonymous macro"

processFunction ::
     C.DeclInfo HandleMacros
  -> C.Function Parse
  -> M (Maybe (C.Decl HandleMacros))
processFunction info C.Function {..} =
  case functionAnn of
    ReparseNotNeeded -> do
      let decl :: C.Decl HandleMacros
          decl = C.Decl{
              declInfo = info
            , declKind = C.DeclFunction C.Function{
                  functionArgs = map processType functionArgs
                , functionRes = processType functionRes
                , functionAnn = NoAnn
                }
            , declAnn = NoAnn
            }
      pure $ Just decl
    ReparseNeeded tokens ->
      reparseWith reparseFunctionDecl tokens $ \((tys, ty), _name) -> do
        -- TODO: We should assert that the name is the name we were expecting
        let decl :: C.Decl HandleMacros
            decl = C.Decl{
                declInfo = info
              , declKind = C.DeclFunction C.Function{
                    functionArgs = tys
                  , functionRes  = ty
                  , functionAnn  = NoAnn
                  }
              , declAnn = NoAnn
              }
        pure $ Just decl

processType :: C.Type Parse -> C.Type HandleMacros
processType = \case
    C.TypePrim prim         -> C.TypePrim prim
    C.TypeStruct uid        -> C.TypeStruct uid
    C.TypeUnion uid         -> C.TypeUnion uid
    C.TypeEnum uid          -> C.TypeEnum uid
    C.TypeTypedef uid ann   -> C.TypeTypedef uid ann
    C.TypePointer ty        -> C.TypePointer (processType ty)
    C.TypeFun args res      -> C.TypeFun (map processType args) (processType res)
    C.TypeVoid              -> C.TypeVoid
    C.TypeExtBinding ref t  -> C.TypeExtBinding ref t
    C.TypeConstArray n t    -> C.TypeConstArray n (processType t)
    C.TypeIncompleteArray t -> C.TypeIncompleteArray (processType t)

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
  deriving stock (Show)

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
  Internal auxiliary: convenience functions wrapping the macro infrastructure
-------------------------------------------------------------------------------}

type Reparse a =
     Macro.TypeEnv
  -> [Token TokenSpelling]
  -> Either MacroError a

reparseWith ::
     Reparse a
  -> [Token TokenSpelling]
  -> (a -> M (Maybe b))
  -> M (Maybe b)
reparseWith p tokens k = state $ \st ->
    case p (macroTypeEnv st) tokens of
      Left  e -> (Nothing, st{stateErrors = e : stateErrors st})
      Right a -> runState (unwrapM $ k a) st

-- | Reparse macro
--
-- We also return the extension of the macro type environment.
reparseMacro :: Reparse (
    Macro.Quant (FunValue, Macro.Type Macro.Ty)
  , CheckedMacro HandleMacros
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
             , MacroExpr CheckedMacroExpr{
                   macroExprBody = body
                 , macroExprType = dropEval inf
                 }
             )
        TypeMacro typeName ->
          case Reparse.typeNameType typeName of
            Right typ -> return (inf, MacroType typ)
            Left  err -> Left (MacroErrorUnsupportedType err)
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
