module HsBindgen.Frontend.Pass.TypecheckMacros (
    typecheckMacros
  ) where

import Control.Applicative
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (MonadReader (..), Reader, runReader)
import Control.Monad.State (MonadState (..), StateT, modify, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Either (partitionEithers)
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map

import C.Expr.Syntax qualified as CExpr
import C.Expr.Typecheck qualified as CExpr
import C.Expr.Typecheck.Interface.Type qualified as T
import C.Expr.Typecheck.Type qualified as CExpr

import Clang.HighLevel.Types

import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.LanguageC qualified as LanC
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.Error
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

type CType = C.Type TypecheckMacros

-- | We perform two traversals:
--
-- 1. Collect known types
--
-- 2. Typecheck macros
--
-- Returns the updated translation unit together with the 'LanC.ReparseEnv'
-- that 'reparseMacroExpansions' uses to reparse declarations.
--
-- Register macro typecheck failures in @DeclMeta@.
typecheckMacros ::
     C.TranslationUnit ConstructTranslationUnit
  -> ( C.TranslationUnit TypecheckMacros
     , LanC.ReparseEnv
     , LanC.ReparseEnv
     )
typecheckMacros unit =
    let typedefTypes :: Map Text CType
        taggedTypes  :: Map CDeclName CType
        (typedefTypes, taggedTypes) =
          bimap Map.fromList Map.fromList $
            partitionEithers $
              mapMaybe getKnownType unit.decls
        ((failedMacros, typecheckedDecls), typecheckState) =
          runM typedefTypes taggedTypes $
            fmap partitionEithers $ mapM typecheckDecl unit.decls
    in  ( reconstructAfterTypecheck unit failedMacros typecheckedDecls
        , Map.map coercePass $
            typedefTypes <> Map.mapKeys renderCDeclNameC taggedTypes
        , Map.map coercePass $ typecheckState.resolvedMacroTypes )

{-------------------------------------------------------------------------------
  Reconstruct translation unit after typechecking
-------------------------------------------------------------------------------}

type FailedMacro = (DeclId, SingleLoc, MacroTypecheckError)

reconstructAfterTypecheck ::
     C.TranslationUnit ConstructTranslationUnit
  -> [FailedMacro]
  -> [C.Decl TypecheckMacros]
  -> C.TranslationUnit TypecheckMacros
reconstructAfterTypecheck unit failedMacros decls =
    C.TranslationUnit{
        decls        = decls
      , includeGraph = unit.includeGraph
      , ann          = unit.ann{
            declIndex = updatedDeclIndex
          }
      }
  where
    updatedDeclIndex :: DeclIndex
    updatedDeclIndex =
      Foldable.foldl'
        (flip DeclIndex.registerMacroTypecheckFailure)
        unit.ann.declIndex
        failedMacros

{-------------------------------------------------------------------------------
  Traversal 1: Underlying types
-------------------------------------------------------------------------------}

-- | Typedef name → CType (Left) or tagged name → CType (Right).
getKnownType ::
     C.Decl ConstructTranslationUnit
  -> Maybe (Either (Text, CType) (CDeclName, CType))
getKnownType decl = case decl.kind of
    C.DeclMacro{}            -> Nothing
    C.DeclTypedef typedef    -> Just $ Left  (typedefName, getKnownTypedef typedef)
    C.DeclStruct{}           -> Just $ Right (taggedName,  knownStructOrUnion)
    C.DeclUnion{}            -> Just $ Right (taggedName,  knownStructOrUnion)
    C.DeclEnum enum          -> Just $ Right (taggedName,  getKnownEnum enum)
    C.DeclAnonEnumConstant{} -> Nothing
    -- Include opaque tagged types: they can be referenced by macros (e.g. as a
    -- pointer type). The only opaque type-like declarations carrying an
    -- ordinary names are @typedef void foo@, which is not a valid standalone
    -- type, so macros using them will fail anyway.
    C.DeclOpaque{}           -> Right <$> getKnownTypeOpaque
    C.DeclFunction{}         -> Nothing
    C.DeclGlobal{}           -> Nothing
  where
    info :: C.DeclInfo ConstructTranslationUnit
    info = decl.info

    getKnownTypeOpaque :: Maybe (CDeclName, CType)
    getKnownTypeOpaque =
      case info.id.name.kind of
        CNameKindTagged _ -> Just (taggedName, knownStructOrUnion)
        _                 -> Nothing

    -- | Typedef names are plain text (always 'CNameKindOrdinary').
    typedefName :: Text
    typedefName = info.id.name.text

    -- | Tagged names carry their tag kind ('CNameKindTagged').
    taggedName :: CDeclName
    taggedName = info.id.name

    getKnownTypedef :: C.Typedef ConstructTranslationUnit -> CType
    getKnownTypedef typedef = C.TypeTypedef $ C.Ref info.id $ coercePass typedef.typ

    knownStructOrUnion :: CType
    knownStructOrUnion = C.TypeRef info.id

    getKnownEnum :: C.Enum ConstructTranslationUnit -> CType
    getKnownEnum enum = C.TypeEnum $ C.Ref info.id $ coercePass enum.typ

{-------------------------------------------------------------------------------
  Traversal 2: Typecheck macros
-------------------------------------------------------------------------------}

typecheckDecl ::
     C.Decl ConstructTranslationUnit
  -> M (Either FailedMacro (C.Decl TypecheckMacros))
typecheckDecl decl = case decl.kind of
    C.DeclMacro        macro -> typecheckMacro info' macro
    C.DeclTypedef          k -> withCoercedKind $ C.DeclTypedef          $ coercePass k
    C.DeclStruct           k -> withCoercedKind $ C.DeclStruct           $ coercePass k
    C.DeclUnion            k -> withCoercedKind $ C.DeclUnion            $ coercePass k
    C.DeclEnum             k -> withCoercedKind $ C.DeclEnum             $ coercePass k
    C.DeclAnonEnumConstant k -> withCoercedKind $ C.DeclAnonEnumConstant $ coercePass k
    C.DeclOpaque             -> withCoercedKind $ C.DeclOpaque
    C.DeclFunction         k -> withCoercedKind $ C.DeclFunction         $ coercePass k
    C.DeclGlobal           k -> withCoercedKind $ C.DeclGlobal           $ coercePass k
  where
    info' :: C.DeclInfo TypecheckMacros
    info' = coercePass decl.info

    withCoercedKind ::
      C.DeclKind TypecheckMacros -> M (Either a (C.Decl TypecheckMacros))
    withCoercedKind kind' = pure $ Right $ C.Decl info' kind' decl.ann

typecheckMacro ::
     C.DeclInfo TypecheckMacros
  -> ParsedMacro
  -> M (Either FailedMacro (C.Decl TypecheckMacros))
typecheckMacro info parsedMacro = do
    result <- tcMacro info.id.name parsedMacro
    pure $ bimap addInfo toDecl result
  where
    addInfo :: MacroTypecheckError -> FailedMacro
    addInfo = (info.id, info.loc,)

    toDecl :: CheckedMacro TypecheckMacros -> C.Decl TypecheckMacros
    toDecl checked = C.Decl{
          info = info
        , kind = C.DeclMacro checked
        , ann  = NoAnn
        }

{-------------------------------------------------------------------------------
  Internal: monad used for type checking macros
-------------------------------------------------------------------------------}

newtype M a = WrapM { _unwrapM :: StateT TypecheckState (Reader TypecheckEnv) a }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadReader TypecheckEnv
    , MonadState  TypecheckState
    )

data TypecheckEnv = TypecheckEnv {
      -- | Known @typedef@ types from the first traversal, keyed by bare name.
      knownTypedefs :: Map Text CType
      -- | Known @struct@, @union@, and @enum@ types from the first traversal,
      --   keyed by 'CDeclName' (which carries the tag kind).
    , knownTaggedTypes :: Map CDeclName CType
    }

data TypecheckState = TypecheckState {
      -- | The c-expr-dsl type environment, accumulating quantified types
      --   for each macro as we typecheck them.
      typeEnv :: CExpr.TypeEnv
      -- | Resolved macro C types, keyed by bare name. We need these when
      --   injecting type names (for macros that reference other macro-defined
      --   types) and for reparsing declarations with macro expansions in the
      --   next pass.
    , resolvedMacroTypes :: Map Text CType
    }
  deriving stock (Generic)

runM ::
     Map Text CType
  -> Map CDeclName CType
  -> M a
  -> (a, TypecheckState)
runM knownTypedefs knownTaggedTypes (WrapM ma) = runReader (runStateT ma s) e
  where
    e :: TypecheckEnv
    e = TypecheckEnv{
      knownTypedefs    = knownTypedefs
    , knownTaggedTypes = knownTaggedTypes
    }

    s :: TypecheckState
    s = TypecheckState{
      typeEnv = CExpr.buildTypedefEnv
                  [ CExpr.Name nm | nm <- Map.keys knownTypedefs ]
    , resolvedMacroTypes = Map.empty
    }

-- | Look up a @typedef@ or macro-defined type by bare name.
--
-- Searches both the known @typedef@s (from the first traversal) and the
-- resolved macro types (accumulated during typechecking).
--
-- 'panicPure' is safe here: c-expr-dsl only calls the inject function for names
-- present in 'CExpr.TypeEnv', which we built from these same maps. That is, it
-- is a bug, if we fail to lookup an ordinary type, because the typechecker
-- should have failed gracefully at an earlier stage.
lookupType :: Text -> M CType
lookupType n = do
    st  <- get
    env <- ask
    case Map.lookup n env.knownTypedefs <|> Map.lookup n st.resolvedMacroTypes of
      Just t  -> pure t
      Nothing -> panicPure $
        "lookupTypedefType: name not found: " <> show n

-- | Look up a tagged type (struct, union, enum) by 'CDeclName'.
--
-- Unlike 'lookupType', there is no invariant guaranteeing presence: tagged
-- types are resolved from syntax alone and are not gated by 'CExpr.TypeEnv'.
-- A macro may reference a tagged type we failed to parse or that was defined
-- in an unprocessed header, so we return a proper error rather than panic.
lookupTaggedType :: CDeclName -> ExceptT MacroTypecheckError M CType
lookupTaggedType key = do
    env <- ask
    case Map.lookup key env.knownTaggedTypes of
      Just t  -> pure t
      Nothing -> throwError $ MacroTypecheckErrorUnresolvedTaggedType key

{-------------------------------------------------------------------------------
  Type checking
-------------------------------------------------------------------------------}

-- | Type check macro.
--
-- Delegates dispatch between type and expression bodies to 'CExpr.tcMacro'.
--
-- The c-expr-dsl typechecker uses two different variable types:
--
-- * For type expressions (@tvar = 'CType'@): the inject functions resolve
--   names to C types immediately, so 'convertTExpr' is pure.
--
-- * For value expressions (@vvar = 'Id' 'TypecheckMacros'@): the inject
--   function creates 'DeclId' values for dependency tracking.
tcMacro ::
     CDeclName
  -> ParsedMacro
  -> M (Either MacroTypecheckError (CheckedMacro TypecheckMacros))
tcMacro name (ParsedMacro macro) = do
    st    <- get
    eTcRes <- runExceptT $ CExpr.tcMacro
               st.typeEnv
               (lift . injectType)
               injectTaggedType
               (lift . injectValueName)
               macro.macroName
               macro.macroArgs
               macro.macroExpr
    case eTcRes of
      Left err    -> pure $ Left err
      Right tcRes -> case tcRes of
        Left err ->
          pure $ Left $ MacroTypecheckErrorCExpr err
        Right (CExpr.MacroTcTypeExpr quant typeExpr) ->
          case convertTExpr typeExpr of
            Left err  -> pure $ Left err
            Right typ -> do
              addQuant quant
              addNewMacroTypeToReparseEnv typ
              pure $ Right $ MacroType $ CheckedMacroType typ NoAnn
        Right (CExpr.MacroTcValueExpr inf valueExpr) -> do
          modify $ #typeEnv %~ Map.insert macro.macroName inf
          pure $ Right $ MacroExpr $ CheckedMacroExpr{
                args = macro.macroArgs
              , body = valueExpr
              , typ  = fmap snd inf
              }
  where
    -- Type names: resolve to 'CType' via lookup in known typedefs and
    -- previously resolved macro types.
    injectType :: CExpr.Name -> M CType
    injectType n = lookupType n.getName

    -- Tagged type names: resolve to 'CType' via lookup in known tagged types.
    -- Uses 'ExceptT' so that unknown tagged types produce a proper error
    -- rather than a panic; see 'lookupTaggedType'.
    injectTaggedType ::
         CExpr.TagKind
      -> CExpr.Name
      -> ExceptT MacroTypecheckError M CType
    injectTaggedType tag n =
        lookupTaggedType CDeclName{
            text = n.getName
          , kind = CNameKindTagged (convertTagKind tag)
          }

    -- Value names: create a 'DeclId' for dependency tracking.
    injectValueName :: CExpr.Name -> M (Id TypecheckMacros)
    injectValueName (CExpr.Name n) = pure $ DeclId{
        name   = CDeclName{ text = n, kind = CNameKindMacro }
      , isAnon = False
      }

    addQuant :: CExpr.Quant (CExpr.FunValue, CExpr.Type CExpr.Ty) -> M ()
    addQuant quant =
        modify $ #typeEnv %~ Map.insert macro.macroName quant

    addNewMacroTypeToReparseEnv :: CType -> M ()
    addNewMacroTypeToReparseEnv typ
      -- stdbool.h defines @#define bool _Bool@, which is just an alias for
      -- the primitive boolean type. We normalise this away: if @bool@ maps
      -- to @PrimBool@, we store @TypePrim PrimBool@ directly instead of
      -- wrapping it in @TypeMacro@. This ensures that @bool@ from stdbool.h
      -- renders identically to @_Bool@, regardless of language-c version.
      -- Genuine redefinitions like @#define bool int@ are not affected
      -- because their underlying type is not @PrimBool@.
      | name.text == "bool"
      , C.TypePrim C.PrimBool <- typ
      = addMacroType name.text typ
      | otherwise
      = addMacroType name.text $
          C.TypeMacro $ C.Ref {
              name = DeclId{name = name, isAnon = False}
            , underlying = typ
            }

    addMacroType :: Text -> CType -> M ()
    addMacroType n typ =
        modify $ #resolvedMacroTypes %~ Map.insert n typ

{-------------------------------------------------------------------------------
  Convert T.Expr to C.Type
-------------------------------------------------------------------------------}

-- | Convert a typechecked macro type expression ('T.Expr') to a C type.
--
-- This is a pure function: all named types have already been resolved to
-- 'CType' values by the inject functions passed to 'CExpr.tcMacro'.
--
-- Correctly handles chains of @const@ and pointer modifiers (e.g.
-- @const int *@, @int * const@, @const int * const *@).
--
-- Rejects bare @void@ at the top level (including @const void@), but allows
-- @void@ as the pointee of a pointer (e.g. @void *@, @const void *@).
convertTExpr :: T.Expr CType -> Either MacroTypecheckError CType
convertTExpr expr = go expr >>= checkTopLevelVoid
  where
    go :: T.Expr CType -> Either MacroTypecheckError CType
    go = \case
      T.App T.Pointer inner -> C.TypePointers 1 <$> go inner
      T.App T.Const   inner -> C.TypeQual C.QualConst <$> go inner
      T.TypeLit t           -> Right $ convertLiteral t
      T.LocalArg nm         -> Left $ MacroTypecheckErrorTypeArgInType nm.getName
      T.Var ctype           -> Right ctype

    -- | @void@ is not a valid standalone type; it is only valid as the
    -- pointee of a pointer (e.g. @void *@).
    checkTopLevelVoid :: CType -> Either MacroTypecheckError CType
    checkTopLevelVoid = \case
      C.TypeVoid              -> Left MacroTypecheckErrorVoidType
      C.TypeQual _ C.TypeVoid -> Left MacroTypecheckErrorVoidType
      ty                      -> Right ty

-- | Convert a primitive type literal to a C type.
convertLiteral :: CExpr.TypeLit -> CType
convertLiteral = \case
    CExpr.TypeInt sign size ->
      C.TypePrim $ C.PrimIntegral (convertIntSize size) (convertSign sign)
    CExpr.TypeChar sign ->
      C.TypePrim $ C.PrimChar (convertCharSign sign)
    CExpr.TypeFloat size ->
      C.TypePrim $ C.PrimFloating (convertFloatSize size)
    CExpr.TypeVoid ->
      C.TypeVoid
    CExpr.TypeBool ->
      C.TypePrim C.PrimBool

convertSign :: Maybe CExpr.Sign -> C.PrimSign
convertSign = \case
    Nothing                  -> C.Signed
    Just CExpr.Signed   -> C.Signed
    Just CExpr.Unsigned -> C.Unsigned

convertCharSign :: Maybe CExpr.Sign -> C.PrimSignChar
convertCharSign = \case
    Nothing                  -> C.PrimSignImplicit Nothing
    Just CExpr.Signed   -> C.PrimSignExplicit C.Signed
    Just CExpr.Unsigned -> C.PrimSignExplicit C.Unsigned

convertIntSize :: Maybe CExpr.IntSize -> C.PrimIntType
convertIntSize = \case
    Nothing                      -> C.PrimInt
    Just CExpr.SizeShort    -> C.PrimShort
    Just CExpr.SizeInt      -> C.PrimInt
    Just CExpr.SizeLong     -> C.PrimLong
    Just CExpr.SizeLongLong -> C.PrimLongLong

convertFloatSize :: CExpr.FloatSize -> C.PrimFloatType
convertFloatSize = \case
    CExpr.SizeFloat  -> C.PrimFloat
    CExpr.SizeDouble -> C.PrimDouble
