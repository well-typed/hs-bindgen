module HsBindgen.Frontend.Pass.TypecheckMacros (
    typecheckMacros
  ) where

import Control.Monad.State (MonadState (..), State, modify, runState)
import Data.Either (partitionEithers)
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map

import C.Expr.Syntax qualified as CExpr.DSL
import C.Expr.Typecheck qualified as CExpr.DSL
import C.Expr.Typecheck.Interface.Type qualified as T
import C.Expr.Typecheck.Type qualified as CExpr.DSL

import Clang.HighLevel.Types

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
type CTypes = Map Text CType

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
    let typedefTypes, taggedTypes :: CTypes
        (typedefTypes, taggedTypes) =
          bimap Map.fromList Map.fromList $
            partitionEithers $
              mapMaybe getKnownType unit.decls
        ((failedMacros, typecheckedDecls), typecheckState) =
          runM (Map.keys typedefTypes) $
            fmap partitionEithers $ mapM typecheckDecl unit.decls
    in  ( reconstructAfterTypecheck unit failedMacros typecheckedDecls
        , Map.map coercePass (typedefTypes <> taggedTypes)
        , Map.map coercePass typecheckState.macroTypes
        )

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

type KnownType = (Text, CType)

getKnownType ::
  C.Decl ConstructTranslationUnit -> Maybe (Either KnownType KnownType)
getKnownType decl = case decl.kind of
    C.DeclMacro{}            -> Nothing
    C.DeclTypedef typedef    -> Just $ Left  $ (name, getKnownTypedef typedef)
    C.DeclStruct{}           -> Just $ Right $ (name, knownStructOrUnion)
    C.DeclUnion{ }           -> Just $ Right $ (name, knownStructOrUnion)
    C.DeclEnum enum          -> Just $ Right $ (name, getKnownEnum    enum)
    C.DeclAnonEnumConstant{} -> Nothing
    C.DeclOpaque{}           -> Nothing
    C.DeclFunction{}         -> Nothing
    C.DeclGlobal{}           -> Nothing
  where
    info :: C.DeclInfo ConstructTranslationUnit
    info = decl.info

    name :: Text
    name = renderCDeclNameC $ info.id.name

    getKnownTypedef :: C.Typedef ConstructTranslationUnit -> C.Type TypecheckMacros
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
    C.DeclTypedef          x -> withCoercedKind $ C.DeclTypedef          $ coercePass x
    C.DeclStruct           x -> withCoercedKind $ C.DeclStruct           $ coercePass x
    C.DeclUnion            x -> withCoercedKind $ C.DeclUnion            $ coercePass x
    C.DeclEnum             x -> withCoercedKind $ C.DeclEnum             $ coercePass x
    C.DeclAnonEnumConstant x -> withCoercedKind $ C.DeclAnonEnumConstant $ coercePass x
    C.DeclOpaque             -> withCoercedKind $ C.DeclOpaque
    C.DeclFunction         x -> withCoercedKind $ C.DeclFunction         $ coercePass x
    C.DeclGlobal           x -> withCoercedKind $ C.DeclGlobal           $ coercePass x
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

newtype M a = WrapM { _unwrapM :: State TypecheckState a }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadState  TypecheckState
    )

data TypecheckState = TypecheckState {
      -- | Quantified types of macro expressions.
      macroEnv   :: CExpr.DSL.TypeEnv
      -- | New macro types. We need these when reparsing declarations with macro
      -- expansions in the next pass.
    , macroTypes :: CTypes
    }
  deriving stock (Generic)

runM :: [Text] -> M a -> (a, TypecheckState)
runM knownTypedefs (WrapM ma) = runState ma s
  where
    s :: TypecheckState
    s = TypecheckState{
      macroEnv   = buildCTypeEnv knownTypedefs
    , macroTypes = Map.empty
    }

    -- Build a 'TypeEnv' fragment for all known @typedef@ types.
    --
    -- Note: @struct@, @union@, and @enum@ types are kept separately in
    -- 'knownTaggedTypes' and are looked up via 'getLookupKnownType' (through
    -- 'convertSpecifier'). They are not added to the c-expr-dsl 'TypeEnv'
    -- because tagged types always parse as 'CExpr.DSL.TypeTagged' literals,
    -- never as bare 'CExpr.DSL.Var' nodes.
    --
    -- Use a dummy 'FunValue' that is never invoked.
    buildCTypeEnv :: [Text] -> CExpr.DSL.TypeEnv
    buildCTypeEnv types =
        Map.fromList
          [ ( CExpr.DSL.Name nm
            , CExpr.DSL.Quant @Z $ \VNil ->
                CExpr.DSL.QuantTyBody []
                  ( CExpr.DSL.FunValue @Z nm (\VNil -> CExpr.DSL.NoValue)
                  , CExpr.DSL.MacroTypeTy
                  )
            )
          | nm <- types
          ]

{-------------------------------------------------------------------------------
  Type checking
-------------------------------------------------------------------------------}

-- | Type check macro.
--
-- Delegates dispatch between type and expression bodies to 'CExpr.DSL.tcMacro'.
tcMacro ::
     CDeclName
  -> ParsedMacro
  -> M (Either MacroTypecheckError (CheckedMacro TypecheckMacros))
tcMacro name (ParsedMacro macro) = do
    st  <- get
    let typeEnv = st.macroEnv
    case CExpr.DSL.tcMacro typeEnv macro.macroName macro.macroArgs macro.macroExpr of
      Left err ->
        pure $ Left $ MacroTypecheckErrorCExpr err
      Right (CExpr.DSL.MacroTcTypeExpr typeExpr quant) -> do
        addQuant quant
        let typ = fmap mkId typeExpr
        addNewMacroTypeToReparseEnv typ
        pure $ Right $ MacroType $ CheckedMacroType {
            typ = typ
          , ann = NoAnn
          }

      Right (CExpr.DSL.MacroTcValueExpr valueExpr inf) -> do
        modify $ #macroEnv %~ Map.insert macro.macroName inf
        pure $ Right $ MacroExpr $ CheckedMacroExpr{
            args = map mkId macro.macroArgs
          , body = fmap mkId valueExpr
          , typ  = fmap snd inf
          }
  where
    mkId :: CExpr.DSL.Name -> Id TypecheckMacros
    mkId (CExpr.DSL.Name n) = DeclId{
          name   = CDeclName{ text = n, kind = CNameKindMacro }
        , isAnon = False
        }

    addQuant ::
         CExpr.DSL.Quant (CExpr.DSL.FunValue, CExpr.DSL.Type CExpr.DSL.Ty)
      -> M ()
    addQuant quant =
        modify $ #macroEnv %~ Map.insert macro.macroName (quant)

    addNewMacroTypeToReparseEnv :: T.Expr DeclId -> M ()
    addNewMacroTypeToReparseEnv typ
      -- stdbool.h defines @#define bool _Bool@, which is just an alias for
      -- the primitive boolean type. We normalise this away: if @bool@ maps
      -- to @PrimBool@, we store @TypePrim PrimBool@ directly instead of
      -- wrapping it in @TypeMacro@. This ensures that @bool@ from stdbool.h
      -- renders identically to @_Bool@, regardless of language-c version.
      -- Genuine redefinitions like @#define bool int@ are not affected
      -- because their underlying type is not @PrimBool@.
      | name.text == "bool"
      , T.TypeLit CExpr.DSL.TypeBool <- typ
        = addMacroType name (C.TypePrim C.PrimBool)
      | otherwise
        = addMacroType name $
            C.TypeMacro $ C.Ref {
                name       = macroId
              , underlying = C.TypeRef macroId
              }

    macroId :: DeclId
    macroId = DeclId{name = name, isAnon = False}

    addMacroType :: CDeclName -> CType -> M ()
    addMacroType n typ =
        modify $ #macroTypes %~ Map.insert (renderCDeclNameC n) typ

-- {-------------------------------------------------------------------------------
--   Convert T.Expr to C.Type
-- -------------------------------------------------------------------------------}

-- TODO-D; Move this to the backend.

-- TODO-D: I am not sure what we do with a the MacroTypecheckError. Panic?

-- TODO-D: Try to translate into HsType right here.

-- -- | Convert a typechecked macro type expression ('T.Expr') to a C type.
-- --
-- -- Correctly handles chains of @const@ and pointer modifiers (e.g.
-- -- @const int *@, @int * const@, @const int * const *@).
-- --
-- -- Rejects bare @void@ at the top level (including @const void@), but allows
-- -- @void@ as the pointee of a pointer (e.g. @void *@, @const void *@).
-- convertTExpr ::
--      T.Expr CExpr.DSL.Name
--   -> M (Either MacroTypecheckError CType)
-- convertTExpr expr = do
--     result <- go expr
--     pure $ result >>= checkTopLevelVoid
--   where
--     go :: T.Expr CExpr.DSL.Name -> M (Either MacroTypecheckError CType)
--     go = \case
--       T.App T.Pointer inner -> fmap (fmap (C.TypePointers 1)) (go inner)
--       T.App T.Const   inner -> fmap (fmap (C.TypeQual C.QualConst)) (go inner)
--       T.TypeLit t           -> convertLiteral t
--       T.Var   nm            -> do
--         lookupKnownType <- getLookupKnownType
--         pure $ case lookupKnownType nm.getName of
--           Just t  -> Right t
--           Nothing -> Left $ MacroTypecheckErrorUnresolvedType nm.getName

--     -- | @void@ is not a valid standalone type; it is only valid as the
--     -- pointee of a pointer (e.g. @void *@).
--     checkTopLevelVoid :: CType -> Either MacroTypecheckError CType
--     checkTopLevelVoid = \case
--       C.TypeVoid              -> Left MacroTypecheckErrorVoidType
--       C.TypeQual _ C.TypeVoid -> Left MacroTypecheckErrorVoidType
--       ty                      -> Right ty

-- -- | Convert a primitive type specifier to a C type.
-- --
-- -- Named types (previously 'CExpr.DSL.MSpecName' / 'CExpr.DSL.MTypeTagged')
-- -- are handled in 'convertTExpr' via 'T.Var'.
-- convertLiteral ::
--      CExpr.DSL.TypeLit
--   -> M (Either MacroTypecheckError CType)
-- convertLiteral = \case
--     CExpr.DSL.TypeInt sign size ->
--       pure $ Right $ C.TypePrim $ C.PrimIntegral (convertIntSize size) (convertSign sign)
--     CExpr.DSL.TypeChar sign ->
--       pure $ Right $ C.TypePrim $ C.PrimChar (convertCharSign sign)
--     CExpr.DSL.TypeFloat size ->
--       pure $ Right $ C.TypePrim $ C.PrimFloating (convertFloatSize size)
--     CExpr.DSL.TypeVoid ->
--       pure $ Right C.TypeVoid
--     CExpr.DSL.TypeBool ->
--       pure $ Right $ C.TypePrim C.PrimBool
--     CExpr.DSL.TypeTagged tag name -> do
--       lookupKnownType <- getLookupKnownType
--       let cname      = CDeclName name (CNameKindTagged (convertTagKind tag))
--           taggedName = renderCDeclNameC cname
--       pure $ case lookupKnownType taggedName of
--         Just typ -> Right typ
--         Nothing  -> Left $ MacroTypecheckErrorUnresolvedType taggedName

-- convertSign :: Maybe CExpr.DSL.Sign -> C.PrimSign
-- convertSign = \case
--     Nothing                  -> C.Signed
--     Just CExpr.DSL.Signed   -> C.Signed
--     Just CExpr.DSL.Unsigned -> C.Unsigned

-- convertCharSign :: Maybe CExpr.DSL.Sign -> C.PrimSignChar
-- convertCharSign = \case
--     Nothing                  -> C.PrimSignImplicit Nothing
--     Just CExpr.DSL.Signed   -> C.PrimSignExplicit C.Signed
--     Just CExpr.DSL.Unsigned -> C.PrimSignExplicit C.Unsigned

-- convertIntSize :: Maybe CExpr.DSL.IntSize -> C.PrimIntType
-- convertIntSize = \case
--     Nothing                      -> C.PrimInt
--     Just CExpr.DSL.SizeShort    -> C.PrimShort
--     Just CExpr.DSL.SizeInt      -> C.PrimInt
--     Just CExpr.DSL.SizeLong     -> C.PrimLong
--     Just CExpr.DSL.SizeLongLong -> C.PrimLongLong

-- convertFloatSize :: CExpr.DSL.FloatSize -> C.PrimFloatType
-- convertFloatSize = \case
--     CExpr.DSL.SizeFloat  -> C.PrimFloat
--     CExpr.DSL.SizeDouble -> C.PrimDouble
