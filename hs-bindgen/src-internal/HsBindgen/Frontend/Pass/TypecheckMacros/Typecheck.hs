module HsBindgen.Frontend.Pass.TypecheckMacros.Typecheck (
    FailedMacro
  , typecheckDecls
  ) where

import Control.Monad.Except (Except, throwError)
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Set qualified as Set

import C.Expr.Syntax qualified as CExpr
import C.Expr.Typecheck qualified as CExpr
import C.Expr.Typecheck.Interface.Type qualified as T

import Clang.HighLevel.Types

import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.Error
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

type Pre   = ConstructTranslationUnit
type Post  = TypecheckMacros
type CType = C.Type Post

type FailedMacro = (DeclId, SingleLoc, TypecheckMacrosError)

{-------------------------------------------------------------------------------
  Traversal 2: Typecheck macros
-------------------------------------------------------------------------------}

-- | Typecheck all macro declarations, threading non-macro declarations through
-- unchanged.
--
-- Returns the per-declaration results together with @resolvedMacroTypes@, the
-- mapping from macro names to their underlying C types (used by the reparse
-- environment).
--
-- TODO <https://github.com/well-typed/hs-bindgen/issues/1952>
--
-- We should only resolve the declaration IDs of macro dependencies once. Now
-- we resolve them twice: when we get the dependencies of parsed macros, and
-- when we typecheck macros.
typecheckDecls ::
     Map Text CType
  -> Map CDeclName CType
  -> [C.Decl Pre]
  -> ([Either FailedMacro (C.Decl Post)], Map Text CType)
typecheckDecls knownTypedefs knownTaggedTypes decls =
    merge knownTypedefs knownTaggedTypes checkedMacros decls
  where
    parsedMacros :: [ParsedMacro]
    parsedMacros = mapMaybe getParsedMacro decls

    knownTypedefNames :: Set CExpr.Name
    knownTypedefNames =
      Set.mapMonotonic CExpr.Name (Map.keysSet knownTypedefs)

    checkedMacros ::
      Map CExpr.Name (CExpr.MacroTcResult TypecheckMacrosError DeclId)
    checkedMacros =
      CExpr.tcMacros
        knownTypedefNames
        injectTypeName
        injectValueName
        (injectTaggedTypeNameWith knownTaggedTypes)
        (map (.parsedMacro) parsedMacros)

{-------------------------------------------------------------------------------
  Inject functions (callbacks for the @c-expr-dsl@ typechecker)
-------------------------------------------------------------------------------}

injectTypeName ::
     CExpr.TypeSource
  -> CExpr.Name
  -> DeclId
injectTypeName = \case
    CExpr.TypeSourceTypedef   ->
      \n -> DeclId (CDeclName n.getName CNameKindOrdinary) False
    CExpr.TypeSourceMacroType ->
      \n -> DeclId (CDeclName n.getName CNameKindMacro) False

injectTaggedTypeNameWith ::
     Map CDeclName CType
  -> CExpr.TagKind
  -> CExpr.Name
  -> Except TypecheckMacrosError DeclId
injectTaggedTypeNameWith knownTaggedTypes k n =
    if Map.member dn knownTaggedTypes then
      pure $ DeclId dn False
    else
      throwError $ TypecheckMacrosErrorUnresolvedTaggedType dn
  where
    dn = CDeclName{
        text = n.getName
      , kind = CNameKindTagged $ convertTagKind k
      }

injectValueName :: CExpr.Name -> DeclId
injectValueName (CExpr.Name n) =
    let dn = CDeclName{text = n , kind = CNameKindMacro}
    in  DeclId dn False

{-------------------------------------------------------------------------------
  Interleaving: thread typechecked macros back into declaration order
-------------------------------------------------------------------------------}

getParsedMacro :: C.Decl Pre -> Maybe ParsedMacro
getParsedMacro decl = case decl.kind of
    C.DeclMacro macro -> Just macro
    _otherwise        -> Nothing

coerceDecl :: C.Decl Pre -> Either (C.Decl Post) (C.DeclInfo Post, ParsedMacro)
coerceDecl decl = case decl.kind of
    C.DeclMacro macro        -> Right (info', macro)
    C.DeclTypedef          k -> withCoercedKind $ C.DeclTypedef          $ coercePass k
    C.DeclStruct           k -> withCoercedKind $ C.DeclStruct           $ coercePass k
    C.DeclUnion            k -> withCoercedKind $ C.DeclUnion            $ coercePass k
    C.DeclEnum             k -> withCoercedKind $ C.DeclEnum             $ coercePass k
    C.DeclAnonEnumConstant k -> withCoercedKind $ C.DeclAnonEnumConstant $ coercePass k
    C.DeclOpaque             -> withCoercedKind $ C.DeclOpaque
    C.DeclFunction         k -> withCoercedKind $ C.DeclFunction         $ coercePass k
    C.DeclGlobal           k -> withCoercedKind $ C.DeclGlobal           $ coercePass k
  where
    info' :: C.DeclInfo Post
    info' = coercePass decl.info

    withCoercedKind :: C.DeclKind Post -> Either (C.Decl Post) a
    withCoercedKind kind' = Left (C.Decl info' kind' decl.ann)

-- | Merge typechecked macro results back into the original declarations
merge ::
     Map Text CType
  -> Map CDeclName CType
  -> Map CExpr.Name (CExpr.MacroTcResult TypecheckMacrosError DeclId)
  -> [C.Decl Pre]
  -> ([Either FailedMacro (C.Decl Post)], Map Text CType)
merge knownTypedefs knownTaggedTypes checkedMacros =
    first reverse . Foldable.foldl' step ([], Map.empty)
  where
    -- step :: _
    step (eDecls, resolvedMacroTypes) decl = case coerceDecl decl of
      Left decl' -> (Right decl' : eDecls, resolvedMacroTypes)
      Right (info', macro) ->
        case Map.lookup macro.parsedMacro.macroName checkedMacros of
          Nothing ->
            panicPure $ "merge: typechecked macro unavailable: "
                      <> show macro.parsedMacro.macroName
          Just checkedMacro ->
            ( fromMacroTcResult info' checkedMacro : eDecls
            , addMacroType
                knownTypedefs
                knownTaggedTypes
                info'
                checkedMacro
                resolvedMacroTypes
            )

fromMacroTcResult ::
     C.DeclInfo Post
  -> CExpr.MacroTcResult TypecheckMacrosError DeclId
  -> Either FailedMacro (C.Decl Post)
fromMacroTcResult info = \case
    CExpr.MacroTcTypeExpr  x ->
      Right $ toDecl $
        MacroType $ CheckedMacroType (fmap MacroTypeBodyVar x) NoAnn
    CExpr.MacroTcValueExpr x ->
      Right $ toDecl $
        MacroValue $ CheckedMacroValue x
    CExpr.MacroTcInjectError err ->
      Left (info.id, info.loc, err)
    CExpr.MacroTcError err ->
      Left (info.id, info.loc, TypecheckMacrosErrorCExpr err)
  where
    toDecl :: CheckedMacro Post -> C.Decl Post
    toDecl checked = C.Decl{ info = info, kind = C.DeclMacro checked, ann = NoAnn }

{-------------------------------------------------------------------------------
  Build resolvedMacroTypes
-------------------------------------------------------------------------------}

-- | Add a macro's underlying C type to the accumulator.
--
-- Special case: @#define bool _Bool@ from stdbool.h is normalised so that
-- @bool@ renders identically to @_Bool@ regardless of @language-c@ version.
addMacroType ::
     Map Text CType
  -> Map CDeclName CType
  -> C.DeclInfo Post
  -> CExpr.MacroTcResult TypecheckMacrosError DeclId
  -> Map Text CType
  -> Map Text CType
addMacroType knownTypedefs knownTaggedTypes declInfo tcRes macroTypes =
    case tcRes of
      CExpr.MacroTcTypeExpr x ->
        let ctype :: CType
            ctype =
              convertTExpr
                (lookupTypeWith knownTypedefs knownTaggedTypes macroTypes)
                x.macroTypeBody
        in  case (declInfo.id.name.text, ctype) of
              ("bool", C.TypePrim C.PrimBool) ->
                Map.insert declInfo.id.name.text ctype macroTypes
              _otherwise ->
                Map.insert declInfo.id.name.text
                  (C.TypeMacro $ C.Ref{
                      name       = declInfo.id
                    , underlying = ctype
                    }) macroTypes
      _ -> macroTypes

-- | Look up the 'CType' for a 'DeclId' produced by the inject functions.
--
-- 'panicPure' indicates a bug: the typechecker only emits 'DeclId's via the
-- inject functions, which only inject names that were present at injection
-- time.
lookupTypeWith ::
     Map Text CType
  -> Map CDeclName CType
  -> Map Text CType
  -> DeclId
  -> CType
lookupTypeWith knownTypedefs knownTaggedTypes knownMacros declId =
    case mType of
      Just t ->
        t
      Nothing ->
        panicPure $ "lookupTypeWith: declaration not found " <> show declId
  where
    mType :: Maybe CType
    mType = case declId.name.kind of
      CNameKindOrdinary -> Map.lookup declId.name.text knownTypedefs
      CNameKindMacro    -> Map.lookup declId.name.text knownMacros
      CNameKindTagged _ -> Map.lookup declId.name      knownTaggedTypes

{-------------------------------------------------------------------------------
  Convert T.Expr to C.Type

  See 'HsBindgen.Backend.Hs.Translation.MacroType'.

  Will be removed in the future.
-------------------------------------------------------------------------------}

-- | Convert a typechecked macro type expression ('T.Expr') to a C type.
--
-- Named types appear as 'DeclId' leaves; 'lookupType' maps each 'DeclId' back
-- to its 'CType'. Top-level @void@ and @const void@ are rejected by
-- 'CExpr.tcMacro' ('TcIncompleteTypeMacro'), so this conversion is total.
convertTExpr :: (DeclId -> CType) -> T.Expr DeclId -> CType
convertTExpr lookupType = go
  where
    go :: T.Expr DeclId -> CType
    go = \case
      T.App T.Pointer inner -> C.TypePointers 1 (go inner)
      T.App T.Const   inner -> C.TypeQual C.QualConst (go inner)
      T.TypeLit t           -> convertLiteral t
      T.Var declId          -> lookupType declId

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
    Nothing             -> C.Signed
    Just CExpr.Signed   -> C.Signed
    Just CExpr.Unsigned -> C.Unsigned

convertCharSign :: Maybe CExpr.Sign -> C.PrimSignChar
convertCharSign = \case
    Nothing             -> C.PrimSignImplicit Nothing
    Just CExpr.Signed   -> C.PrimSignExplicit C.Signed
    Just CExpr.Unsigned -> C.PrimSignExplicit C.Unsigned

convertIntSize :: Maybe CExpr.IntSize -> C.PrimIntType
convertIntSize = \case
    Nothing                 -> C.PrimInt
    Just CExpr.SizeShort    -> C.PrimShort
    Just CExpr.SizeInt      -> C.PrimInt
    Just CExpr.SizeLong     -> C.PrimLong
    Just CExpr.SizeLongLong -> C.PrimLongLong

convertFloatSize :: CExpr.FloatSize -> C.PrimFloatType
convertFloatSize = \case
    CExpr.SizeFloat  -> C.PrimFloat
    CExpr.SizeDouble -> C.PrimDouble
