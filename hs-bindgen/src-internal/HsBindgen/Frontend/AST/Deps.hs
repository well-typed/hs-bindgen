module HsBindgen.Frontend.AST.Deps (
    DepsOfDecl(..)
    -- * Structs and unions
  , depsOfStruct
  , depsOfUnion
  , depsOfField
    -- * Parsed macros
  , MacroNameResolver
  , depsOfDeclParsedMacro
  ) where

import Data.Set qualified as Set
import GHC.Records

import C.Expr.Syntax qualified as CExpr.DSL
import C.Expr.Typecheck.Interface.Type qualified as T
import C.Expr.Typecheck.Interface.Value qualified as V

import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type (ValOrRef (..), depsOfType, depsOfTypeFunArg)
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass (ResolveBindingSpecs)
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Get all dependencies
-------------------------------------------------------------------------------}

-- | Get all dependencies of a declaration
--
-- Disclaimer: For a specific declaration, we can only determine the full set of
-- dependencies after we have reparsed this declaration, because it may contain
-- macro expansions. This step happens in the 'ReparseMacroExpansions' pass.
--
-- Before reparsing, that is, when @p@ is 'TypecheckMacros' or earlier, the
-- dependencies of declarations with macro expansions can only refer to the
-- /underlying types/ of the expanded macros. We do not provide instance of
-- 'DepsOfDecl' for early passes.
--
-- The primary use case of 'DepsOfDecl' is in the 'ReparseMacroExpansions' pass,
-- where we first reparse the declaration, and afterwards update the dependency
-- graphs using this class.
class DepsOfDecl p where
  depsOfDecl :: IsPass p => C.DeclKind p -> [(ValOrRef, Id p)]

instance DepsOfDecl ReparseMacroExpansions where depsOfDecl = depsOfDeclTcMacro
instance DepsOfDecl ResolveBindingSpecs    where depsOfDecl = depsOfDeclTcMacro
instance DepsOfDecl MangleNames            where depsOfDecl = depsOfDeclTcMacro
instance DepsOfDecl AdjustTypes            where depsOfDecl = depsOfDeclTcMacro
instance DepsOfDecl Select                 where depsOfDecl = depsOfDeclTcMacro

depsOfDeclWith ::
     forall p. IsPass p
  => (MacroBody p -> [(ValOrRef, Id p)]) -> C.DeclKind p -> [(ValOrRef, Id p)]
depsOfDeclWith depsOfMacro = \case
    (C.DeclStruct struct)      -> depsOfStruct struct
    (C.DeclUnion union)        -> depsOfUnion union
    (C.DeclEnum _)             -> []
    (C.DeclAnonEnumConstant _) -> []
    (C.DeclTypedef ty)         -> depsOfTypedef ty
    C.DeclOpaque               -> []
    (C.DeclMacro m)            -> depsOfMacro m
    (C.DeclFunction function)  ->
      depsOfType function.res ++
      concatMap (\arg -> depsOfTypeFunArg arg.argTyp) function.args
    (C.DeclGlobal global)      -> depsOfType global.typ

depsOfDeclTcMacro ::
     forall p. (IsPass p, MacroBody p ~ CheckedMacro p)
  => C.DeclKind p -> [(ValOrRef, Id p)]
depsOfDeclTcMacro = depsOfDeclWith (depsOfTcMacro (Proxy @p))

-- | Dependencies of typechecked macro declarations
depsOfTcMacro ::
     (IsPass p, MacroBody p ~ CheckedMacro p)
  => Proxy p -> MacroBody p -> [(ValOrRef, Id p)]
depsOfTcMacro proxy = map (ByValue,) . \case
    MacroType typ ->
      depsOfTExpr proxy typ.typ
    MacroExpr expr ->
      depsOfVExpr proxy (Set.fromList expr.args) expr.body
  where
    -- Collect value-level dependencies from a checked macro type.
    depsOfTExpr ::
         forall p.
         Proxy p
      -> T.Expr (Id p)
      -> [Id p]
    depsOfTExpr _ = toList
    -- Collect value-level dependencies from a checked macro body. Local
    -- arguments (lambda-bound ids) are excluded.
    depsOfVExpr ::
         forall p. (Ord (Id p))
      => Proxy p
      -> Set (Id p)
      -> V.Expr (Id p)
      -> [Id p]
    depsOfVExpr _ localArgs =
         filter (not . isLocalArg) . toList
      where
        isLocalArg :: Id p -> Bool
        isLocalArg x = Set.member x localArgs

{-------------------------------------------------------------------------------
  Structs and unions
-------------------------------------------------------------------------------}

depsOfStruct :: IsPass p => C.Struct p -> [(ValOrRef, Id p)]
depsOfStruct struct = concat [
      concatMap depsOfField struct.fields
    , concatMap depsOfField struct.flam
    ]

depsOfUnion :: IsPass p => C.Union p -> [(ValOrRef, Id p)]
depsOfUnion union = concatMap depsOfField union.fields

-- | Dependencies of struct or union field
depsOfField :: forall a p.
     (HasField "typ" (a p) (C.Type p), IsPass p)
  => a p -> [(ValOrRef, Id p)]
depsOfField field = depsOfType field.typ

{-------------------------------------------------------------------------------
  Typedefs
-------------------------------------------------------------------------------}

depsOfTypedef :: IsPass p => C.Typedef p -> [(ValOrRef, Id p)]
depsOfTypedef typedef = depsOfType typedef.typ

{-------------------------------------------------------------------------------
  Dependencies of declarations with parsed macros only
-------------------------------------------------------------------------------}

-- | For parsed macros that have not been typechecked yet, we need to resolve
--   bare names (see 'MacroNameResolver').
depsOfDeclParsedMacro ::
     forall p. (IsPass p, MacroBody p ~ ParsedMacro, Id p ~ DeclId)
  => MacroNameResolver -> C.DeclKind p -> [(ValOrRef, Id p)]
depsOfDeclParsedMacro resolver = depsOfDeclWith depsOfParsedMacro
  where
    depsOfParsedMacro :: ParsedMacro -> [(ValOrRef, DeclId)]
    depsOfParsedMacro (ParsedMacro m) = depsOfCExprMacro resolver m

-- | Resolves a bare name found in a parsed macro body
--
-- Result:
--
-- - @'Just' 'CNameKind'@: This bare name refers to another declaration that is in
--   the 'DeclIndex'. For example, a macro, or a @typedef@.
--
-- - @'Nothing'@: This bare name refers to a built-in type or a system @typedef@
--   not present in the 'DeclIndex'. In this case, we /do not generate/ a
--   dependency.
type MacroNameResolver = Text -> Maybe CNameKind

-- | Collect all references in a macro body.
--
-- Local macro arguments are excluded.
depsOfCExprMacro ::
     MacroNameResolver
  -> CExpr.DSL.Macro
  -> [(ValOrRef, DeclId)]
depsOfCExprMacro resolver macro =
    map (ByValue,) $ goExpr (Set.fromList macro.macroArgs) macro.macroExpr
  where
    goExpr :: Set CExpr.DSL.Name -> CExpr.DSL.Expr CExpr.DSL.Ps -> [DeclId]
    goExpr localArgs = \case
      CExpr.DSL.Term  term   -> goTerm  localArgs term
      CExpr.DSL.TyApp _ xs   -> concatMap (goExpr localArgs) xs
      CExpr.DSL.VaApp _ _ xs -> concatMap (goExpr localArgs) xs

    goTerm :: Set CExpr.DSL.Name -> CExpr.DSL.Term CExpr.DSL.Ps -> [DeclId]
    goTerm localArgs = \case
      CExpr.DSL.Literal lit -> goLit lit
      -- Variable / function call. A bare identifier is always parsed as Var;
      -- the typechecker decides whether it is a type or value reference.
      -- We use the resolver to emit a dep of the correct kind, or skip the
      -- name entirely if it is not a known declaration (e.g. a built-in type).
      CExpr.DSL.Var _ nm callArgs
        | nm `Set.member` localArgs ->
            concatMap (goExpr localArgs) callArgs
        | Just kind <- resolver nm.getName ->
            mkId nm.getName kind :
            concatMap (goExpr localArgs) callArgs
        | otherwise ->
            concatMap (goExpr localArgs) callArgs

    goLit :: CExpr.DSL.Literal -> [DeclId]
    goLit = \case
      -- Named type specifier using an elaborated tag: struct/union/enum.
      CExpr.DSL.TypeLit (CExpr.DSL.TypeTagged tag nm) ->
        [ mkId nm (CNameKindTagged (convertTagKind tag)) ]
      -- Other built-in type specifiers (int, char, etc.) have no dependencies.
      CExpr.DSL.TypeLit _ -> []
      -- Value literals have no dependencies.
      _ -> []

    mkId :: Text -> CNameKind -> DeclId
    mkId name kind = DeclId (CDeclName name kind) False
