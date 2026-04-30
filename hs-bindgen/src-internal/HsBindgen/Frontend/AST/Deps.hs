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

import GHC.Records

import C.Expr.Syntax qualified as CExpr
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
depsOfTcMacro proxy = \case
    MacroType typ  -> depsOfType typ.typ
    MacroExpr expr -> depsOfVExpr proxy expr.body
  where
    -- Collect value-level dependencies from a checked macro body. Local
    -- arguments (lambda-bound ids) are excluded.
    depsOfVExpr ::
         forall p.
         Proxy p
      -> V.Expr (Id p)
      -> [(ValOrRef, Id p)]
    depsOfVExpr _ =
        map (ByValue,) . toList

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
  -> CExpr.Macro
  -> [(ValOrRef, DeclId)]
depsOfCExprMacro resolver macro =
    map (ByValue,) $ goExpr macro.macroExpr
  where
    goExpr :: CExpr.Expr CExpr.Ps -> [DeclId]
    goExpr = \case
      CExpr.Term  term   -> goTerm term
      CExpr.TyApp _ xs   -> concatMap goExpr xs
      CExpr.VaApp _ _ xs -> concatMap goExpr xs

    goTerm :: CExpr.Term CExpr.Ps -> [DeclId]
    goTerm = \case
      CExpr.Literal lit -> goLit lit
      CExpr.LocalArg  _ -> []
      -- Variable / function call. A bare identifier is always parsed as Var;
      -- the typechecker decides whether it is a type or value reference.
      -- We use the resolver to emit a dep of the correct kind, or skip the
      -- name entirely if it is not a known declaration (e.g. a built-in type).
      CExpr.Var _ nm callArgs
        | Just kind <- resolver nm.getName ->
            mkId nm kind :
            concatMap goExpr callArgs
        | otherwise ->
            concatMap goExpr callArgs

    goLit :: CExpr.Literal -> [DeclId]
    goLit = \case
      -- Named type specifier using an elaborated tag: struct/union/enum.
      CExpr.TypeTagged tag nm ->
        [ mkId nm (CNameKindTagged (convertTagKind tag)) ]
      -- Other built-in type specifiers (int, char, etc.) have no dependencies.
      CExpr.TypeLit{} ->
        []
      -- Value literals have no dependencies.
      CExpr.ValueLit{} ->
        []

    mkId :: CExpr.Name -> CNameKind -> DeclId
    mkId name kind = DeclId (CDeclName name.getName kind) False
