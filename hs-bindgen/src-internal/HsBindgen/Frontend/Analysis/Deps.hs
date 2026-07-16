module HsBindgen.Frontend.Analysis.Deps (
    depsOfDecl
    -- * Structs and unions
  , depsOfStruct
  , depsOfUnion
  , depsOfField
  , depsOfExplicitField
    -- * Parsed macros
  , depsOfDeclParsedMacro
  ) where

import HsBindgen.Frontend.Analysis
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Macro.Interface qualified as Macro

{-------------------------------------------------------------------------------
  Get all dependencies
-------------------------------------------------------------------------------}


depsOfDeclWith ::
     forall l p. IsPass p
  => (MacroBody p l -> [(Id p, Dependency)])
  -> C.DeclKind l p
  -> [(Id p, Dependency)]
depsOfDeclWith depsOfMacro = \case
    (C.DeclStruct struct)      -> depsOfStruct struct
    (C.DeclUnion union)        -> depsOfUnion union
    (C.DeclEnum _)             -> []
    (C.DeclAnonEnumConstant _) -> []
    (C.DeclTypedef ty)         -> depsOfTypedef ty
    C.DeclOpaque{}             -> []
    (C.DeclMacro m)            -> depsOfMacro m
    (C.DeclFunction function)  ->
      C.depsOfType function.res ++
      concatMap (\arg -> C.depsOfTypeFunArg arg.argTyp) function.args
    (C.DeclGlobal global)      -> C.depsOfType global.typ

{-------------------------------------------------------------------------------
  Dependencies of declarations with parsed macros only
-------------------------------------------------------------------------------}

-- | For parsed macros that have not been typechecked yet, we need to resolve
--   names, using all available declaration IDs
depsOfDeclParsedMacro ::
     forall l p.
     ( IsPass p
     , MacroBody p ~ Macro.Resolved
     , Id p ~ C.DeclId )
  => C.DeclKind l p
  -> [(Id p, Dependency)]
depsOfDeclParsedMacro = depsOfDeclWith (.deps)

{-------------------------------------------------------------------------------
  Dependencies of declaration after @ReparseMacroExpansions@
-------------------------------------------------------------------------------}

-- | Get all dependencies of a declaration in the 'ReparseMacroExpansions' pass
--
-- Disclaimer: For a specific declaration, we can only determine the full set of
-- dependencies after we have reparsed this declaration, because it may contain
-- macro expansions. This step happens in the @ReparseMacroExpansions@ pass.
--
-- Before reparsing, the dependencies of declarations with macro expansions can
-- only refer to the /underlying types/ of the expanded macros.
--
depsOfDecl ::
     C.DeclKind l ReparseMacroExpansions
  -> [(C.DeclId, Dependency)]
depsOfDecl = depsOfDeclTcMacro

depsOfDeclTcMacro ::
     C.DeclKind l ReparseMacroExpansions
  -> [(C.DeclId, Dependency)]
depsOfDeclTcMacro = depsOfDeclWith typecheckedMacroDeps

-- | Dependencies of typechecked macro declarations
typecheckedMacroDeps ::
     TypecheckedMacro ReparseMacroExpansions l
  -> [(C.DeclId, Dependency)]
typecheckedMacroDeps = \case
    MacroType  typ -> typ.deps
    MacroValue val -> val.deps

{-------------------------------------------------------------------------------
  Structs and unions
-------------------------------------------------------------------------------}

depsOfStruct :: IsPass p => C.Struct p -> [(Id p, Dependency)]
depsOfStruct struct = concat [
      concatMap depsOfField struct.fields
    , foldMap   depsOfExplicitField (C.flamStructField struct.flam)
    ]

depsOfUnion :: IsPass p => C.Union p -> [(Id p, Dependency)]
depsOfUnion union = concatMap depsOfField union.fields

-- | Dependencies of struct or union field
depsOfField ::
     forall p. IsPass p
  => C.Field p
  -> [(Id p, Dependency)]
depsOfField = C.elimField depsOfExplicitField depsOfImplicitField

depsOfExplicitField ::
     forall p. IsPass p
  => C.ExplicitField p
  -> [(Id p, Dependency)]
depsOfExplicitField field = C.depsOfType field.typ

depsOfImplicitField ::
     forall p. IsPass p
  => C.ImplicitField p
  -> [(Id p, Dependency)]
depsOfImplicitField field = C.depsOfType field.typ ++ concatMap depsOfIndirectField field.indirect

depsOfIndirectField ::
     forall p. IsPass p
  => C.IndirectField p
  -> [(Id p, Dependency)]
depsOfIndirectField field = C.depsOfType field.typ

{-------------------------------------------------------------------------------
  Typedefs
-------------------------------------------------------------------------------}

depsOfTypedef :: IsPass p => C.Typedef p -> [(Id p, Dependency)]
depsOfTypedef typedef = C.depsOfType typedef.typ
