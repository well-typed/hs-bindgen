module HsBindgen.Frontend.AST.Deps (
    depsOfDecl
    -- * Structs and unions
  , depsOfStruct
  , depsOfUnion
  , depsOfField
    -- * Parsed macros
  , depsOfDeclParsedMacro
  ) where

import GHC.Records

import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type (ValOrRef (..), depsOfType, depsOfTypeFunArg)
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.Pass.Zip.IsPass
import HsBindgen.Imports
import HsBindgen.Macro.Interface
import HsBindgen.Macro.Type

{-------------------------------------------------------------------------------
  Get all dependencies
-------------------------------------------------------------------------------}


depsOfDeclWith ::
     forall l p. IsPass p
  => (MacroBody p l -> [(ValOrRef, Id p)])
  -> C.DeclKind l p
  -> [(ValOrRef, Id p)]
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

{-------------------------------------------------------------------------------
  Dependencies of declarations with parsed macros only
-------------------------------------------------------------------------------}

-- | For parsed macros that have not been typechecked yet, we need to resolve
--   names, using all available declaration IDs
depsOfDeclParsedMacro ::
     forall l p.
     ( IsPass p
     , MacroBody p ~ ParsedMacroBody
     , Id p ~ DeclId )
  => MacroLang l
  -> Set DeclId
  -> C.DeclKind l p
  -> [(ValOrRef, Id p)]
depsOfDeclParsedMacro macroLang allDeclIds = depsOfDeclWith depsOfParsedMacro
  where
    depsOfParsedMacro :: ParsedMacroBody l -> [(ValOrRef, DeclId)]
    depsOfParsedMacro body = macroLang.parsedMacroDeps allDeclIds body

{-------------------------------------------------------------------------------
  Dependencies of declaration after @ReparseMacroExpansions@ and 'Zip'
-------------------------------------------------------------------------------}

-- | Get all dependencies of a declaration in the 'Zip' pass
--
-- Disclaimer: For a specific declaration, we can only determine the full set of
-- dependencies after we have reparsed this declaration, because it may contain
-- macro expansions. This step happens in the @ReparseMacroExpansions@ and 'Zip'
-- passes.
--
-- Before reparsing, that is, when @p@ is 'TypecheckMacros' or earlier, the
-- dependencies of declarations with macro expansions can only refer to the
-- /underlying types/ of the expanded macros.
--
-- We use 'depsOfDecl' after reconciling the pre- and post-reparse ASTs in the
-- 'Zip' pass, updating the dependency graphs.
depsOfDecl ::
     HasMacroTypes l
  => MacroLang l
  -> C.DeclKind l Zip
  -> [(ValOrRef, DeclId)]
depsOfDecl = depsOfDeclTcMacro

depsOfDeclTcMacro ::
     forall l. (HasMacroTypes l)
  => MacroLang l -> C.DeclKind l Zip -> [(ValOrRef, DeclId)]
depsOfDeclTcMacro macroLang = depsOfDeclWith (typecheckedMacroDeps macroLang)

-- | Dependencies of typechecked macro declarations
typecheckedMacroDeps ::
     forall l. HasMacroTypes l
  => MacroLang l
  -> TypecheckedMacro Zip l
  -> [(ValOrRef, DeclId)]
typecheckedMacroDeps macroLang = \case
    MacroType  typ ->
      macroLang.typecheckedMacroTypeDeps $ fmap fromMacroTypeBodyVar typ.body
    MacroValue val ->
      typecheckedMacroValueDeps val.body
  where
    fromMacroTypeBodyVar :: MacroTypeBodyVar Zip -> DeclId
    fromMacroTypeBodyVar = \case
      MacroTypeExtBinding      x -> absurd x
      MacroTypeBodyVar    declId -> declId

    -- Collect value-level dependencies from a checked macro body. Local
    -- arguments (lambda-bound IDs) are excluded.
    --
    -- On the value-level, all dependencies must be 'ByValue'.
    typecheckedMacroValueDeps ::
         TypecheckedMacroValueBody l DeclId
      -> [(ValOrRef, DeclId)]
    typecheckedMacroValueDeps = map (ByValue,) . toList

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
