module HsBindgen.Frontend.Analysis.Deps (
    depsOfDecl
    -- * Structs and unions
  , depsOfStruct
  , depsOfUnion
  , depsOfField
    -- * Parsed macros
  , depsOfDeclParsedMacro
  ) where

import GHC.Records

import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.Pass.Zip.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Macro.Interface
import HsBindgen.Macro.Type

{-------------------------------------------------------------------------------
  Get all dependencies
-------------------------------------------------------------------------------}


depsOfDeclWith ::
     forall l p. IsPass p
  => (MacroBody p l -> [(C.ValOrRef, Id p)])
  -> C.DeclKind l p
  -> [(C.ValOrRef, Id p)]
depsOfDeclWith depsOfMacro = \case
    (C.DeclStruct struct)      -> depsOfStruct struct
    (C.DeclUnion union)        -> depsOfUnion union
    (C.DeclEnum _)             -> []
    (C.DeclAnonEnumConstant _) -> []
    (C.DeclTypedef ty)         -> depsOfTypedef ty
    C.DeclOpaque               -> []
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
     , MacroBody p ~ ParsedMacroBody
     , Id p ~ C.DeclId )
  => MacroLang l
  -> Set C.DeclId
  -> C.DeclKind l p
  -> [(C.ValOrRef, Id p)]
depsOfDeclParsedMacro macroLang allDeclIds = depsOfDeclWith depsOfParsedMacro
  where
    depsOfParsedMacro :: ParsedMacroBody l -> [(C.ValOrRef, C.DeclId)]
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
  -> [(C.ValOrRef, C.DeclId)]
depsOfDecl = depsOfDeclTcMacro

depsOfDeclTcMacro ::
     forall l. (HasMacroTypes l)
  => MacroLang l -> C.DeclKind l Zip -> [(C.ValOrRef, C.DeclId)]
depsOfDeclTcMacro macroLang = depsOfDeclWith (typecheckedMacroDeps macroLang)

-- | Dependencies of typechecked macro declarations
typecheckedMacroDeps ::
     forall l. HasMacroTypes l
  => MacroLang l
  -> TypecheckedMacro Zip l
  -> [(C.ValOrRef, C.DeclId)]
typecheckedMacroDeps macroLang = \case
    MacroType  typ ->
      macroLang.typecheckedMacroTypeDeps $ fmap fromMacroTypeBodyVar typ.body
    MacroValue val ->
      typecheckedMacroValueDeps val.body
  where
    fromMacroTypeBodyVar :: MacroTypeBodyVar Zip -> C.DeclId
    fromMacroTypeBodyVar = \case
      MacroTypeExtBinding      x -> absurd x
      MacroTypeBodyVar    declId -> declId

    -- Collect value-level dependencies from a checked macro body. Local
    -- arguments (lambda-bound IDs) are excluded.
    --
    -- On the value-level, all dependencies must be 'ByValue'.
    typecheckedMacroValueDeps ::
         TypecheckedMacroValueBody l C.DeclId
      -> [(C.ValOrRef, C.DeclId)]
    typecheckedMacroValueDeps = map (C.ByValue,) . toList

{-------------------------------------------------------------------------------
  Structs and unions
-------------------------------------------------------------------------------}

depsOfStruct :: IsPass p => C.Struct p -> [(C.ValOrRef, Id p)]
depsOfStruct struct = concat [
      concatMap depsOfField struct.fields
    , concatMap depsOfField struct.flam
    ]

depsOfUnion :: IsPass p => C.Union p -> [(C.ValOrRef, Id p)]
depsOfUnion union = concatMap depsOfField union.fields

-- | Dependencies of struct or union field
depsOfField :: forall a p.
     (HasField "typ" (a p) (C.Type p), IsPass p)
  => a p -> [(C.ValOrRef, Id p)]
depsOfField field = C.depsOfType field.typ

{-------------------------------------------------------------------------------
  Typedefs
-------------------------------------------------------------------------------}

depsOfTypedef :: IsPass p => C.Typedef p -> [(C.ValOrRef, Id p)]
depsOfTypedef typedef = C.depsOfType typedef.typ
