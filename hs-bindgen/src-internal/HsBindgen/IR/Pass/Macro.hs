-- | Macros
--
-- This module should only be used within the @HsBindgen.IR@ hierarchy.  From
-- outside the @HsBindgen.IR@ hierarchy, "HsBindgen.IR.Pass" should be used.
--
-- Intended for unqualified import.
--
-- > import HsBindgen.IR.Pass.Macro
module HsBindgen.IR.Pass.Macro (
    -- * Associated type families
    PassMacro(..)
    -- * Coercion
  , CoercePassMacroId(..)
  , CoercePassMacroBody(..)
  , CoercePassMacroUnderlying(..)
  ) where

import HsBindgen.Imports
import HsBindgen.IR.Pass.Definition
import HsBindgen.IR.Pass.Id
import HsBindgen.Macro.Type qualified as Macro

{-------------------------------------------------------------------------------
  Associated type families
-------------------------------------------------------------------------------}

-- | Macros vary across passes
class (
      Eq   (MacroId         p)  -- For @Eq DeclKind@ in @DeclIndex@ construction
    , Eq   (MacroUnderlying p)  -- For @Eq DeclKind@ in @DeclIndex@ construction
    , Ord  (MacroId         p)  -- For using as map key
    , Ord  (MacroUnderlying p)  -- For using as map key
    , Show (MacroId         p)  -- For debugging
    , Show (MacroUnderlying p)  -- For debugging
    , forall l. Macro.HasTypes l => ValidMacroBody p l
    ) => PassMacro (p :: Pass) where

  -- | Declaration identifier for macro types
  --
  -- 1. Before 'HsBindgen.Frontend.Pass.TypecheckMacros.IsPass.TypecheckMacros',
  --   this is 'Void', as macro types do not exist yet.
  -- 2. After 'HsBindgen.Frontend.Pass.TypecheckMacros.IsPass.TypecheckMacros',
  --   this is @'Id' p@.
  type MacroId p :: Star
  type MacroId p = Void

  -- | Macro body
  --
  -- Typechecking macros is non-trivial, and requires knowledge of other types
  -- and macros in scope.  We therefore only parse (and not typecheck) macros
  -- while parsing the LLVM/Clang AST.
  -- 'HsBindgen.Frontend.Pass.TypecheckMacros.IsPass.TypecheckMacros' then does
  -- the actual typechecking, instantiating this to
  -- 'HsBindgen.Frontend.Pass.TypecheckMacros.IsPass.TypecheckedMacro'.
  type MacroBody p :: Star -> Star

  -- | Underlying type stored in a macro reference
  --
  -- 1. Before 'HsBindgen.Frontend.Pass.TypecheckMacros.IsPass.TypecheckMacros',
  --   this is 'Void', as macro types do not exist yet.
  -- 2. After
  --   'HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass.ReparseMacroExpansions',
  --   the reparser produces 'HsBindgen.IR.C.Type.MacroRef' values with a
  --   placeholder underlying, recording \"a macro is referenced here,
  --   underlying not yet known\".
  -- 3. After 'HsBindgen.Frontend.Pass.Zip.IsPass.Zip', this is
  --   @'HsBindgen.IR.C.Type.Type' p@.  That pass fills the underlying in from
  --   the pre-reparse tree.
  type MacroUnderlying p :: Star
  type MacroUnderlying p = Void

  macroIdId :: Proxy p -> MacroId p -> Id p
  default macroIdId :: MacroId p ~ Void => Proxy p -> MacroId p -> Id p
  macroIdId _ = absurd

-- | Class alias; limitation of quantified constraints
class    (Eq (MacroBody p l), Show (MacroBody p l)) => ValidMacroBody p l
instance (Eq (MacroBody p l), Show (MacroBody p l)) => ValidMacroBody p l

{-------------------------------------------------------------------------------
  Coercion
-------------------------------------------------------------------------------}

class CoercePassMacroId (p :: Pass) (p' :: Pass) where
  coercePassMacroId :: Proxy '(p, p') -> MacroId p -> MacroId p'

  default coercePassMacroId ::
       (MacroId p ~ MacroId p')
    => Proxy '(p, p') -> MacroId p -> MacroId p'
  coercePassMacroId _ = id

class CoercePassMacroBody (p :: Pass) (p' :: Pass) where
  coercePassMacroBody :: Proxy '(p, p') -> MacroBody p l -> MacroBody p' l

  default coercePassMacroBody ::
       (MacroBody p ~ MacroBody p' )
    => Proxy '(p, p') -> MacroBody p l -> MacroBody p' l
  coercePassMacroBody _ = id

class CoercePassMacroUnderlying (p :: Pass) (p' :: Pass) where
  coercePassMacroUnderlying ::
    Proxy '(p, p') -> MacroUnderlying p -> MacroUnderlying p'

  default coercePassMacroUnderlying ::
       (MacroUnderlying p ~ MacroUnderlying p')
    => Proxy '(p, p') -> MacroUnderlying p -> MacroUnderlying p'
  coercePassMacroUnderlying _ = id
