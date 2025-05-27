module HsBindgen.Frontend.Pass.HandleMacros.IsPass (
    HandleMacros
    -- * Parsed macros
  , CheckedMacro(..)
  ) where

import GHC.TypeLits (Symbol)

-- TODO.
import HsBindgen.C.AST.Macro qualified as Old
import HsBindgen.C.Tc.Macro qualified as Macro
import HsBindgen.C.Tc.Macro.Type qualified as Macro
import HsBindgen.Frontend.AST
import HsBindgen.Frontend.Graph.UseDef (UseDefGraph)
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type HandleMacros :: Pass
data HandleMacros a deriving anyclass ValidPass

-- We don't need the 'ReparseInfo' anymore.
type family AnnHandleMacros (ix :: Symbol) :: Star where
  AnnHandleMacros "TranslationUnit" = UseDefGraph Parse
  AnnHandleMacros _                 = NoAnn

instance IsPass HandleMacros where
  type Id     HandleMacros = DeclId
  type Macro  HandleMacros = CheckedMacro
  type Ann ix HandleMacros = AnnHandleMacros ix

{-------------------------------------------------------------------------------
  Parsed macros
-------------------------------------------------------------------------------}

data CheckedMacro = CheckedMacro {
      checkedMacro     :: Old.Macro Macro.Ps
    , checkedMacroType :: Macro.Quant (Macro.Type Macro.Ty)
    }
  deriving stock (Show)
