module HsBindgen.Frontend.Pass.HandleMacros.IsPass (
    HandleMacros
    -- * Parsed macros
  , CheckedMacro(..)
  ) where

import GHC.TypeLits (Symbol)

import HsBindgen.C.AST.Macro qualified as Old -- TODO
import HsBindgen.C.Tc.Macro qualified as Macro
import HsBindgen.C.Tc.Macro.Type qualified as Macro
import HsBindgen.Frontend.AST
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type HandleMacros :: Pass
data HandleMacros a

-- We don't need the 'ReparseInfo' anymore.
type family AnnHandleMacros (ix :: Symbol) :: Star where
  AnnHandleMacros "Field"   = NoAnn
  AnnHandleMacros "Typedef" = NoAnn
  AnnHandleMacros ix        = Ann ix (Previous HandleMacros)

instance IsPass HandleMacros where
  type Previous HandleMacros = Parse
  type Macro    HandleMacros = CheckedMacro
  type Ann ix   HandleMacros = AnnHandleMacros ix

instance ShowPass HandleMacros

{-------------------------------------------------------------------------------
  Parsed macros
-------------------------------------------------------------------------------}

data CheckedMacro = CheckedMacro {
      checkedMacro     :: Old.Macro Macro.Ps
    , checkedMacroType :: Macro.Quant (Macro.Type Macro.Ty)
    }
  deriving stock (Show)
