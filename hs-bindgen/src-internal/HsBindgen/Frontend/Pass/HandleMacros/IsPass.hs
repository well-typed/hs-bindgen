module HsBindgen.Frontend.Pass.HandleMacros.IsPass (
    HandleMacros
    -- * Parsed macros
  , CheckedMacro(..)
  , CheckedMacroExpr(..)
  ) where

import HsBindgen.C.Tc.Macro qualified as Macro
import HsBindgen.C.Tc.Macro.Type qualified as Macro
import HsBindgen.Frontend.AST.Internal (ValidPass, CName, Type)
import HsBindgen.Frontend.Graph.UseDef (UseDefGraph)
import HsBindgen.Frontend.Macros.AST.Syntax qualified as Macro
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
  type Id        HandleMacros = DeclId
  type FieldName HandleMacros = CName
  type MacroBody HandleMacros = CheckedMacro HandleMacros
  type Ann ix    HandleMacros = AnnHandleMacros ix

{-------------------------------------------------------------------------------
  Parsed macros
-------------------------------------------------------------------------------}

data CheckedMacro p =
    MacroType (Type p)
  | MacroExpr CheckedMacroExpr
  deriving stock (Show, Eq, Generic)

-- TODO: This is wrong, it does not allow name mangling to do its job.
data CheckedMacroExpr = CheckedMacroExpr{
      macroExprBody :: Macro.MExpr Macro.Ps
    , macroExprType :: Macro.Quant (Macro.Type Macro.Ty)
    }
  deriving stock (Show, Eq, Generic)

