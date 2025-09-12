-- | Main entry point for macro parsing
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Macro (MacroParseError(..), MacroTcError(..))
-- > import HsBindgen.Frontend.Macro qualified as Macro
--
-- TODO: This should move to c-expr-dsl.
module HsBindgen.Frontend.Macro (
    -- * Syntax
    Macro(..)
  , MExpr(..)
  , MTerm(..)
  , MFun(..)
    -- ** Passes
  , Pass
  , Ps
  , Tc
  , XVar(..)
  , XApp(..)
    -- * Parsing
  , MacroParseError(..)
  , runParser
  , parseExpr
    -- * Type-checking
  , TypeEnv
  , MacroTcError(..)
  , tcMacro
  , pprTcMacroError
    -- ** Type language
  , Quant(..)
  , QuantTyBody(..)
  , Type(..)
  , Kind(Ty, Ct)
  , TyCon(..)
  , DataTyCon(..)
  , ClassTyCon(..)
  , FamilyTyCon(..)
  , GenerativeTyCon(..)
  , IntegralType(..)
  , mkQuantTyBody
  , tyVarName
  , tyVarNames
    -- * Evaluation
  , FunValue(..)
  ) where

import HsBindgen.Frontend.Macro.Parse.Expr
import HsBindgen.Frontend.Macro.Parse.Infra
import HsBindgen.Frontend.Macro.Pass
import HsBindgen.Frontend.Macro.Syntax
import HsBindgen.Frontend.Macro.Tc
import HsBindgen.Frontend.Macro.Tc.Type
