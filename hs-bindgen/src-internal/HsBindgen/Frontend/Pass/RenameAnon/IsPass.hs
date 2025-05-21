module HsBindgen.Frontend.Pass.RenameAnon.IsPass (
    RenameAnon
  , CName(..)
  ) where

import HsBindgen.Frontend.AST
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Pass definition
-------------------------------------------------------------------------------}

type RenameAnon :: Pass
data RenameAnon a

instance IsPass RenameAnon where
  type Previous RenameAnon = HandleMacros
  type Id       RenameAnon = CName

instance ShowPass RenameAnon

{-------------------------------------------------------------------------------
  Identity
-------------------------------------------------------------------------------}

newtype CName = CName Text
  deriving newtype (Show, IsString, Semigroup)

