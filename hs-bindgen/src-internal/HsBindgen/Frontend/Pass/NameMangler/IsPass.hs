module HsBindgen.Frontend.Pass.NameMangler.IsPass (
    NameMangler
  , PairOfIds(..)
  ) where

import HsBindgen.Frontend.AST
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.RenameAnon
import HsBindgen.Frontend.Pass.ResolveBindingSpecs
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | NameMangler
--
-- Name mangling depends on information from the binding specs, and must
-- therefore happen after 'ResolveBindingSpecs'.
type NameMangler :: Pass
data NameMangler a

instance IsPass NameMangler where
  type Id       NameMangler = PairOfIds

-- instance ShowPass NameMangler

{-------------------------------------------------------------------------------
  Identifiers
-------------------------------------------------------------------------------}

data PairOfIds = PairOfIds {
      nameC       :: CName
    , nameHaskell :: Text
    }
  deriving stock (Show)

