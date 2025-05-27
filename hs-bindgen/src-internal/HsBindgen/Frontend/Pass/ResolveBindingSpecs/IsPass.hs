module HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass (
    ResolveBindingSpecs
  ) where

import HsBindgen.Frontend.AST
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.RenameAnon

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Resolve binding specifications
--
-- For every C name, we resolve
--
-- * External binding specifications ("we already have bindings for this").
--   Such declarations are removed from the AST.
-- * Input binding specifications
--   ("we want to adjust how to generate the binding for this").
type ResolveBindingSpecs :: Pass
data ResolveBindingSpecs a

instance IsPass ResolveBindingSpecs where

-- instance ShowPass ResolveBindingSpecs


