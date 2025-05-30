module HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass (
    ResolveBindingSpecs
  ) where

import HsBindgen.BindingSpecs qualified as BindingSpecs
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.RenameAnon
import HsBindgen.Frontend.Graph.UseDef (UseDefGraph)

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

type family AnnResolveBindingSpecs ix where
  AnnResolveBindingSpecs "Decl"            = Maybe BindingSpecs.Type
  AnnResolveBindingSpecs "TranslationUnit" = UseDefGraph Parse
  AnnResolveBindingSpecs "TypeTypedef"     = SquashedTypedef
  AnnResolveBindingSpecs _                 = NoAnn

instance IsPass ResolveBindingSpecs where
  type Id     ResolveBindingSpecs = CName
  type Macro  ResolveBindingSpecs = CheckedMacro
  type Ann ix ResolveBindingSpecs = AnnResolveBindingSpecs ix
