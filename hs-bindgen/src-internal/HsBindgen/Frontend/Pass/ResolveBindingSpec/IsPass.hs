module HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass (
    ResolveBindingSpec
  ) where

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.AST.Internal (ValidPass, CheckedMacro)
import HsBindgen.Frontend.Graph.UseDef (UseDefGraph)
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.RenameAnon
import HsBindgen.Language.C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Resolve binding specification
--
-- For every C name, we resolve
--
-- * External binding specification ("we already have bindings for this").
--   Such declarations are removed from the AST.
-- * Input binding specification
--   ("we want to adjust how to generate the binding for this").
type ResolveBindingSpec :: Pass
data ResolveBindingSpec a deriving anyclass (ValidPass)

type family AnnResolveBindingSpec ix where
  AnnResolveBindingSpec "Decl"            = BindingSpec.TypeSpec
  AnnResolveBindingSpec "TranslationUnit" = UseDefGraph Parse
  AnnResolveBindingSpec "TypeTypedef"     = TypedefSquashed
  AnnResolveBindingSpec _                 = NoAnn

instance IsPass ResolveBindingSpec where
  type Id        ResolveBindingSpec = CName
  type FieldName ResolveBindingSpec = CName
  type MacroBody ResolveBindingSpec = CheckedMacro ResolveBindingSpec
  type Ann ix    ResolveBindingSpec = AnnResolveBindingSpec ix
