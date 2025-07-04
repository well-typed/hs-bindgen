module HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass (
    ResolveBindingSpec
  , ResolvedExtBinding(..)
  ) where

import HsBindgen.BindingSpec.Internal qualified as BindingSpec
import HsBindgen.Frontend.AST.Internal (CheckedMacro, ValidPass)
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Sort.IsPass (DeclMeta)
import HsBindgen.Imports
import HsBindgen.Language.C
import HsBindgen.Language.Haskell (ExtHsRef)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Resolve binding specification
--
-- For every C name, we resolve:
--
-- * External binding specification, which is used to specify existing bindings
--   that should be used, external from the module being generated.  Matching
--   types are replaced with external references, and matching declarations are
--   removed from the AST.
-- * Prescriptive binding specification, which is used to configure how bindings
--   are generated.  This information is added to the AST as annotations.
type ResolveBindingSpec :: Pass
data ResolveBindingSpec a deriving anyclass (ValidPass)

type family AnnResolveBindingSpec ix where
  AnnResolveBindingSpec "Decl"            = BindingSpec.TypeSpec
  AnnResolveBindingSpec "TranslationUnit" = DeclMeta
  AnnResolveBindingSpec _                 = NoAnn

instance IsPass ResolveBindingSpec where
  type Id         ResolveBindingSpec = CName
  type FieldName  ResolveBindingSpec = CName
  type TypedefRef ResolveBindingSpec = CName
  type MacroBody  ResolveBindingSpec = CheckedMacro ResolveBindingSpec
  type ExtBinding ResolveBindingSpec = ResolvedExtBinding
  type Ann ix     ResolveBindingSpec = AnnResolveBindingSpec ix

data ResolvedExtBinding = ResolvedExtBinding{
      -- | Name of the C declaration for which we are using this binding
      extCName :: QualName

      -- | The Haskell type which will be used
    , extHsRef :: ExtHsRef

      -- | Additional information about the Haskell type
    , extHsSpec :: BindingSpec.TypeSpec
    }
  deriving stock (Show, Eq, Generic)
