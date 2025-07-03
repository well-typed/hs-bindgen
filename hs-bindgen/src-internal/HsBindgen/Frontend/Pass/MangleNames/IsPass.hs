module HsBindgen.Frontend.Pass.MangleNames.IsPass (
    NameMangler
    -- * Annotations
  , NamePair(..)
  , nameHs
  , RecordNames(..)
  , NewtypeNames(..)
  , DeclSpec(..)
  ) where

import HsBindgen.BindingSpec.Internal qualified as BindingSpec
import HsBindgen.Frontend.AST.Internal (CheckedMacro, ValidPass)
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.HandleTypedefs.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass (ResolvedExtBinding)
import HsBindgen.Frontend.Pass.Sort.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C
import HsBindgen.Language.Haskell

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | NameMangler
type NameMangler :: Pass
data NameMangler a deriving anyclass (ValidPass)

type family AnnNameMangler ix where
  AnnNameMangler "Decl"             = DeclSpec
  AnnNameMangler "TranslationUnit"  = DeclMeta
  AnnNameMangler "Struct"           = RecordNames
  AnnNameMangler "Union"            = NewtypeNames
  AnnNameMangler "Enum"             = NewtypeNames
  AnnNameMangler "Typedef"          = NewtypeNames
  AnnNameMangler "CheckedMacroType" = NewtypeNames
  AnnNameMangler _                  = NoAnn

instance IsPass NameMangler where
  type Id         NameMangler = NamePair
  type FieldName  NameMangler = NamePair
  type TypedefRef NameMangler = RenamedTypedefRef NameMangler
  type MacroBody  NameMangler = CheckedMacro NameMangler
  type ExtBinding NameMangler = ResolvedExtBinding
  type Ann ix     NameMangler = AnnNameMangler ix

{-------------------------------------------------------------------------------
  Identifiers
-------------------------------------------------------------------------------}

-- | Pair of a C name and the corresponding Haskell name
--
-- Invariant: the 'HsIdentifier' must satisfy the rules for legal Haskell names,
-- for its intended use (constructor, variable, ..).
data NamePair = NamePair {
      nameC       :: CName
    , nameHsIdent :: HsIdentifier
    }
  deriving stock (Show, Eq, Ord, Generic)

-- | Extract namespaced Haskell name
--
-- The invariant on 'NamePair' justifies this otherwise unsafe operation.
nameHs :: NamePair -> HsName ns
nameHs NamePair{nameHsIdent = HsIdentifier name} = HsName name

{-------------------------------------------------------------------------------
  Additional names

  This is in addition to the 'NamePair's already embedded in the AST.
-------------------------------------------------------------------------------}

-- | Names for a Haskell record type
data RecordNames = RecordNames {
      recordConstr :: HsName NsConstr
    }
  deriving stock (Show, Eq, Generic)

-- | Names for a Haskell newtype
data NewtypeNames = NewtypeNames {
      newtypeConstr :: HsName NsConstr
    , newtypeField  :: HsName NsVar
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Information from the binding spec, minus naming information
-------------------------------------------------------------------------------}

-- | Binding specification for this declaration
--
-- Although we have interpreted /part/ of this binding specification during
-- name mangling, we leave the /full/ binding specification in the AST, because
-- we need it when we  /generate/ the output binding specification.
--
-- TODO: This is not quite right: we should distinguish between binding
-- specifications for different classes of things (declarations of types,
-- functions, etc.). When we do, we should not associate them with the top-level
-- 'Decl' but instead with specific 'DeclKind's. When we change this, this will
-- have consequences for "Hs.Origin" also.
newtype DeclSpec = DeclSpec BindingSpec.TypeSpec
  deriving stock (Show, Eq, Generic)
