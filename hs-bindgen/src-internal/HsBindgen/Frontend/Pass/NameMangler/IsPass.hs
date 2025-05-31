module HsBindgen.Frontend.Pass.NameMangler.IsPass (
    NameMangler
    -- * Annotations
  , NamePair(..)
  , nameHs
  , RecordNames(..)
  , NewtypeNames(..)
  , DeclSpec(..)
  ) where

import HsBindgen.Frontend.AST.Internal (ValidPass, CName)
import HsBindgen.Frontend.Graph.UseDef (UseDefGraph)
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.RenameAnon
import HsBindgen.Imports
import HsBindgen.Language.Haskell
import HsBindgen.BindingSpecs

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | NameMangler
--
-- Name mangling depends on information from the binding specs, and must
-- therefore happen after 'ResolveBindingSpecs'.
type NameMangler :: Pass
data NameMangler a deriving anyclass (ValidPass)

type family AnnNameMangler ix where
  AnnNameMangler "Decl"            = DeclSpec
  AnnNameMangler "TranslationUnit" = UseDefGraph Parse
  AnnNameMangler "TypeTypedef"     = TypedefSquashed
  AnnNameMangler "Struct"          = RecordNames
  AnnNameMangler "Union"           = NewtypeNames
  AnnNameMangler "Enum"            = NewtypeNames
  AnnNameMangler "Typedef"         = NewtypeNames
  AnnNameMangler _                 = NoAnn

instance IsPass NameMangler where
  type Id        NameMangler = NamePair
  type FieldName NameMangler = NamePair
  type MacroBody NameMangler = CheckedMacro NameMangler
  type Ann ix    NameMangler = AnnNameMangler ix

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
  deriving stock (Show, Eq, Ord)

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
  deriving stock (Show, Eq)

-- | Names for a Haskell newtype
data NewtypeNames = NewtypeNames {
      newtypeConstr :: HsName NsConstr
    , newtypeField  :: HsName NsVar
    }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Information from the binding specs, minus naming information
-------------------------------------------------------------------------------}

data DeclSpec = DeclSpec {
      -- | Module the Haskell declaration should be placed in
      declSpecModule :: Maybe HsModuleName

      -- | Instances
    , declSpecInstances :: Map HsTypeClass (Omittable Instance)
    }
  deriving stock (Show, Eq)

