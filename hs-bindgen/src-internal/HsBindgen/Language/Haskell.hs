-- | General Haskell language types
--
-- Intended for qualified import.
--
-- > import HsBindgen.Language.Haskell qualified as Hs
module HsBindgen.Language.Haskell (
    -- * Module names
    ModuleName(..)
  , moduleNameFromString
  , moduleNameToString
  , moduleNamePath
    -- * Module imports
  , Import(..)
    -- * References
  , ExtRef(..)
  , Ref(..)
    -- * Namespaces
  , Namespace(..)
  , SNamespace(..)
  , namespaceOf
  , SingNamespace(..)
    -- * Namespaced names
  , Name(..)
  , nameToStr
  , SomeName(..)
  , demoteNs
  , assertNs
  ) where

import Data.Foldable qualified as Foldable
import Data.Text qualified as Text
import System.FilePath
import Text.SimplePrettyPrint ((><))
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Module names
-------------------------------------------------------------------------------}

-- | Haskell module name
--
-- Example: @HsBindgen.Runtime.LibC@
newtype ModuleName = ModuleName { text :: Text }
  deriving stock (Generic)
  -- 'Show' instance valid due to 'IsString' instance
  deriving newtype (Eq, IsString, Ord, Show)

moduleNameFromString :: String -> ModuleName
moduleNameFromString = ModuleName . Text.pack

moduleNameToString :: ModuleName -> String
moduleNameToString moduleName = Text.unpack moduleName.text

moduleNamePath :: ModuleName -> FilePath
moduleNamePath moduleName = withoutExt <.> "hs"
  where
    withoutExt :: FilePath
    withoutExt =
        Foldable.foldl' (</>) "" $
          map Text.unpack (Text.splitOn "." moduleName.text)

instance PrettyForTrace ModuleName where
  prettyForTrace moduleName = PP.text moduleName.text

{-------------------------------------------------------------------------------
  Module imports
-------------------------------------------------------------------------------}

-- | A qualified or unqualified import of a module
data Import =
    -- | The symbol has been imported implicitly from the Haskell "Prelude"
    --
    -- We require an implicit prelude (1) for TH use and (2) the type equality
    -- operator @(~)@ depends on it.
    --
    -- In detail: On GHC versions <= 9.2, type equality @(~)@ is a magic
    -- built-in syntax, while on later GHC versions it is a proper type operator
    -- that has to be imported from 'Prelude' or some other module from the
    -- @base@ package.
    ImplicitPrelude
  | UnqualifiedImport ModuleName
    -- | Qualified import possibly with an alias
  | QualifiedImport   ModuleName (Maybe String)
  deriving (Eq, Ord, Show)

{-------------------------------------------------------------------------------
  References
-------------------------------------------------------------------------------}

-- | External reference
data ExtRef = ExtRef {
      moduleName :: ModuleName
      -- TODO https://github.com/well-typed/hs-bindgen/issues/423
      --
      -- At the moment, we only handle external references to types types. Later
      -- we may support external references to variables (e.g., in macros).
    , name       :: Name NsTypeConstr
    }
  deriving stock (Eq, Generic, Ord, Show)

-- | Reference
data Ref =
    -- | Reference to a name in the local scope
    RefLocal SomeName

  | -- | Reference to a type in a different module
    RefExt ExtRef
  deriving stock (Eq, Show)

{-------------------------------------------------------------------------------
  Namespaces
-------------------------------------------------------------------------------}

-- | Namespace
--
-- See section 1.4, "Namespaces" of the Haskell report
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch1.html#x6-130001.4>
data Namespace =
    NsTypeConstr
  | NsConstr
  | NsVar
  deriving (Eq, Ord, Show)

instance PrettyForTrace Namespace where
  prettyForTrace = \case
    NsTypeConstr -> "type constructor"
    NsConstr     -> "data constructor"
    NsVar        -> "variable"

-- | Namespace singleton
data SNamespace :: Namespace -> Star where
  SNsTypeConstr :: SNamespace 'NsTypeConstr
  SNsConstr     :: SNamespace 'NsConstr
  SNsVar        :: SNamespace 'NsVar

deriving stock instance Show (SNamespace ns)

-- | Get the namespace of a namespace singleton
namespaceOf :: SNamespace ns -> Namespace
namespaceOf = \case
    SNsTypeConstr -> NsTypeConstr
    SNsConstr     -> NsConstr
    SNsVar        -> NsVar

-- | Namespace singleton class
class SingNamespace ns where
  singNamespace :: SNamespace ns

instance SingNamespace 'NsTypeConstr where singNamespace = SNsTypeConstr
instance SingNamespace 'NsConstr     where singNamespace = SNsConstr
instance SingNamespace 'NsVar        where singNamespace = SNsVar

{-------------------------------------------------------------------------------
  Namespaced names
-------------------------------------------------------------------------------}

-- | Haskell name created by "HsBindgen.Config.MangleCandidate" with correctness
-- guarantees.
--
-- Also stores information about the Haskell namespace on the type level.
-- Useful locally, to ensure correctness on the type level.
--
-- The constructor is "unsafe", because only the name mangler can ensure that
-- namespace-specific naming rules are honored.
newtype Name (ns :: Namespace) = UnsafeName {
      text :: Text
    }
    deriving stock   (Show, Eq, Ord, Generic)

nameToStr :: Name ns -> String
nameToStr n = Text.unpack n.text

instance SingNamespace ns => PrettyForTrace (Name ns) where
  prettyForTrace x = prettyForTrace $ UnsafeSomeName ns x.text
    where
      ns :: Namespace
      ns = namespaceOf (singNamespace @ns)

-- | Haskell names with information about the Haskell namespace on the value
--   level. Useful when handling names more globally such as in the list of
--   declarations.
--
-- The constructor is "unsafe", because only the name mangler can ensure that
-- namespace-specific naming rules are honored.
data SomeName = UnsafeSomeName {
      ns   :: Namespace
    , text :: Text
    }
  deriving stock (Show, Eq, Ord, Generic)

instance PrettyForTrace SomeName where
  prettyForTrace x = PP.hsep [
      PP.text x.text
    , "(" >< prettyForTrace x.ns >< ")"
    ]

-- | Remove the type-level namespace information, and store it on the value
--   level.
demoteNs :: forall ns. SingNamespace ns => Name ns -> SomeName
demoteNs name = UnsafeSomeName (namespaceOf $ singNamespace @ns) name.text

-- | Get Haskell name for some name in the expected namespace
--
-- Panics if the stored namespace does not match the expected one.
assertNs ::
     forall ns. (HasCallStack, SingNamespace ns)
  => Proxy ns -> SomeName -> Name ns
assertNs _ nm
    | nm.ns == expected = UnsafeName nm.text
    | otherwise = panicPure $
        "assertNs: namespace mismatch; expected "
          <> show expected <> " but got " <> show nm.ns
  where
    expected :: Namespace
    expected = namespaceOf (singNamespace @ns)
