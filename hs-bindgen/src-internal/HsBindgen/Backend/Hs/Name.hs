module HsBindgen.Backend.Hs.Name (
    Name(..)
  , ExportedName(..)
  , getName
    -- * Construction
  , unsafeHsIdHsName
  , unsafeUniqueHsName
  ) where
import Data.Text qualified as Text

import HsBindgen.Backend.UniqueSymbol
import HsBindgen.Imports
import HsBindgen.Language.Haskell
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Haskell name in namespace @ns@
data Name (ns :: Namespace) =
      -- | Human-readable name that is to be exported
      --
      -- Term-level declarations with exported names should not have use sites
      -- and therefore they can easily be renamed ('RenameTerm').
      ExportedName (ExportedName ns)

      -- | Auxiliary name used in the implementation of other declarations
      --
      -- Since those functions have use-sites, we should not normally rename
      -- them ('RenameTerm').
    | InternalName UniqueSymbol
  deriving stock (Eq, Ord, Show)

-- | Exported name
--
-- The constructor is marked @Unsafe@ because callers need to ensure that
-- namespacing rules are adhered to.
newtype ExportedName (ns :: Namespace) = UnsafeExportedName {
      text :: Text
    }
  deriving newtype (Eq, Ord, Show, IsString)

getName :: Name ns -> Text
getName = \case
  ExportedName x -> x.text
  InternalName x -> Text.pack x.unique

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Construct Haskell name in arbitrary name space
--
-- The caller must ensure that name rules are adhered to.
unsafeHsIdHsName ::  Hs.Identifier -> Name ns
unsafeHsIdHsName ident = ExportedName $ UnsafeExportedName ident.text

-- | Construct Haskell name from unique symbol
--
-- Caller must ensure that namespace rules are adhered to.
unsafeUniqueHsName :: UniqueSymbol -> Name ns
unsafeUniqueHsName = InternalName
