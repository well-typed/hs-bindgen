module HsBindgen.Backend.Hs.Name (
    Name(..)
  , getName
    -- * Construction
  , unsafeHsIdHsName
  , unsafeDeclIdHsName
  , unsafeUniqueHsName
  ) where
import Data.Text qualified as Text

import HsBindgen.Backend.UniqueSymbol
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Imports
import HsBindgen.Language.Haskell
import HsBindgen.Language.Haskell qualified as Hs

-- | Haskell name in namespace @ns@
data Name (ns :: Namespace) =
      -- | Human-readable name that is to be exported
      --
      -- Term-level declarations with exported names should not have use sites
      -- and therefore they can easily be renamed.
      ExportedName Text
      -- | Auxiliary name used in the implementation of other declarations
      --
      -- Since those functions have use-sites, we should not normally rename them.
    | InternalName UniqueSymbol
  deriving stock (Eq, Ord, Show)

getName :: Name ns -> Text
getName = \case
  ExportedName x -> x
  InternalName x -> Text.pack x.unique

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Construct Haskell name in arbitrary name space
--
-- The caller must ensure that name rules are adhered to.
unsafeHsIdHsName ::  Hs.Identifier -> Name ns
unsafeHsIdHsName = ExportedName . getIdentifier

-- | Construct Haskell name in arbitrary name space
--
-- The caller must ensure that name rules are adhered to.
unsafeDeclIdHsName :: (HaskellId p ~ Hs.Identifier) => DeclId p -> Name ns
unsafeDeclIdHsName = unsafeHsIdHsName . haskellId

-- | Construct Haskell name from unique symbol
--
-- Caller must ensure that namespace rules are adhered to.
unsafeUniqueHsName :: UniqueSymbol -> Name ns
unsafeUniqueHsName = InternalName
