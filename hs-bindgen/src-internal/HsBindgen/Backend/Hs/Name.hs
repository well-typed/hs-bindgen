-- | Intended for qualified import
--
-- > import HsBindgen.Backend.Hs.Name qualified as Hs
module HsBindgen.Backend.Hs.Name (
    TermName(..)
    -- * Deconstruction
  , termNameToText
  , termNameToStr
  ) where
import Data.Text qualified as Text

import HsBindgen.Backend.UniqueSymbol
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Haskell term name
data TermName =
      -- | Human-readable name that is to be exported
      --
      -- Term-level declarations with exported names should not have use sites
      -- and therefore they can easily be renamed (@RenameTerm@).
      ExportedName (Hs.Name Hs.NsVar)

      -- | Auxiliary name used in the implementation of other declarations
      --
      -- Since those functions have use-sites, we should not normally rename
      -- them (@RenameTerm@).
    | InternalName UniqueSymbol
  deriving stock (Eq, Ord, Show)

{-------------------------------------------------------------------------------
  Deconstruction
-------------------------------------------------------------------------------}

termNameToText :: TermName -> Text
termNameToText = \case
  ExportedName x -> x.text
  InternalName x -> Text.pack x.unique

termNameToStr :: TermName -> String
termNameToStr = Text.unpack . termNameToText
