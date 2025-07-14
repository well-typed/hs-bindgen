module HsBindgen.Frontend.Naming (
    -- * AnonId
    AnonId(..)

    -- * NameOrigin
  , NameOrigin(..)
  ) where

import Clang.HighLevel (ShowFile(..))
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Util.Tracer (PrettyForTrace (prettyForTrace))
import Text.SimplePrettyPrint ((<+>))
import Text.SimplePrettyPrint qualified as PP

{-------------------------------------------------------------------------------
  AnonId
-------------------------------------------------------------------------------}

-- | Identity of an anonymous declaration
newtype AnonId = AnonId SingleLoc
  deriving stock (Show, Eq, Ord, Generic)

-- | We mimick the syntax used by clang itself for anonymous declarations
instance PrettyForTrace AnonId where
  prettyForTrace (AnonId loc) = PP.string $
    "unnamed at " ++ HighLevel.prettySingleLoc ShowFile loc

{-------------------------------------------------------------------------------
  NameOrigin
-------------------------------------------------------------------------------}

-- | C name origin
--
-- This type describes the origin of a C name.
data NameOrigin =
    -- | Name in source
    --
    -- The name may be used to construct a valid C type.
    NameOriginInSource

    -- | Name is generated
    --
    -- The name may not be used to construct a valid C type.
  | NameOriginGenerated AnonId

    -- | Name is renamed
    --
    -- The name may not be used to construct a valid C type, but this original
    -- name may be used to construct a valid C type.
  | NameOriginRenamedFrom C.Name
  deriving stock (Show, Eq, Ord, Generic)

instance PrettyForTrace NameOrigin where
  prettyForTrace = \case
    NameOriginInSource ->
      PP.string "in source"
    NameOriginGenerated anonId ->
      PP.string "generated for" <+> prettyForTrace anonId
    NameOriginRenamedFrom name ->
      PP.string "renamed from" <+> prettyForTrace name
