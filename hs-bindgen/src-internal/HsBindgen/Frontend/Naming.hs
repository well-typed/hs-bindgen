module HsBindgen.Frontend.Naming (
    -- * Located
    Located(..)

    -- * AnonId
  , AnonId(..)

    -- * NameOrigin
  , NameOrigin(..)

    -- * DeclId
  , DeclId(..)
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
  Located
-------------------------------------------------------------------------------}

-- | Indirection for 'PrettyForTrace' instance for @DeclInfo@
--
-- By introducting this auxiliary type, used in the 'PrettyForTrace' instance
-- for @DeclInfo@, we delegate to @Id p@ instances.
data Located a = Located SingleLoc a

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

    -- | Name is a Clang built-in
  | NameOriginBuiltin
  deriving stock (Show, Eq, Ord, Generic)

instance PrettyForTrace NameOrigin where
  prettyForTrace = \case
    NameOriginInSource ->
      PP.string "in source"
    NameOriginGenerated anonId ->
      PP.string "generated for" <+> prettyForTrace anonId
    NameOriginRenamedFrom name ->
      PP.string "renamed from" <+> prettyForTrace name
    NameOriginBuiltin ->
      PP.string "built-in"

{-------------------------------------------------------------------------------
  DeclId
-------------------------------------------------------------------------------}

-- | Declaration identity
--
-- All declarations have names after renaming in the @NameAnon@ pass.  This type
-- is used until the @MangleNames@ pass.
data DeclId = DeclId {
      declIdName   :: C.Name
    , declIdOrigin :: NameOrigin
    }
  deriving stock (Show, Eq, Ord, Generic)

instance PrettyForTrace DeclId where
  prettyForTrace DeclId{..} =
    prettyForTrace declIdName <+> PP.parens (prettyForTrace declIdOrigin)

instance PrettyForTrace (Located DeclId) where
  prettyForTrace (Located loc DeclId{..}) =
    let details = case declIdOrigin of
          NameOriginBuiltin -> prettyForTrace declIdOrigin
          _otherwise -> PP.hsep [
              prettyForTrace declIdOrigin
            , "at"
            , PP.showToCtxDoc loc
            ]
    in  prettyForTrace declIdName <+> PP.parens details
