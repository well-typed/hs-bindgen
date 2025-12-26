-- | C naming and declaration identifiers
--
-- Intended for qualified import within frontend.
--
-- > import HsBindgen.Frontend.Naming qualified as C
module HsBindgen.Frontend.Naming (
    -- * DeclId
    DeclId(..)
  , declIdSourceName
  , renderDeclId
  , parseDeclId

    -- * DeclIdPair
  , DeclIdPair(..)

    -- * ScopedNamePair
  , ScopedNamePair(..)
  ) where

import Data.Text qualified as Text
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer (PrettyForTrace (prettyForTrace))

{-------------------------------------------------------------------------------
  DeclId
-------------------------------------------------------------------------------}

-- | Identifier for a declaration that appears in the C source
data DeclId = DeclId{
      -- | Name of the declaration
      --
      -- For named (non-anonymous) declarations, this is /always/ the name as it
      -- appears in the C source; any renaming of declarations we do in
      -- @hs-bindgen@ happens in the generated /Haskell/ code, not the C
      -- declarations.
      --
      -- For anonymous declarations, this is the name as it is assigned by the
      -- @AssignAnonIds@ pass, which is also how we then refer to this
      -- declaration in binding specs. The user-facing syntax for anonymous
      -- declarations uses an \@-sign in the name; that is not present in the
      -- Haskell value.
      name :: C.DeclName

      -- | Is this declaration anonymous?
      --
      -- We do /NOT/ record the original anon ID here, because that is a source
      -- location, which is impossible to construct in many places (for example,
      -- when parsing @struct \@foo@ in binding specs).
    , isAnon :: Bool
    }
  deriving stock (Show, Eq, Ord)

declIdSourceName :: DeclId -> Maybe C.DeclName
declIdSourceName declId = do
    guard $ not declId.isAnon
    return declId.name

-- | User-facing syntax for 'DeclId'
renderDeclId :: DeclId -> Text
renderDeclId declId
  | declId.isAnon = C.renderDeclName $ C.mapDeclNameText ("@" <>) declId.name
  | otherwise     = C.renderDeclName declId.name

-- | Parse user-facing syntax for 'DeclId'
parseDeclId :: Text -> Maybe DeclId
parseDeclId t = do
    declName <- C.parseDeclName t
    return $ case Text.uncons declName.text of
      Just ('@', n) -> DeclId{name = C.DeclName n declName.kind, isAnon = True}
      _otherwise    -> DeclId{name = declName, isAnon = False}

instance PrettyForTrace DeclId where
  prettyForTrace = PP.singleQuotes . PP.text . renderDeclId

{-------------------------------------------------------------------------------
  DeclIdPair
-------------------------------------------------------------------------------}

data DeclIdPair = DeclIdPair{
      cName  :: DeclId
    , hsName :: Hs.Identifier
    }
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  ScopedNamePair
-------------------------------------------------------------------------------}

-- | Pair of a (scoped) C name and the corresponding Haskell name
--
-- Invariant: the 'Hs.Identifier' must satisfy the rules for legal Haskell
-- names, for its intended use (constructor, variable, ..).
data ScopedNamePair = ScopedNamePair {
      cName  :: C.ScopedName
    , hsName :: Hs.Identifier
    }
  deriving stock (Show, Eq, Ord, Generic)
