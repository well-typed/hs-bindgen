{-# LANGUAGE NoFieldSelectors  #-}
{-# LANGUAGE NoNamedFieldPuns  #-}
{-# LANGUAGE NoRecordWildCards #-}

-- | C naming and declaration identifiers
--
-- This is such a central module in @hs-bindgen@, it is intended for
-- /unqualified/ import.
--
-- (Historical note: we used to import this qualified as @C@, but that is
-- incorrect: these are @hs-bindgen@ concepts, /not/ standard C concepts. In
-- particular, the names we assign to anonymous declarations is very much
-- @hs-bindgen@ specific.)
module HsBindgen.Frontend.Naming (
    -- * DeclId
    DeclId(..)
  , declIdSourceName
  , renderDeclId
  , parseDeclId

    -- * Pairing C names and Haskell names
  , DeclIdPair(..)
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
--
-- This is the main ID used throughout @hs-bindgen@ for declarations.
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
  Pairing C names and Haskell names

  The Haskell name must satisfy the rules for legal Haskell names for the
  intended usage (constructor, variable, ..).
-------------------------------------------------------------------------------}

data DeclIdPair = DeclIdPair{
      cName  :: DeclId
    , hsName :: Hs.Identifier
    }
  deriving stock (Show, Eq, Ord)

data ScopedNamePair = ScopedNamePair {
      cName  :: C.ScopedName
    , hsName :: Hs.Identifier
    }
  deriving stock (Show, Eq, Ord, Generic)
