{-# LANGUAGE NamedFieldPuns #-}

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
  , ScopedNamePair(..)
  , DeclIdPair(..)
  , renameHsName
  , unsafeHsName
  , OptIdentifier (..)
  , optIdentifier
  , optNoIdentifier
  , Reason (..)
  ) where

import Data.Text qualified as Text
import GHC.Records (HasField (..))
import GHC.Stack (CallStack, callStack, prettyCallStack)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Errors (panicPure)
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

data ScopedNamePair = ScopedNamePair {
      cName  :: C.ScopedName
    , hsName :: Hs.Identifier
    }
  deriving stock (Show, Eq, Ord, Generic)

data DeclIdPair = DeclIdPair {
      cName :: DeclId
    , hsName :: OptIdentifier
    }
  deriving stock (Show, Eq, Ord)

renameHsName :: (Hs.Identifier -> Hs.Identifier) -> DeclIdPair -> DeclIdPair
renameHsName f DeclIdPair {cName, hsName} = DeclIdPair {
      cName
    , hsName = renameOptIdentifier f hsName
    }

instance HasField "unsafeHsName" DeclIdPair Hs.Identifier where
  getField = unsafeHsName

unsafeHsName :: DeclIdPair -> Hs.Identifier
unsafeHsName DeclIdPair {hsName} =
      case hsName of
        NoIdentifier cstack reason ->
          panicPure $ concat
            [ prettyReason reason, "\n"
            , "Call stack:\n"
            , prettyCallStack cstack
            ]
        Identifier x -> x

data OptIdentifier = Identifier Hs.Identifier | NoIdentifier CallStack Reason
  deriving stock Show

-- | Ignores the 'CallStack' field
instance Eq OptIdentifier where
  Identifier x == Identifier y = x == y
  NoIdentifier _ x == NoIdentifier _ y = x == y
  _ == _ = False

-- | Ignores the 'CallStack' field
instance Ord OptIdentifier where
  Identifier x `compare` Identifier y = x `compare` y
  Identifier{} `compare` NoIdentifier{} = GT
  NoIdentifier{} `compare` Identifier{} = LT
  NoIdentifier _ x `compare` NoIdentifier _ y = x `compare` y

optIdentifier :: Hs.Identifier -> OptIdentifier
optIdentifier = Identifier

optNoIdentifier :: HasCallStack => Reason -> OptIdentifier
optNoIdentifier = NoIdentifier callStack

renameOptIdentifier :: (Hs.Identifier -> Hs.Identifier) -> OptIdentifier -> OptIdentifier
renameOptIdentifier f = \case
    NoIdentifier cstack reason -> NoIdentifier cstack reason
    Identifier x -> Identifier (f x)

-- |A Haskell identifier is not available.
data Reason =
      -- | The C name of the declaration was not mangled because it only appears
      -- in an underlying type.
      --
      -- Mangling produces Haskell identifiers for the names of C declarations.
      -- Only the names of declarations that are selected are mangled, but
      -- references to unselected declarations can still appear in /underlying
      -- types/. See 'HsBindgen.Frontend.AST.Type.Ref' for info about underlying
      -- types. Haskell identifiers are not expected to be used in underlying
      -- types.
    UnderlyingTypeNotMangled
  deriving stock (Show, Eq, Ord)

-- | See 'Reason' for more information about the reasons.
prettyReason :: Reason -> String
prettyReason reason = "A haskell identifier is not avilable: " <> case reason of
    UnderlyingTypeNotMangled ->
      "The C name of the declaration was not mangled because it only appears \
      \in an underlying type."
