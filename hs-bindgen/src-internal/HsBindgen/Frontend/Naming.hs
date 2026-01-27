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
  , unsafeHsName
  , AssignedIdentifier (..)
  , assignedIdentifier
  , noAssignedIdentifier
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
      -- appears in the C source; @hs-bindgen@ assigns names to declarations in
      -- the generated /Haskell/ code, and, in particular, does not rename the C
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
    , hsName :: AssignedIdentifier
    }
  deriving stock (Show, Eq, Ord)

instance HasField "unsafeHsName" DeclIdPair Hs.Identifier where
  getField = unsafeHsName

unsafeHsName :: HasCallStack => DeclIdPair -> Hs.Identifier
unsafeHsName dip =
      case dip.hsName of
        NoAssignedIdentifier cstack reason ->
          panicPure $ concat
            [ prettyReason reason, "\n"
            , "No identifier assigned at:\n"
            , prettyCallStack cstack
            ]
        AssignedIdentifier x -> x

data AssignedIdentifier =
    AssignedIdentifier Hs.Identifier
    -- | Haskell identifiers are not always assigned. See 'Reason'.
  | NoAssignedIdentifier CallStack Reason
  deriving stock Show

-- | Ignores the 'CallStack' field
instance Eq AssignedIdentifier where
  AssignedIdentifier x     == AssignedIdentifier y     = x == y
  NoAssignedIdentifier _ x == NoAssignedIdentifier _ y = x == y
  _                        == _                        = False

-- | Ignores the 'CallStack' field
instance Ord AssignedIdentifier where
  AssignedIdentifier x     `compare` AssignedIdentifier y     = x `compare` y
  AssignedIdentifier{}     `compare` NoAssignedIdentifier{}   = GT
  NoAssignedIdentifier{}   `compare` AssignedIdentifier{}     = LT
  NoAssignedIdentifier _ x `compare` NoAssignedIdentifier _ y = x `compare` y

assignedIdentifier :: Hs.Identifier -> AssignedIdentifier
assignedIdentifier = AssignedIdentifier

noAssignedIdentifier :: HasCallStack => Reason -> AssignedIdentifier
noAssignedIdentifier = NoAssignedIdentifier callStack

-- | A Haskell identifier is not available.
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
prettyReason reason = "A haskell identifier is not available: " <> case reason of
    UnderlyingTypeNotMangled ->
      "The C name of the declaration was not mangled because it only appears \
      \in an underlying type."
