module HsBindgen.Frontend.Pass.Parse.ParseStatus (
    -- * Type
    ParseStatus(..)
  , DeclStatus(..)
    -- * API
  , empty
  , insert
  , lookup
  )
where

import Data.Map.Strict qualified as Map
import Prelude hiding (lookup)

import Clang.HighLevel.Types
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Type
-------------------------------------------------------------------------------}

-- | Parse status of all declarations that are parsed by @libclang@ and provided
-- to @hs-bindgen@.
newtype ParseStatus = ParseStatus {
    unParseStatus :: Map C.QualPrelimDeclId (SingleLoc, DeclStatus)
  }
  deriving (Show, Eq, Ord)

-- | It is important to keep track of the parse and reification status of
-- declarations.
--
data DeclStatus =
    -- | Declarations that do not match the parse predicate.
    --
    -- For example, we may provide external bindings for skipped declarations.
    -- We do /not/ support external bindings for /anonymous/ non-parsed
    -- declarations; /if/ you want to provide an external binding for some local
    -- type, for example
    --
    -- > struct rect {
    -- >   struct { int x; int y; } bottomleft;
    -- >   struct { int x; int y; } topright;
    -- > };
    --
    -- then you need to make sure that you /traverse/ @rect@, so that the
    -- @NameAnon@ pass can do its work.
    ParseSkipped

    -- | Happy case ;-). Declarations that match the parse predicate and that we
    -- succeed to parse and reify.
  | ParseSucceeded [ParseMsg]

    -- | Declarations that match the parse predicate and that we fail to parse
    -- and reify.
    --
    -- The generation of bindings will fail if we happen to select failed
    -- bindings.
  | ParseFailed    [ParseMsg]
  deriving (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

empty :: ParseStatus
empty = ParseStatus Map.empty

insert ::
     C.QualPrelimDeclId
  -> SingleLoc
  -> DeclStatus
  -> ParseStatus
  -> ParseStatus
insert k x y = ParseStatus . Map.insert k (x, y) . unParseStatus

lookup :: C.QualPrelimDeclId -> ParseStatus -> Maybe (SingleLoc, DeclStatus)
lookup k = Map.lookup k . unParseStatus
