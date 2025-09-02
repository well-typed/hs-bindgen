module HsBindgen.Frontend.NonParsedDecls (
    -- * Type
    NonParsedDecls(..)
    -- * API
  , empty
  , insert
  , lookup
  ) where

import Data.Map.Strict qualified as Map
import Prelude hiding (lookup)

import Clang.Paths (SourcePath)
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Type
-------------------------------------------------------------------------------}

-- | Declarations that are not parsed
--
-- It is important to keep track of these declarations because they can be given
-- external bindings.  We do /not/ support external bindings for /anonymous/
-- non-parsed declarations; /if/ you want to provide an external binding for
-- some local type, for example
--
-- > struct rect {
-- >   struct { int x; int y; } bottomleft;
-- >   struct { int x; int y; } topright;
-- > };
--
-- then you need to make sure that you /traverse/ @rect@, so that the
-- @NameAnon@ pass can do its work.
newtype NonParsedDecls = NonParsedDecls {
      unNonParsedDecls :: Map C.QualName SourcePath
    }
  deriving (Show, Eq)

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

empty :: NonParsedDecls
empty = NonParsedDecls Map.empty

insert ::
     C.QualName
  -> SourcePath
  -> NonParsedDecls
  -> NonParsedDecls
insert k v = NonParsedDecls . Map.insert k v . unNonParsedDecls

lookup :: C.QualName -> NonParsedDecls -> Maybe SourcePath
lookup k = Map.lookup k . unNonParsedDecls
