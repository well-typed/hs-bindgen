module HsBindgen.Frontend.NonSelectedDecls (
    -- * Type
    NonSelectedDecls(..)
    -- * API
  , empty
  , insert
  , lookup
  ) where

import Data.Map.Strict qualified as Map
import Prelude hiding (lookup)

import Clang.Paths (SourcePath)
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Type
-------------------------------------------------------------------------------}

-- | Declarations that are not selected during parsing
--
-- It is important to keep track of these non-selected declarations because they
-- can be given external bindings.  We do /not/ support external bindings for
-- /anonymous/ non-selected declarations; /if/ you want to provide an external
-- binding for some local type, for example
--
-- > struct rect {
-- >   struct { int x; int y; } bottomleft;
-- >   struct { int x; int y; } topright;
-- > };
--
-- then you need to make sure that you /traverse/ @rect@, so that the
-- @NameAnon@ pass can do its work.
newtype NonSelectedDecls = NonSelectedDecls {
      unNonSelectedDecls :: Map C.QualName SourcePath
    }
  deriving (Eq, Show)

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

empty :: NonSelectedDecls
empty = NonSelectedDecls Map.empty

insert ::
     C.QualName
  -> SourcePath
  -> NonSelectedDecls
  -> NonSelectedDecls
insert k v = NonSelectedDecls . Map.insert k v . unNonSelectedDecls

lookup :: C.QualName -> NonSelectedDecls -> Maybe SourcePath
lookup k = Map.lookup k . unNonSelectedDecls
