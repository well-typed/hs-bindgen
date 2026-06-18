-- | Declaration identifiers
--
-- This module should only be used within the @HsBindgen.IR@ hierarchy.  From
-- outside the @HsBindgen.IR@ hierarchy, "HsBindgen.IR.Pass" should be used.
--
-- Intended for unqualified import.
--
-- > import HsBindgen.IR.Pass.Id
module HsBindgen.IR.Pass.Id (
    -- * Associated type families
    PassId(..)
  ) where

import Clang.HighLevel.Types

import HsBindgen.Imports
import HsBindgen.IR.C.LocationInfo qualified as C
import HsBindgen.IR.C.Naming qualified as C
import HsBindgen.IR.Pass.Definition

{-------------------------------------------------------------------------------
  Associated type families
-------------------------------------------------------------------------------}

-- | Declaration identifiers vary across passes
class (
      Ord  (Id p)  -- For using as map key
    , Show (Id p)  -- For debugging
    ) => PassId (p :: Pass) where

  -- | Declaration identifier
  --
  -- 1. After 'HsBindgen.Frontend.Pass.Parse.IsPass.Parse', this is
  --   'PrelimDeclId'.  Anonymous declarations are assigned an ID based on
  --   source location.  For everything else, we use the C name.
  -- 2. After 'HsBindgen.Frontend.Pass.AssignAnonIds.IsPass.AssignAnonIds', this
  --   is 'C.DeclId'.  /Everything/ has a name, because we have assigned names
  --   to anonymous declarations.
  -- 3. After 'HsBindgen.Frontend.Pass.MangleNames.IsPass.MangleNames', this is
  --   'DeclIdPair'.
  type Id p :: Star
  type Id p = C.DeclId

  -- | Name kind of the C name
  idNameKind :: Proxy p -> Id p -> C.NameKind
  default idNameKind :: Id p ~ C.DeclId => Proxy p -> Id p -> C.NameKind
  idNameKind _ = (.name.kind)

  -- | Name of the declaration as it appears in the C source, if any
  idSourceName :: Proxy p -> Id p -> Maybe C.DeclName
  default idSourceName :: Id p ~ C.DeclId => Proxy p -> Id p -> Maybe C.DeclName
  idSourceName _ = C.declIdSourceName

  -- | Location information
  idLocationInfo :: Proxy p -> Id p -> [SingleLoc] -> C.LocationInfo
  default idLocationInfo ::
       Id p ~ C.DeclId
    => Proxy p -> Id p -> [SingleLoc] -> C.LocationInfo
  idLocationInfo _ = C.declIdLocationInfo
