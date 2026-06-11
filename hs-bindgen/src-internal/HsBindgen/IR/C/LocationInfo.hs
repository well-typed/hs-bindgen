-- | Trace messages with location information
--
-- This module should only be used within the @HsBindgen.IR@ hierarchy.  From
-- outside the @HsBindgen.IR@ hierarchy, "HsBindgen.IR.C" should be used.
--
-- Within @HsBindgen.IR@, all modules aside from "HsBindgen.IR.C" should import
-- this module qualified for consistency.
--
-- > import HsBindgen.IR.C.LocationInfo qualified as C
module HsBindgen.IR.C.LocationInfo (
    WithLocationInfo(..)
    -- * Location info
  , LocationInfo(..)
    -- ** Construction
  , prelimDeclIdLocationInfo
  , declIdLocationInfo
    -- ** Query
  , locationInfoName
  , locationInfoLocs
    -- * Declaration locations
  , DeclLocs(..)
  , declLocsMin
  , declLocsToList
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Text.SimplePrettyPrint (CtxDoc, (><))
import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel.Types

import HsBindgen.IR.C.Naming qualified as C
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Trace message with location information
data WithLocationInfo a = WithLocationInfo{
      loc :: LocationInfo
    , msg :: a
    }
  deriving stock (Eq, Ord, Show, Functor)

instance IsTrace lvl a => IsTrace lvl (WithLocationInfo a) where
  getDefaultLogLevel x = getDefaultLogLevel x.msg
  getSource          x = getSource          x.msg
  getTraceId         x = getTraceId         x.msg

instance PrettyForTrace a => PrettyForTrace (WithLocationInfo a) where
  prettyForTrace x =
      case x.loc of
        LocationUnavailable ->
          prettyForTrace x.msg
        _otherwise ->
          PP.hang (prettyForTrace x.loc >< ":") 2 $
            prettyForTrace x.msg

{-------------------------------------------------------------------------------
  Location info
-------------------------------------------------------------------------------}

data LocationInfo =
    -- | Message about a named declaration in the C source
    --
    -- Usually we expect the list of locations to be a singleton: the location
    -- of the declaration.
    LocationDeclNamed C.DeclName [SingleLoc]

    -- | Message about an anonymous declaration
    --
    -- We record the /assigned/ name, /if/ it is available.
    --
    -- Usually we expect the list of locations to be a singleton: the location
    -- of the declaration.
  | LocationDeclAnon (Maybe C.DeclName) [SingleLoc]

    -- | No location information
  | LocationUnavailable
  deriving stock (Eq, Ord, Show)

instance PrettyForTrace LocationInfo where
  prettyForTrace = \case
      LocationDeclNamed name locs -> PP.hsep [
          prettyForTrace name
        , "at"
        , prettyLocs locs
        ]
      LocationDeclAnon (Just name) locs -> PP.hsep [
          "anonymous declaration"
        , prettyForTrace name
        , "at"
        , prettyLocs locs
        ]
      LocationDeclAnon Nothing locs -> PP.hsep [
          "anonymous declaration at"
        , prettyLocs locs
        ]
      LocationUnavailable ->
        "location unavailable"
    where
      prettyLocs :: Show a => [a] -> CtxDoc
      prettyLocs = \case
        []    -> "(source location unavailable)"
        [loc] -> PP.show loc
        locs  -> PP.hlist "(" ")" (map PP.show locs)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

prelimDeclIdLocationInfo :: C.PrelimDeclId -> [SingleLoc] -> LocationInfo
prelimDeclIdLocationInfo prelimDeclId knownLocs =
    case prelimDeclId of
      C.PrelimDeclIdNamed name -> LocationDeclNamed name knownLocs
      C.PrelimDeclIdAnon  anon -> LocationDeclAnon Nothing [anon.loc]

declIdLocationInfo :: C.DeclId -> [SingleLoc] -> LocationInfo
declIdLocationInfo declId knownLocs =
    if not declId.isAnon
      then LocationDeclNamed declId.name knownLocs
      else LocationDeclAnon (Just declId.name) knownLocs

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

locationInfoName :: LocationInfo -> Maybe C.DeclName
locationInfoName = \case
    LocationDeclNamed name _ -> Just name
    LocationDeclAnon mName _ -> mName
    LocationUnavailable      -> Nothing

locationInfoLocs :: LocationInfo -> [SingleLoc]
locationInfoLocs = \case
    LocationDeclNamed _ locs -> locs
    LocationDeclAnon _ locs  -> locs
    LocationUnavailable      -> []

{-------------------------------------------------------------------------------
  Declaration locations
-------------------------------------------------------------------------------}

-- | Source location(s) of a declaration.
--
-- Most declarations have a single source location; only conflicting
-- declarations carry more than one.
data DeclLocs =
    -- | The declaration has a single source location.
    DeclLoc SingleLoc
    -- | Conflicting declarations, each with its own source location.
  | DeclLocsConflict (NonEmpty SingleLoc)
  deriving stock (Eq, Ord, Show)

-- | Attempts to get the “minimum” location.
--
-- This is only meaningful if the locations share the same source path.
-- Comparisons across source paths happen in lexicographical order.
declLocsMin :: DeclLocs -> SingleLoc
declLocsMin = \case
  DeclLoc x           -> x
  DeclLocsConflict xs -> minimum xs

-- | All source locations, discarding the single\/conflict distinction.
declLocsToList :: DeclLocs -> [SingleLoc]
declLocsToList = \case
    DeclLoc          loc  -> [loc]
    DeclLocsConflict locs -> NonEmpty.toList locs
