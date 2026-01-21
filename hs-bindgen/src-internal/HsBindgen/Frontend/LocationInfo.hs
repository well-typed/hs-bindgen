-- | Trace messages with location information
module HsBindgen.Frontend.LocationInfo (
    WithLocationInfo(..)
    -- * Info
  , LocationInfo(..)
    -- ** Construction
  , prelimDeclIdLocationInfo
  , declIdLocationInfo
    -- ** Query
  , locationInfoName
  , locationInfoLocs
  ) where

import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel.Types

import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (PrelimDeclId)
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId qualified as PrelimDeclId
import HsBindgen.Language.C qualified as C
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Trace message with location information
data WithLocationInfo a = WithLocationInfo{
      loc :: LocationInfo
    , msg :: a
    }
  deriving stock (Show)

instance IsTrace lvl a => IsTrace lvl (WithLocationInfo a) where
  getDefaultLogLevel x = getDefaultLogLevel x.msg
  getSource          x = getSource          x.msg
  getTraceId         x = getTraceId         x.msg

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
  | LocationDeclAnon (Maybe C.DeclName) [SingleLoc]

    -- | No location information
  | LocationUnavailable
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Construct 'LocationInfo'
-------------------------------------------------------------------------------}

prelimDeclIdLocationInfo :: PrelimDeclId -> [SingleLoc] -> LocationInfo
prelimDeclIdLocationInfo prelimDeclId knownLocs =
    case prelimDeclId of
      PrelimDeclId.Named name -> LocationDeclNamed name knownLocs
      PrelimDeclId.Anon  anon -> LocationDeclAnon Nothing [anon.loc]

declIdLocationInfo :: DeclId -> [SingleLoc] -> LocationInfo
declIdLocationInfo declId knownLocs =
    if not declId.isAnon
      then LocationDeclNamed declId.name knownLocs
      else LocationDeclAnon (Just declId.name) knownLocs

{-------------------------------------------------------------------------------
  Query 'LocationInfo'
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
  Pretty-printing
-------------------------------------------------------------------------------}

instance PrettyForTrace a => PrettyForTrace (WithLocationInfo a) where
  prettyForTrace x =
      case x.loc of
        LocationUnavailable ->
          prettyForTrace x.msg
        _otherwise ->
          PP.hang (prettyForTrace x.loc) 2 $
            prettyForTrace x.msg

instance PrettyForTrace LocationInfo where
  prettyForTrace = \case
      LocationDeclNamed name [] -> PP.hsep [
          prettyForTrace name
        , "(location unavailable)"
        ]
      LocationDeclNamed name [loc] -> PP.hsep [
          prettyForTrace name
        , "at"
        , PP.show loc
        ]
      LocationDeclNamed name locs -> PP.hsep [
          prettyForTrace name
        , "at"
        , PP.hlist "(" ")" (map PP.show locs)
        ]
      LocationDeclAnon (Just name) loc -> PP.hsep [
          "anonymous declaration"
        , prettyForTrace name
        , "at"
        , PP.show loc
        ]
      LocationDeclAnon Nothing loc -> PP.hsep [
          "anonymous declaration at"
        , PP.show loc
        ]
      LocationUnavailable ->
        "location unavailable"
