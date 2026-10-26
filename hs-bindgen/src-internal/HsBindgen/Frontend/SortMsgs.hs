-- | Sort the trace messages of a pass by location
--
-- A pass that sorts its messages on output emits them in the same order,
-- whatever the order it processes its declarations in.
--
-- Intended for unqualified import.
module HsBindgen.Frontend.SortMsgs (
    sortMsgs
  , msgLoc
  ) where

import Data.List qualified as List
import Data.Maybe (listToMaybe)

import Clang.HighLevel.Types

import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.IR.C qualified as C
import HsBindgen.Util.Tracer

-- | Sort messages by location (stable)
--
-- Messages whose source is not in the include graph sort after those that
-- have a location; messages without a location sort last.
sortMsgs ::
     IncludeGraph.IncludeOrder
  -> [WithCallStack (C.WithLocationInfo msg)]
  -> [WithCallStack (C.WithLocationInfo msg)]
sortMsgs order = List.sortOn (msgSortKey order)

-- | Location a trace message is sorted by
--
-- Only conflicting declarations carry more than one location; we sort by the
-- first.
msgLoc :: WithCallStack (C.WithLocationInfo msg) -> Maybe (SingleLoc C.DeclPath)
msgLoc msg = listToMaybe $ C.locationInfoLocs msg.traceMsg.loc

-- | Sort key of a trace message
--
-- The constructor order /is/ the specification: messages not attached to a
-- declaration sort to the back. Do not reorder.
data MsgSortKey =
    MsgAt {
        msgSource :: IncludeGraph.IncludeOrderIx
      , msgLine   :: Int
      , msgColumn :: Int
      }
  | MsgNoLocation
  deriving stock (Eq, Ord)

msgSortKey ::
     IncludeGraph.IncludeOrder
  -> WithCallStack (C.WithLocationInfo msg)
  -> MsgSortKey
msgSortKey order msg = case msgLoc msg of
    Nothing  -> MsgNoLocation
    Just loc -> MsgAt {
        msgSource = IncludeGraph.lookupIncludeOrder order (singleLocPath loc)
      , msgLine   = singleLocLine   loc
      , msgColumn = singleLocColumn loc
      }
