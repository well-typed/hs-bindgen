-- | Sort the trace messages of a pass by location
--
-- A pass whose messages are sorted on output emits them in the same order,
-- whatever the order it processes its declarations in.
--
-- Intended for unqualified import.
module HsBindgen.Frontend.SortMsgs (
    sortMsgs
    -- * Trace messages
  , SortMsgsMsg(..)
  ) where

import Data.List qualified as List
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Util.Tracer

-- | Sort messages by location (stable)
--
-- Messages without a location sort first; messages whose source is not in the
-- include graph sort after those that have a location.
--
-- We do not know where to sort a message whose source is not in the include
-- graph, but a mis-sorted message is no reason to abort: we report the bug, one
-- message per source, and sort the message to the back.
sortMsgs ::
     HasCallStack
  => IncludeGraph.IncludeOrder
  -> [WithCallStack (C.WithLocationInfo msg)]
  -> ([WithCallStack SortMsgsMsg], [WithCallStack (C.WithLocationInfo msg)])
sortMsgs order msgs = (bugMsgs, List.sortOn (msgSortKey order) msgs)
  where
    bugMsgs :: [WithCallStack SortMsgsMsg]
    bugMsgs = [
        withCallStack $ SortMsgsSourceNotInIncludeGraph path
      | path <- Set.toList unknownPaths
      ]

    unknownPaths :: Set RealPath
    unknownPaths = Set.fromList [
        path
      | msg <- msgs
      , Just loc <- [msgLoc msg]
      , C.InHeader path <- [singleLocPath loc]
      , IncludeGraph.lookupIncludeOrder order (C.InHeader path)
          == IncludeGraph.NotInIncludeGraph
      ]

-- | Location a trace message is sorted by
--
-- Only conflicting declarations carry more than one location; we sort by the
-- first.
msgLoc :: WithCallStack (C.WithLocationInfo msg) -> Maybe (SingleLoc C.DeclPath)
msgLoc msg = listToMaybe $ C.locationInfoLocs msg.traceMsg.loc

-- | Sort key of a trace message
--
-- The constructor order /is/ the specification: messages not attached to a
-- declaration sort to the front. Do not reorder.
data MsgSortKey =
    MsgNoLocation
  | MsgAt {
        msgSource :: IncludeGraph.IncludeOrderIx
      , msgLine   :: Int
      , msgColumn :: Int
      }
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

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data SortMsgsMsg =
    -- | The source of a trace message is not part of the include graph, so we
    -- do not know where to sort it.
    SortMsgsSourceNotInIncludeGraph RealPath
  deriving stock (Show)

instance PrettyForTrace SortMsgsMsg where
  prettyForTrace = \case
      SortMsgsSourceNotInIncludeGraph path -> PP.hsep [
          "Source not in include graph:"
        , PP.string $ getRealPath path
        ]

instance IsTrace Level SortMsgsMsg where
  getDefaultLogLevel = \case
    SortMsgsSourceNotInIncludeGraph{} -> Bug
  getSource  = const HsBindgen
  getTraceId = \case
    SortMsgsSourceNotInIncludeGraph{} -> "sort-msgs-source-not-in-include-graph"
