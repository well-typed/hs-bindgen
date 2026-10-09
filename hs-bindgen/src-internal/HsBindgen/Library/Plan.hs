-- | Which modules a library is generated as, and in which order
--
-- This module knows nothing about C: a header and a declaration can be
-- anything that can be ordered.
--
-- Intended for qualified import.
--
-- > import HsBindgen.Library.Plan (Module (..), Uses (..))
-- > import HsBindgen.Library.Plan qualified as Plan
module HsBindgen.Library.Plan (
    Uses(..)
  , Module(..)
  , plan
  ) where

import Data.Graph qualified as Graph
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set qualified as Set

import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | What a library is planned from
data Uses header decl = Uses {
      -- | The header that owns each declaration
      --
      -- A declaration that is missing here belongs to no header.
      owner :: Map decl header

      -- | What the given declarations use, directly or indirectly
      --
      -- The result includes the given declarations.
    , uses :: Set decl -> Set decl

      -- | The headers that a header includes, directly or indirectly
    , includes :: header -> Set header

      -- | The headers in include order
    , order :: [header]
    }

-- | A module of the library
data Module header decl = Module {
      -- | The headers that the module is generated for
      --
      -- The first one gives the module its name.  The others follow in include
      -- order.
      headers :: NonEmpty header

      -- | The declarations that these headers own
    , decls :: Set decl

      -- | The modules that this one uses, each given by its first header
    , uses :: Set header
    }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Planning
-------------------------------------------------------------------------------}

-- | The modules of a library, in the order in which they are to be generated
--
-- The order comes from the uses between headers, not from the includes.  A
-- header /uses/ another if a declaration that it owns uses, directly or
-- indirectly, a declaration that the other one owns.  In
--
-- > // draw.h                        // shape.h
-- > struct shape;                    struct shape { int sides; };
-- > int draw(struct shape *s);
--
-- neither header includes the other, but draw.h uses shape.h, so the module of
-- shape.h comes first and @draw@ takes a pointer to the @Shape@ it defines.
--
-- Haskell modules cannot import each other, so headers that use each other
-- share a module, and headers that only include each other do not:
--
-- * a.h and b.h include each other, with unrelated structs: two modules.
-- * node.h includes list.h, and each struct points to the other: one module.
-- * window.h and widget.h include nothing of each other, but a function of
--   each takes the struct of the other: one module.  No declaration is on a
--   loop; the headers are.
--
-- The rules, in full:
--
-- * Every header that owns a declaration is in one module.
-- * Two headers are in the same module if and only if they use each other,
--   directly or by way of other headers.
-- * A module of several headers has the name of the header that includes most
--   of the others.  If there are several such headers, it is the first of them
--   in include order.
-- * The modules come in rounds.  A round has every module whose uses are all
--   in earlier rounds, in include order of the headers that give the names.
--   So a module comes after the modules that it uses.
plan :: forall header decl.
     (Ord header, Ord decl)
  => Uses header decl
  -> [Module header decl]
plan input =
    inRounds Set.empty $
      List.sortOn (position . name) (map toModule sharing)
  where
    -- The declarations of each header that owns one
    owned :: Map header (Set decl)
    owned = Map.fromListWith Set.union [
          (header, Set.singleton decl)
        | (decl, header) <- Map.toList input.owner
        ]

    -- The other headers that each of these headers uses.  A use can go
    -- through declarations that no header owns, such as one from outside the
    -- library.
    used :: Map header (Set header)
    used = Map.mapWithKey others owned
      where
        others :: header -> Set decl -> Set header
        others header decls =
            Set.delete header
          . Set.fromList
          . mapMaybe (`Map.lookup` input.owner)
          . Set.toList
          $ input.uses decls

    -- The headers that share a module, the one that gives the name first
    sharing :: [NonEmpty header]
    sharing =
        mapMaybe (fmap nameFirst . NonEmpty.nonEmpty . Graph.flattenSCC) $
          Graph.stronglyConnComp [
              (header, header, Set.toList others)
            | (header, others) <- Map.toList used
            ]

    nameFirst :: NonEmpty header -> NonEmpty header
    nameFirst group =
        named :| filter (/= named) (NonEmpty.toList inOrder)
      where
        inOrder :: NonEmpty header
        inOrder = NonEmpty.sortWith position group

        -- The sort is stable: of the headers that include equally many, the
        -- first in include order stays first.
        named :: header
        named = NonEmpty.head $ NonEmpty.sortWith (Down . included) inOrder

        -- How many of the other headers of the group a header includes
        included :: header -> Int
        included header = Set.size $
          Set.intersection
            (input.includes header)
            (Set.delete header (Set.fromList (NonEmpty.toList group)))

    -- The first header of the module that each header is in
    moduleOf :: Map header header
    moduleOf = Map.fromList [
          (header, NonEmpty.head group)
        | group  <- sharing
        , header <- NonEmpty.toList group
        ]

    toModule :: NonEmpty header -> Module header decl
    toModule group = Module{
          headers = group
        , decls   = foldMap (lookupSet owned) group
        , uses    =
              Set.delete (NonEmpty.head group)
            . Set.fromList
            . mapMaybe (`Map.lookup` moduleOf)
            . Set.toList
            $ foldMap (lookupSet used) group
        }

    lookupSet :: forall a. Map header (Set a) -> header -> Set a
    lookupSet m header = Map.findWithDefault Set.empty header m

    -- One round: take the modules whose uses are all done, in the order that
    -- they are in, then go on with the rest.
    --
    -- Headers that use each other share a module, so the uses between modules
    -- have no cycle, and as long as modules are left one of them is ready.
    inRounds :: Set header -> [Module header decl] -> [Module header decl]
    inRounds done pending =
        case List.partition (\m -> m.uses `Set.isSubsetOf` done) pending of
          (ready, later)
            | null ready || null later -> pending
            | otherwise                ->
                ready ++ inRounds (done <> Set.fromList (map name ready)) later

    name :: Module header decl -> header
    name m = NonEmpty.head m.headers

    -- Where a header is in include order
    --
    -- A header that the order does not list comes after those that it lists.
    position :: header -> (Int, header)
    position header =
        (Map.findWithDefault (length input.order) header positions, header)

    positions :: Map header Int
    positions = Map.fromListWith min $ zip input.order [0 ..]
