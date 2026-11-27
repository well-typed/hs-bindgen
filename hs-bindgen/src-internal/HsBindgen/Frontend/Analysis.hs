-- | Common vocabulary for the analysis done in the frontend
--
-- Intended for unqualified import.
module HsBindgen.Frontend.Analysis (
    -- * Dependencies
    Dependency(..)
  ) where

{-------------------------------------------------------------------------------
  Dependencies
-------------------------------------------------------------------------------}

-- | What a C declaration needs to know about a declaration it depends on
--
-- The label describes the C declarations, not the bindings generated from
-- them. The output order, and with it instance resolution, follows the
-- 'NeedsShape' edges: the instances of a Haskell type follow its
-- representation, which follows the C layout.
data Dependency =
    -- | The layout (size, alignment, representation) of the dependent is
    -- determined by the layout of the dependency.
    --
    -- For example, @B@ needs the shape of @A@:
    --
    -- @
    -- typedef int A;
    -- struct B {
    --   A fieldA;
    -- };
    -- @
    --
    -- A layout cannot contain itself, so the 'NeedsShape' edges are acyclic.
    NeedsShape
    -- | The layout of the dependent is independent of the dependency.
    --
    -- For example, @fieldA@ is a /pointer/ to @A@, so @B@ needs the name of @A@
    -- only:
    --
    -- @
    -- typedef int A;
    -- struct B {
    --   A* fieldA;
    -- };
    -- @
    --
    -- The same holds under a block pointer and inside a function type, which
    -- has no layout. Here @B@ is a pointer, and @C@ a function type; both need
    -- the name of @A@ only:
    --
    -- @
    -- typedef int A;
    -- typedef void (*B) (A);
    -- typedef void C (A);
    -- @
    --
    -- C completeness is not the criterion: C allows @typedef struct S T;@ with
    -- an incomplete @S@, yet @T@ needs the shape of @S@.
  | NeedsNameOnly
  deriving stock (Eq, Ord, Show)
