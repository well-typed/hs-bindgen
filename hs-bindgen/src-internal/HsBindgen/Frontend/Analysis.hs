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

-- | Does the dependent require the dependency's full definition or only a
-- name-level reference?
--
-- That is, when generating a binding for @B@ which depends on @A@, what do we
-- need to know about @A@?
data Dependency =
    -- | We need to know the full shape (i.e., its size and instances) of the
    -- dependency.
    --
    -- For example, when generating a binding for @B@, we need to know the shape
    -- of @A@:
    --
    -- @
    -- typedef int A;
    -- struct B {
    --   A fieldA;
    -- };
    -- @
    NeedsShape
    -- | We only need to know the name of the dependency.
    --
    -- For example, since @fieldA@ is a /pointer/ to @A@, when generating a
    -- binding for @B@, we only need to know the name of @A@:
    --
    -- @
    -- typedef int A;
    -- struct B {
    --   A* fieldA;
    -- };
    -- @
    --
    -- A more subtle example using a function pointer:
    --
    -- @
    -- typedef int A;
    -- typedef void (*B) (A);
    -- @
  | NeedsNameOnly
  deriving stock (Eq, Ord, Show)
