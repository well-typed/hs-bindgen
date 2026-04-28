-- | Macro handler interface.
--
-- A 'Handler' provides macro parsing and typechecking, decoupling the
-- core pipeline from any particular implementation (such as @c-expr-dsl@).
--
-- Intended import:
--
-- > import HsBindgen.Macro.Handler qualified as Handler
module HsBindgen.Macro.Handler (
    -- * Handler
    Handler(..)
    -- * Parsed macros
  , Parsed(..)
    -- * Errors
  , ParseError(..)
  , TcError(..)
    -- * Typecheck result
  , TcResult(..)
  ) where

import Data.Typeable (Typeable, cast)

import Clang.CStandard (CStandard)
import Clang.HighLevel.Types (Token, TokenSpelling)

import HsBindgen.Imports
import HsBindgen.Macro.Ref
import HsBindgen.Macro.Type qualified as Type
import HsBindgen.Macro.Value qualified as Value

{-------------------------------------------------------------------------------
  Parsed macros
-------------------------------------------------------------------------------}

-- | A parsed macro, carrying dependency information for @hs-bindgen@
-- and an opaque payload for the handler's typechecker.
data Parsed where
  Parsed :: (Typeable a, Eq a) => {
      -- | All names referenced in the macro body (excluding local args).
      refs   :: [Ref]
      -- | Opaque parsed representation, passed back to the handler
      -- for typechecking.
    , parsed :: a
    } -> Parsed

instance Eq Parsed where
  Parsed (refs1 :: [Ref]) (parsed1 :: a1) == Parsed (refs2 :: [Ref]) (parsed2 :: a2) =
    refs1 == refs2 && case cast @a1 @a2 parsed1 of
      Just p1 -> p1 == parsed2
      Nothing -> False

instance Show Parsed where
  show (Parsed rs _) = "Parsed " ++ show rs ++ " <opaque>"

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data ParseError = ParseError String
  deriving stock (Show)

data TcError = TcError String
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Typecheck result
-------------------------------------------------------------------------------}

-- | Result of typechecking a macro.
--
-- The asymmetry is intentional: a type macro result is a single expression
-- ('Type.Expr'), so there is nothing to bundle. A value macro result requires
-- three things together — argument names, expression body, and the quantified
-- Haskell type — which are bundled in 'Value.Result'.
data TcResult =
    Type  Type.Expr
  | Value Value.Result
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Handler
-------------------------------------------------------------------------------}

-- | A macro handler providing parsing and typechecking.
--
-- The handler's typechecker state (@tcState@) is existentially quantified
-- so that @hs-bindgen:internal@ does not depend on its representation.
data Handler where
  Handler :: forall tcState. {

      -- | Parse a macro definition from its token stream.
      --
      -- Returns 'Nothing' if the macro has no tokens (empty body).
      -- Returns 'Left' on parse errors.
      parse
        :: CStandard
        -> Text                           -- ^ Macro name.
        -> [Text]                         -- ^ Macro argument names.
        -> [Token TokenSpelling]          -- ^ Macro body tokens.
        -> Either ParseError (Maybe Parsed)

      -- | Create the initial typechecker state.
      --
      -- Receives the set of known typedef names (names only, not types;
      -- type resolution is done by @hs-bindgen@ after the handler returns).
    , initTcState
        :: Set Text                       -- ^ Known typedef names.
        -> tcState

      -- | Typecheck a single parsed macro.
      --
      -- The handler receives the current typechecker state and must return
      -- an updated state on success.
    , typecheck
        :: tcState
        -> Text                           -- ^ Macro name.
        -> [Text]                         -- ^ Macro argument names.
        -> Parsed                         -- ^ Parsed macro (opaque payload).
        -> Either TcError (TcResult, tcState)

    } -> Handler
