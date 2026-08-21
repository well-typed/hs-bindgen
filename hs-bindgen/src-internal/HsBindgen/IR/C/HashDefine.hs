-- | @#define@ directive
--
-- This module should only be used within the @HsBindgen.IR@ hierarchy.  From
-- outside the @HsBindgen.IR@ hierarchy, "HsBindgen.IR.C" should be used.
--
-- Within @HsBindgen.IR@, all modules aside from "HsBindgen.IR.C" should import
-- this module qualified for consistency.
--
-- > import HsBindgen.IR.C.HashDefine qualified as C
module HsBindgen.IR.C.HashDefine (
    -- * HashDefine
    HashDefine(..)
  , hashDefineToDirective
  ) where

import HsBindgen.Imports

{-------------------------------------------------------------------------------
  HashDefine
-------------------------------------------------------------------------------}

-- | A @#define@ directive
--
-- This is @#define@ syntax, /not/ Clang's @-D@ syntax; the translation between
-- the two is not the identity:
--
-- * The @=@ of @-DFOO=BAR@ is not @#define@ syntax.  @#define FOO=BAR@ is valid
--   C, but defines a macro @FOO@ whose replacement list is @= BAR@.
-- * @-DFOO@ is not @#define FOO@.  Clang defines a bare @-D@ macro as @1@, so
--   @-DFOO@ and @-DFOO=@ define /different/ macros.  Both are @#ifdef@-true,
--   but @#if FOO@ is true for the former and an error for the latter.
--
-- The correspondence is therefore:
--
-- +---------------------+--------------------------------+--------------------+
-- | Clang @-D@ argument | t'HashDefine'                  | emitted directive  |
-- +=====================+================================+====================+
-- | @-D FOO@            | @name = "FOO", value = "1"@    | @#define FOO 1@    |
-- +---------------------+--------------------------------+--------------------+
-- | @-D FOO=BAR@        | @name = "FOO", value = "BAR"@  | @#define FOO BAR@  |
-- +---------------------+--------------------------------+--------------------+
-- | @-D FOO=@           | @name = "FOO", value = ""@     | @#define FOO@      |
-- +---------------------+--------------------------------+--------------------+
-- | @-D \'FOO(x)=x\'@   | @name = "FOO(x)", value = "x"@ | @#define FOO(x) x@ |
-- +---------------------+--------------------------------+--------------------+
--
-- Neither field is parsed or validated: a malformed definition is reported by
-- Clang as a diagnostic in the root header, the same treatment an unresolvable
-- @#include@ gets.
data HashDefine = HashDefine {
      -- | Macro name; may be function-like, e.g. @FOO(x)@
      name :: String

      -- | Replacement list; @""@ for @#define FOO@
    , value :: String
    }
  deriving stock (Show, Eq, Generic)

-- | Render a t'HashDefine' as a @#define@ directive (no trailing newline)
hashDefineToDirective :: HashDefine -> String
hashDefineToDirective hashDefine
  | null hashDefine.value = "#define " ++ hashDefine.name
  | otherwise             = "#define " ++ hashDefine.name ++ " " ++ hashDefine.value
