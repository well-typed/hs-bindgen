{-# LANGUAGE NoImplicitPrelude #-}

-- | C boolean semantics
--
-- In C, boolean types are numeric, where value @0@ is considered false and any
-- other value is considered true.  They are implemented differently across
-- different C projects:
--
-- * Since C23, there is a @bool@ type and predefined constants @true@ and
--   @false@.
-- * Since C99, standard header @stdbool.h@ defines (implementation-dependent)
--   type @_Bool@ and macros @true@ and @false@.  This header is deprecated
--   since C23.
-- * Before C99, users defined boolean types and values, following the common
--   conventions.  Some projects still define their own boolean type, perhaps
--   for compatibility with old C standards.  Common implementations include the
--   following, where @bool@ may be spelled differently (such as @Bool@ or
--   @BOOL@):
--
--     * An @enum@ type:
--
--         @
--         typedef enum { false, true } bool;
--         @
--
--     * A @typedef@ and /separate/ @enum@ values:
--
--         @
--         typedef int bool;
--         enum { false, true };
--         @
--
--     * A @typedef@ and macro values:
--
--         @
--         typedef int bool;
--         #define true 1
--         #define false 0
--         @
--
--     * A macro alias and macro values:
--
--         @
--         #define bool int
--         #define true 1
--         #define false 0
--         @
--
-- This module provides an API that is compatible with bindings generated for
-- any of these possible implementations.  Note that the implementation only
-- requires 'Eq' and 'Num' instances.  Conversion follows C23 semantics: /only/
-- value @0@ is considered false, and any other value is considered true.
--
-- Intended for qualified import.
--
-- > import HsBindgen.Runtime.CBool qualified as CBool
module HsBindgen.Runtime.CBool (
    -- * Values
    true
  , false
    -- * Predicates
  , isTrue
  , isFalse
    -- * Conversion
  , fromBool
  , toBool
    -- * Operations
  , (&&)
  , (||)
  , not
  , bool
  , if_
  , when
  , unless
  ) where

import Prelude hiding (not, (&&), (||))
import Prelude qualified

{-------------------------------------------------------------------------------
  Values
-------------------------------------------------------------------------------}

-- | Standard true value: @1@
true :: Num b => b
true = 1

-- | Standard false value: @0@
false :: Num b => b
false = 0

{-------------------------------------------------------------------------------
  Predicates
-------------------------------------------------------------------------------}

-- | 'True' if the value is not @0@
isTrue :: (Eq b, Num b) => b -> Bool
isTrue = (/= false)

-- | 'True' if the value is @0@
isFalse :: (Eq b, Num b) => b -> Bool
isFalse = (== false)

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

-- | Convert from 'Bool' to 'true' or 'false'
fromBool :: Num b => Bool -> b
fromBool True  = true
fromBool False = false

-- | Convert to 'Bool' using 'isTrue'
toBool :: (Eq b, Num b) => b -> Bool
toBool = isTrue

{-------------------------------------------------------------------------------
  Operations
-------------------------------------------------------------------------------}

-- | Boolean /and/, lazy in the second argument
--
-- This function returns one of the standard values.
(&&) :: (Eq b, Num b) => b -> b -> b
l && r = fromBool $ toBool l Prelude.&& toBool r

-- | Boolean /or/, lazy in the second argument
--
-- This function returns one of the standard values.
(||) :: (Eq b, Num b) => b -> b -> b
l || r = fromBool $ toBool l Prelude.|| toBool r

-- | Boolean /not/
--
-- This function returns one of the standard values.
not :: (Eq b, Num b) => b -> b
not = fromBool . Prelude.not . toBool

-- | Boolean case analysis, implemented using 'isTrue'
--
-- See 'Data.Bool.bool' for details.
bool :: (Eq b, Num b) => a -> a -> b -> a
bool f t b = if isTrue b then t else f

-- | Boolean case analysis, implemented using 'isTrue'
if_ :: (Eq b, Num b) => b -> a -> a -> a
if_ b t f = if isTrue b then t else f

-- | Execute an applicative expression when the condition 'isTrue'
when :: (Eq b, Num b, Applicative f) => b -> f () -> f ()
when b e = if isTrue b then e else pure ()
{-# ANN when ("HLint: ignore Use when" :: String) #-}

-- | Execute an applicative expression when the condition 'ifFalse'
unless :: (Eq b, Num b, Applicative f) => b -> f () -> f ()
unless b e = if isFalse b then e else pure ()
{-# ANN unless ("HLint: ignore Use when" :: String) #-}
