module HsBindgen.Patterns.Enum.Simple (
    SimpleEnum(..)
  , IsSimpleEnum(..)
    -- * API
  , simpleEnum
  , fromSimpleEnum
  , unsafeFromSimpleEnum
  ) where

import Foreign.C
import GHC.Show (appPrec1)
import GHC.Stack

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | ADTs corresponding to simple enums
--
-- See 'SimpleEnum' for discussion
class IsSimpleEnum hs where
  -- | Translate Haskell constructor to C value
  simpleToC :: hs -> CInt

  -- | Translate C value to haskell constructor
  --
  -- This returns a 'Maybe' value, because C enums do not restrict the range.
  -- From Wikipedia (<https://en.wikipedia.org/wiki/C_syntax#Enumerated_type>):
  --
  -- > Some compilers warn if an object with enumerated type is assigned a value
  -- > that is not one of its constants. However, such an object can be assigned
  -- > any values in the range of their compatible type, and enum constants can
  -- > be used anywhere an integer is expected. For this reason, enum values are
  -- > often used in place of preprocessor #define directives to create named
  -- > constants. Such constants are generally safer to use than macros, since
  -- > they reside within a specific identifier namespace.
  --
  -- This means that a 'Nothing' value is not necessary an error.
  simpleFromC :: CInt -> Maybe hs

-- | Simple C enums
--
-- Suppose we have a simple C enum defined like this:
--
-- > enum SomeEnum {
-- >   Value1,
-- >   Value2,
-- >   Value3
-- > };
--
-- Then 'SimpleEnum' can link the underlying 'CInt' to a Haskell ADT. Using
-- @hsc2hs@, this might look like
--
-- > data SomeEnum = Value1 | Value2 | Value3
-- >
-- > instance IsSimpleEnum SomeEnum where
-- >   simpleToC Value1 = #const Value1
-- >   simpleToC Value2 = #const Value2
-- >   simpleToC Value3 = #const Value3
-- >
-- >   simpleFromC (#const Value1) = Just Value1
-- >   simpleFromC (#const Value2) = Just Value2
-- >   simpleFromC (#const Value3) = Just Value3
-- >
-- >   simpleFromC _otherwise = Nothing
newtype SimpleEnum hs = SimpleEnum CInt

instance (IsSimpleEnum hs, Show hs) => Show (SimpleEnum hs) where
  showsPrec p x = showParen (p >= appPrec1) $
      either showC showHS $ fromSimpleEnum x
    where
      showC :: CInt -> ShowS
      showC c =
            showString "SimpleEnum "
          . showsPrec appPrec1 c

      showHS :: hs -> ShowS
      showHS hs =
             showString "simpleEnum "
           . showsPrec appPrec1 hs

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

simpleEnum :: IsSimpleEnum hs => hs -> SimpleEnum hs
simpleEnum = SimpleEnum . simpleToC

-- | Underlying C value
--
-- Returns the raw 'CInt' if is out of the range of @a@
fromSimpleEnum :: IsSimpleEnum hs => SimpleEnum hs -> Either CInt hs
fromSimpleEnum (SimpleEnum i) = maybe (Left i) Right $ simpleFromC i

-- | Like 'fromSimpleEnum', but throw an exception if the value is out of range
unsafeFromSimpleEnum :: (HasCallStack, IsSimpleEnum hs) => SimpleEnum hs -> hs
unsafeFromSimpleEnum = either (error . err) id . fromSimpleEnum
  where
    err :: CInt -> String
    err c = "SimpleEnum out of range: " ++ show c
