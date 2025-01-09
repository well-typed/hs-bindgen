{-# LANGUAGE CPP #-}

module HsBindgen.Runtime.Patterns.Enum.Simple (
    SimpleEnum(..)
  , IsSimpleEnum(..)
  , SimpleEnumOutOfRange(..)
    -- * API
  , simpleEnum
  , coerceSimpleEnum
  , fromSimpleEnum
  , simpleEnumInRange
  , unsafeFromSimpleEnum
  ) where

import Control.Exception
import Data.Coerce
import Data.Kind
import Data.Typeable
import Foreign.C
import GHC.Generics (Generic)
import GHC.Show (appPrec1, showSpace)
import GHC.Stack
import Text.Show.Pretty (PrettyVal(..))
import Text.Show.Pretty qualified as Pretty

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | ADTs corresponding to simple enums
--
-- Instances should satisfy the following laws:
--
-- > forall (x :: hs).   simpleFromC (simpleToC x) == Just x
-- > forall (i :: CInt). if   simpleFromC i == Just x
-- >                     then simpleToC   x == i
--
-- We require 'Typeable' so that we can show the Haskell type in error messages
-- (since it's the Haskell type that determines the range of the enum).
--
-- See 'SimpleEnum' for additional discussion.
class Typeable hs => IsSimpleEnum (hs :: Type) where
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
newtype SimpleEnum (hs :: Type) = SimpleEnum CInt
  deriving stock (Eq, Ord, Generic)

{-------------------------------------------------------------------------------
  Showing values
-------------------------------------------------------------------------------}

instance (IsSimpleEnum hs, Show hs) => Show (SimpleEnum hs) where
  showsPrec p i = showParen (p >= appPrec1) $
      either (uncurry showC) showHs $ showSimpleEnum i
    where
      showC :: CInt -> TypeRep -> ShowS
      showC c typ  =
            showString "SimpleEnum @"
          . showsPrec appPrec1 typ
          . showSpace
          . showsPrec appPrec1 c

      showHs :: hs -> ShowS
      showHs hs =
             showString "simpleEnum "
           . showsPrec appPrec1 hs

instance (IsSimpleEnum hs, PrettyVal hs) => PrettyVal (SimpleEnum hs) where
  prettyVal =
      either (uncurry showC) showHs . showSimpleEnum
    where
      showC :: CInt -> TypeRep -> Pretty.Value
      showC c typ = Pretty.Con "SimpleEnum" [
            Pretty.Con ("@" ++ show typ) []
          , prettyVal (fromIntegral c :: Int)
          ]

      showHs :: hs -> Pretty.Value
      showHs hs = Pretty.Con "simpleEnum" [
            prettyVal hs
          ]

-- | Internal auxiliary for showing 'SimpleEnum'
showSimpleEnum :: forall hs.
     IsSimpleEnum hs
  => SimpleEnum hs -> Either (CInt, TypeRep) hs
showSimpleEnum =
    either (Left . showC) Right . fromSimpleEnum
  where
    showC :: CInt -> (CInt, TypeRep)
    showC c = (c, typeRep (Proxy @hs))

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Construct 'SimpleEnum' from Haskell value
--
-- > forall (x :: hs). fromSimpleEnum (simpleEnum x) == Right x
simpleEnum :: IsSimpleEnum hs => hs -> SimpleEnum hs
simpleEnum = SimpleEnum . simpleToC

-- | Construct 'SimpleEnum' from C value
--
-- The 'CInt' may be outside the range of the 'SimpleEnum'.
--
-- > forall (y :: CInt). if   simpleFromEnum (coerceSimpleEnum y) == Right x
-- >                     then simpleEnum x == coerceSimpleEnum y
coerceSimpleEnum :: CInt -> SimpleEnum hs
coerceSimpleEnum = coerce

-- | Underlying C value
--
-- Returns the raw 'CInt' if is out of the range of @a@
fromSimpleEnum :: IsSimpleEnum hs => SimpleEnum hs -> Either CInt hs
fromSimpleEnum (SimpleEnum i) = maybe (Left i) Right $ simpleFromC i

-- | Is the underlying C value in the range of the Haskell type?
simpleEnumInRange :: IsSimpleEnum hs => SimpleEnum hs -> Bool
simpleEnumInRange = either (const False) (const True) . fromSimpleEnum

-- | Like 'fromSimpleEnum', but throws 'SimpleEnumOutOfRange' if out of range
unsafeFromSimpleEnum :: forall hs.
     (HasCallStack, IsSimpleEnum hs, Typeable hs)
  => SimpleEnum hs -> hs
unsafeFromSimpleEnum = either (throw . err) id . fromSimpleEnum
  where
    err :: CInt -> SimpleEnumOutOfRange hs
    err = SimpleEnumOutOfRange callStack

-- | Exception thrown by 'unsafeFromSimpleEnum'
data SimpleEnumOutOfRange (hs :: Type) = SimpleEnumOutOfRange CallStack CInt

instance IsSimpleEnum hs => Exception (SimpleEnumOutOfRange hs) where
  displayException (SimpleEnumOutOfRange cs i) = concat [
        "C value "
      , show i
      , " out of range of "
      , show (typeRep (Proxy @hs))
      , " at "
      , prettyCallStack cs
      ]

#if MIN_VERSION_base(4,20,0)
  backtraceDesired _ = False
#endif

instance IsSimpleEnum hs => Show (SimpleEnumOutOfRange hs) where
  showsPrec p (SimpleEnumOutOfRange cs i) = showParen (p >= appPrec1) $
        showString "SimpleEnumOutOfRange @"
      . showsPrec appPrec1 (typeRep (Proxy @hs))
      . showSpace
      . showsPrec appPrec1 cs
      . showSpace
      . showsPrec appPrec1 i
