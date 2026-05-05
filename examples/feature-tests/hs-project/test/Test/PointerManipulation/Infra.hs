-- | Infra for the pointer manipulation API tests.
--
-- This module is intended to be imported qualified.
--
-- > import Test.PointerManipulation.Infra (ComposableFunc, FieldFunc (..), Func)
-- > import Test.PointerManipulation.Infra qualified as Infra
module Test.PointerManipulation.Infra (
    -- * Properties (also exported for haddocks)
    prop_applyValue_equiv_applyPointer
  , prop_applyPointer_equiv_applyPointerFields
    -- * Composable functions
  , ComposableFunc (..)
  , FieldFunc (..)
  , applyValue
  , applyPointer
  , applyPointerFields
  ) where

import Control.Monad (forM_)
import Data.Kind (Type)
import Data.Proxy (Proxy)
import Foreign (Ptr, Storable (peek, poke), with)
import GHC.Records (HasField (..))
import Test.Tasty.QuickCheck (Property, ioProperty, (===))

import HsBindgen.Runtime.BitfieldPtr qualified as BitfieldPtr
import HsBindgen.Runtime.Prelude (BitfieldPtr)

import Test.Util.Orphans ()

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- |
--
-- A function @f@ applied to a value @x@ should yield the same value as @f@
-- applied to a pointer to @x@ (ignoring 'IO').
--
-- \[
--      \text{applyValue}
--    = \text{applyPointer}
-- \]
--
-- ... (continues in 'prop_applyPointer_equiv_applyPointerFields')
--
prop_applyValue_equiv_applyPointer ::
     (ComposableFunc a, Storable a, Eq a, Show a)
  => Func a
  -> a
  -> Property
prop_applyValue_equiv_applyPointer f p =
    ioProperty $ do
      let p1 = applyValue f p
      p2 <- applyPointer f p
      pure $ p1 === p2

-- |
--
-- ... (continued from 'prop_applyValue_equiv_applyPointer')
--
-- Moreover, it is equivalent to modify a a pointer to @x@ as a whole, and to
-- modify each of the fields of @x@ separately using the pointer manipulation
-- API.
--
-- \[
--      \text{applyPointer}
--    = \text{applyPointerFields}
-- \]
--
prop_applyPointer_equiv_applyPointerFields ::
     (ComposableFunc a, Storable a, Eq a, Show a)
  => Func a
  -> a
  -> Property
prop_applyPointer_equiv_applyPointerFields f p =
    ioProperty $ do
      p1 <- applyPointer f p
      p2 <- applyPointerFields f p
      pure $ p1 === p2

{-------------------------------------------------------------------------------
  Composable functions
-------------------------------------------------------------------------------}

class ComposableFunc a where
  data Func a :: Type

  composed :: Func a -> (a -> a)
  decomposed :: Func a -> [FieldFunc a]

data FieldFunc a where
  FieldFunc ::
       forall field a b.
       ( HasField field a b
       , HasField field (Ptr a) (Ptr b)
       , Storable b
       )
    => Proxy field
    -> (b -> b)
    -> FieldFunc a
  BitfieldFunc ::
       forall field a b.
       ( HasField field a b
       , HasField field (Ptr a) (BitfieldPtr b)
       , Storable b
       , BitfieldPtr.Bitfield b
       )
    => Proxy field
    -> (b -> b)
    -> FieldFunc a

-- | Apply a function to a value
--
-- NOTE: We don't make a distinction here like we did with 'applyPointer' vs.
-- 'applyPointerFields' because it is not an interesting distinction when
-- considering pure, immutable Haskell values.
applyValue :: ComposableFunc a => Func a -> a -> a
applyValue f x = composed f x

-- | Apply a function to a value inside a pointer
--
-- The value is written to a pointer, then the pointer is modified, and the
-- final value is read from the pointer. The pointer is modified as a whole in
-- one go (as opposed to 'applyPointerFields').
applyPointer :: (ComposableFunc a, Storable a) => Func a -> a -> IO a
applyPointer f x = with x $ \ptr -> do
    y <- peek ptr
    poke ptr (composed f y)
    peek ptr

-- | Apply a function to a value (through pointer manipulation)
--
-- The value is written to a pointer, then the pointer is modified, and the
-- final value is read from the pointer. The pointer is not modified as a whole
-- in one go (as opposed to 'applyPointer'). Instead, a sub-function is applied
-- to each of the point's fields using the pointer manipulation API.
applyPointerFields :: forall a. (ComposableFunc a, Storable a) => Func a -> a -> IO a
applyPointerFields f x =
    with x $ \ptr -> do
      forM_ (decomposed @a f) $ \case
        (FieldFunc (_ :: Proxy field) g) -> do
          let fieldPtr = getField @field ptr
          y <- peek fieldPtr
          poke fieldPtr (g y)
        (BitfieldFunc (_ :: Proxy field) g) -> do
          let bitfieldPtr = getField @field ptr
          y <- BitfieldPtr.peek bitfieldPtr
          BitfieldPtr.poke bitfieldPtr (g y)
      peek ptr
