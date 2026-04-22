{-# OPTIONS_GHC -Wno-orphans #-}

module Test.PointerManipulation.Util (
    -- * Properties (exported for haddocks)
    prop_applyValue_equiv_applyPointer
  , prop_applyPointer_equiv_applyPointerFields
    -- *
  , ComposableFunc (..)
  , FieldFunc (..)
  , applyValue
  , applyPointer
  , applyPointerFields
  ) where

import Control.Monad (forM_)
import Data.Kind (Type)
import Data.Proxy
import Foreign
import GHC.Records
import Test.Tasty.QuickCheck

import Test.Util.Orphans ()

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- |
--
-- A function @f@ applied to a value @x@ should yield the same value as @f@
-- applied to a pointer to @x@ (with appropriate modifications to @f@ to deal
-- with IO).
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
      pure $ counterexample "equation 1"
           $ p1 === p2

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
      pure $ counterexample "equation 2"
           $ p1 === p2

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
      forM_ (decomposed @a f) $ \(FieldFunc (_ :: Proxy field) g) -> do
        let fieldPtr = getField @field ptr
        y <- peek (getField @field ptr)
        poke fieldPtr (g y)
      peek ptr
