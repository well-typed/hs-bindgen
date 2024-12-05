{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}

module HsBindgen.Util.TestEquality
  ( equals1, equals2 )
  where

-- base
import Data.Type.Equality
  ( (:~:)(..) )
import GHC.Exts
  ( (==#), dataToTag#, isTrue#
#if MIN_VERSION_base(4,20,0)
  , DataToTag
#endif
  )
import Unsafe.Coerce
  ( unsafeCoerce )

--------------------------------------------------------------------------------

infixr 4 `equals1`
-- | Check whether two GADT values of type @k ->Type@ are equal.
--
-- If so, also return a proof that the tags were equal.
--
-- NB: this is stricter than 'testEquality', as 'testEquality' is supposed
-- to return @Just Refl@ whenever the tags are equal, even when the values
-- themselves are different.
equals1 :: forall a tag1 tag2.
  ( forall tag. Eq ( a tag )
#if MIN_VERSION_base(4,20,0)
  , forall tag. DataToTag ( a tag )
#endif
  ) => a tag1 -> a tag2 -> Maybe ( tag1 :~: tag2 )
equals1 k1 k2
  | -- Fail-fast: comparing the tag first.
    isTrue# ( dataToTag# k1 ==# dataToTag# k2 )
    -- Assume the types are the same; this allows us to use the 'Eq' instance.
  , Refl <- ( unsafeCoerce Refl :: tag1 :~: tag2 )
  , k1 == k2
  -- The values are equal (according to the 'Eq' instance): this justifies the
  -- unsafe coercion above, assuming that the 'Eq' instance is lawful.
  = Just $ unsafeCoerce Refl
  | otherwise
  = Nothing

infixr 4 `equals2`
-- | Check whether two GADT values of type @k -> l ->Type@ are equal.
--
-- If so, also return a proof that the tags were equal.
--
-- NB: this is stricter than 'testEquality', as 'testEquality' is supposed
-- to return @Just Refl@ whenever the tags are equal, even when the values
-- themselves are different.
equals2 :: forall a k1 k2 l1 l2.
  ( forall k l. Eq ( a k l)
#if MIN_VERSION_base(4,20,0)
  , forall k l. DataToTag ( a k l )
#endif
  ) => a k1 l1 -> a k2 l2 -> Maybe ( '( k1, l1 ) :~: '( k2, l2 ) )
equals2 k1 k2
  | -- Fail-fast: comparing the tag first.
    isTrue# ( dataToTag# k1 ==# dataToTag# k2 )
    -- Assume the types are the same; this allows us to use the 'Eq' instance.
  , Refl <- ( unsafeCoerce Refl :: '( k1, l1 ) :~: '( k2, l2 ) )
  , k1 == k2
  -- The values are equal (according to the 'Eq' instance): this justifies the
  -- unsafe coercion above, assuming that the 'Eq' instance is lawful.
  = Just $ unsafeCoerce Refl
  | otherwise
  = Nothing
