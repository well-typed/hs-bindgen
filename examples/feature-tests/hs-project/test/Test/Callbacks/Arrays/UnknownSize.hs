module Test.Callbacks.Arrays.UnknownSize (
    tests
    -- * Properties (exported for haddocks)
  , prop_reverse_list_equiv_toFromFunPtr_reverse_list
  , prop_reverse_list_equiv_fromFunPtr_reverse_list
  , prop_apply_list_op_reverse_list
  , prop_apply_list_op_hsFun
  ) where

import Control.Monad (forM_)
import Data.Vector.Storable qualified as VS
import Foreign.C.Types (CInt (CInt))
import Foreign.Ptr (FunPtr)
import Foreign.Storable (Storable (peekElemOff, pokeElemOff))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), Fun, Property, applyFun,
                              ioProperty, testProperty, (===))

import HsBindgen.Runtime.IncompleteArray qualified as IA
import HsBindgen.Runtime.IsArray qualified as IsA
import HsBindgen.Runtime.Prelude (FromFunPtr (fromFunPtr), IsArray (Elem),
                                  ToFunPtr (toFunPtr), safeCastFunPtr)

import Generated.Callbacks.Arrays.UnknownSize qualified as Types
import Generated.Callbacks.Arrays.UnknownSize.FunPtr qualified as FunPtr
import Generated.Callbacks.Arrays.UnknownSize.Safe qualified as Safe
import Test.Util.Orphans ()

tests :: TestTree
tests = testGroup "Test.Callbacks.UnknownSize" [
      testProperty "prop_reverse_list_equiv_toFromFunPtr_reverse_list"
        prop_reverse_list_equiv_toFromFunPtr_reverse_list
    , testProperty "prop_reverse_list_equiv_fromFunPtr_reverse_list"
        prop_reverse_list_equiv_fromFunPtr_reverse_list
    , testProperty "prop_apply_list_op_reverse_list"
        prop_apply_list_op_reverse_list
    , testProperty "prop_apply_list_op_hsFun"
        prop_apply_list_op_hsFun
    ]

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- |
-- \[
--  \forall xs, len.
--      \text{Safe.reverse_list} ~ xs ~ len
--    = (\text{fromFunPtr}
--        ~ (\text{toFunPtr} ~ \text{Safe.reverse_list})
--      ) ~ xs
--        ~ len
-- \]
--
prop_reverse_list_equiv_toFromFunPtr_reverse_list :: List -> Property
prop_reverse_list_equiv_toFromFunPtr_reverse_list (List xs) = ioProperty $ do
    zs1 <- runList_op reverse_list xs
    reverse_list' <- fromFunPtr <$> toFunPtr reverse_list
    zs2 <- runList_op reverse_list' xs
    pure $ zs1 === zs2
  where
    reverse_list :: Types.List_op
    reverse_list = Types.List_op Safe.reverse_list

-- |
-- \[
--  \forall xs, len.
--      \text{Safe.reverse_list} ~ xs ~ len
--    = (\text{fromFunPtr} ~ \text{FunPtr.reverse_list})
--      ) ~ xs
--        ~ len
-- \]
--
prop_reverse_list_equiv_fromFunPtr_reverse_list :: List -> Property
prop_reverse_list_equiv_fromFunPtr_reverse_list (List xs) = ioProperty $ do
    zs1 <- runList_op reverse_list xs
    zs2 <- runList_op reverse_list' xs
    pure $ zs1 === zs2
  where
    reverse_list :: Types.List_op
    reverse_list = Types.List_op Safe.reverse_list
    reverse_listPtr :: FunPtr Types.List_op
    reverse_listPtr = safeCastFunPtr FunPtr.reverse_list
    reverse_list' :: Types.List_op
    reverse_list' = fromFunPtr reverse_listPtr

-- |
-- \[
--  \forall xs, len.
--      \text{reverseList} ~ xs ~ len
--    = \text{Safe.apply_list_op}
--        ~ \text{FunPtr.reverse_list}
--        ~ xs
--        ~ len
-- \]
--
prop_apply_list_op_reverse_list :: List -> Property
prop_apply_list_op_reverse_list (List xs) = ioProperty $ do
    zs <- apply_list_opHelper reverse_listPtr xs
    pure $ reverseList xs === zs
  where
    reverse_listPtr :: FunPtr Types.List_op
    reverse_listPtr = safeCastFunPtr FunPtr.reverse_list

-- |
-- \[
--  \forall f, xs, len.
--      f ~ xs ~ len
--    = \text{Safe.apply_list_op}
--        ~ (\text{toFunPtr} ~ f)
--        ~ xs
--        ~ len
-- \]
--
prop_apply_list_op_hsFun :: ListOp -> List -> Property
prop_apply_list_op_hsFun f (List xs) = ioProperty $ do
    zs1 <- runList_op fOp xs
    fPtr <- toFunPtr fOp
    zs2 <- apply_list_opHelper fPtr xs
    pure $ zs1 === zs2
  where
    fOp :: Types.List_op
    fOp = getList_op f

{-------------------------------------------------------------------------------
  Modelled callback function
-------------------------------------------------------------------------------}

reverseList :: Types.List -> Types.List
reverseList (Types.List xs) = Types.List (IA.fromList (reverse (IA.toList xs)))

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

runList_op :: Types.List_op -> Types.List -> IO Types.List
runList_op (Types.List_op f) xs = do
    IsA.withElemPtr xs $ \xsPtr -> do
        f xsPtr xsLen
        IA.peekArray xsLen (IA.toPtr xsPtr)
  where
    xsLen :: Num a => a
    xsLen = fromIntegral (VS.length (IA.toVector (Types.unwrapList xs)))

apply_list_opHelper :: FunPtr Types.List_op -> Types.List -> IO Types.List
apply_list_opHelper fPtr xs = do
    IsA.withElemPtr xs $ \xsPtr -> do
        Safe.apply_list_op fPtr xsPtr xsLen
        IA.peekArray xsLen (IA.toPtr xsPtr)
  where
    xsLen :: Num a => a
    xsLen = fromIntegral (VS.length (IA.toVector (Types.unwrapList xs)))

getList_op :: ListOp -> Types.List_op
getList_op (ListOp f) = Types.List_op $ \xsPtr xsLen -> do
    forM_ [0.. fromIntegral xsLen - 1] $ \i -> do
      x <- peekElemOff xsPtr i
      let y = applyFun f x
      pokeElemOff xsPtr i y

{-------------------------------------------------------------------------------
  Arbitrary
-------------------------------------------------------------------------------}

newtype List = List Types.List
  deriving stock (Show, Eq)

instance Arbitrary List where
  arbitrary = do
      xs <- arbitrary
      pure (List (Types.List (IA.fromList xs)))
  shrink (List (Types.List (IA.toList -> xs))) =
      [ List (Types.List (IA.fromList xs'))
      | xs' <- shrink xs
      ]

-- Intended to be opaque: use 'getList_op' to obtain a 'Types.List_op'
newtype ListOp = ListOp (Fun (Elem Types.List) (Elem Types.List))

deriving stock instance Show ListOp
deriving newtype instance Arbitrary ListOp
