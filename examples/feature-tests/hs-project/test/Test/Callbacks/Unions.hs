module Test.Callbacks.Unions (
    tests
    -- * Properties (exported for haddocks)
  , prop_apply_object_op_increment_object
  ) where

import Foreign.C (CFloat, CLLong)
import Foreign.Ptr (FunPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import HsBindgen.Runtime.Prelude (safeCastFunPtr)

import Generated.Callbacks.Unions qualified as Types
import Generated.Callbacks.Unions.FunPtr qualified as FunPtr
import Generated.Callbacks.Unions.Safe qualified as Safe

tests :: TestTree
tests = testGroup "Test.Callbacks.Unions" [
      testProperty "prop_apply_object_op_increment_object"
        prop_apply_object_op_increment_object
    ]

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- |
-- \[
--  \forall obj.
--      \text{incrementObject}
--        ~ (\text{getVal} ~ obj)
--        ~ (\text{getTyp} ~ obj)
--    = \text{Safe.apply_object_op}
--        ~ \text{FunPtr.increment_object}
--        ~ (\text{getVal} ~ obj)
--        ~ (\text{getTyp} ~ obj)
-- \]
--
prop_apply_object_op_increment_object :: Object -> Property
prop_apply_object_op_increment_object obj = ioProperty $ do
    z <- Safe.apply_object_op increment_objectPtr (getVal obj) (getTyp obj)
    let z' = incrementObject (getVal obj) (getTyp obj)
    pure $ Types.get_val_integer z' === Types.get_val_integer z .||.
           Types.get_val_floating z' === Types.get_val_floating z
  where
    increment_objectPtr :: FunPtr Types.Object_op
    increment_objectPtr = safeCastFunPtr FunPtr.increment_object

{-------------------------------------------------------------------------------
  Modelled callback function
-------------------------------------------------------------------------------}

incrementObject :: Types.Val -> Types.Typ -> Types.Val
incrementObject val typ =
    case typ of
      Types.LongLong -> Types.set_val_integer  (Types.get_val_integer  val + 1)
      Types.Float    -> Types.set_val_floating (Types.get_val_floating val + 1)
      _              -> val

{-------------------------------------------------------------------------------
  Arbitrary
-------------------------------------------------------------------------------}

data Object =
    Integer CLLong
  | Floating CFloat
  deriving stock (Show, Eq)

getVal :: Object -> Types.Val
getVal = \case
    Integer x -> Types.set_val_integer x
    Floating x -> Types.set_val_floating x

getTyp :: Object -> Types.Typ
getTyp = \case
    Integer{}  -> Types.LongLong
    Floating{} -> Types.Float

instance Arbitrary Object where
  arbitrary = oneof [ Integer <$> arbitrary, Floating <$> arbitrary ]
  shrink = \case
      Integer x -> Integer <$> shrink x
      Floating x -> Floating <$> shrink x
