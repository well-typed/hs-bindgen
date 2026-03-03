module Test.Callbacks.Structs (
    tests
    -- * Properties (exported for haddocks)
  , prop_apply_point_op_scale_2_point
  ) where

import Foreign.Ptr (FunPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import HsBindgen.Runtime.Prelude (safeCastFunPtr)

import Generated.Callbacks.Structs qualified as Types
import Generated.Callbacks.Structs.FunPtr qualified as FunPtr
import Generated.Callbacks.Structs.Safe qualified as Safe

tests :: TestTree
tests = testGroup "Test.Callbacks.Structs" [
      testProperty "prop_apply_point_op_scale_2_point"
        prop_apply_point_op_scale_2_point
    ]

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- |
-- \[
--  \forall p.
--      \text{scale2Point} ~ p
--    = \text{Safe.apply_point_op}
--        ~ \text{FunPtr.scale_2_point}
--        ~ p
-- \]
--
prop_apply_point_op_scale_2_point :: Point -> Property
prop_apply_point_op_scale_2_point (Point x) = ioProperty $ do
    z <- Safe.apply_point_op scale_2_pointPtr x
    pure $ scale2Point x === z
  where
    scale_2_pointPtr :: FunPtr Types.Point_op
    scale_2_pointPtr = safeCastFunPtr FunPtr.scale_2_point

{-------------------------------------------------------------------------------
  Modelled callback function
-------------------------------------------------------------------------------}

scale2Point :: Types.Point -> Types.Point
scale2Point (Types.Point x y) = Types.Point (2 * x) (2 * y)

{-------------------------------------------------------------------------------
  Arbitrary
-------------------------------------------------------------------------------}

newtype Point = Point Types.Point
  deriving stock (Show, Eq)

instance Arbitrary Point where
  arbitrary = Point <$> (Types.Point <$> arbitrary <*> arbitrary)
  shrink (Point (Types.Point x y)) =
      [ Point (Types.Point x' y') | (x', y') <- shrink (x, y) ]
