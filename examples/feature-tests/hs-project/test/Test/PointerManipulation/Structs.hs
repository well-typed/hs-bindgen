{-# OPTIONS_GHC -Wno-orphans #-}

module Test.PointerManipulation.Structs (
    tests
    -- * Properties (exported for haddocks)
  , prop_applyValue_equiv_applyPointer_structs
  , prop_applyPointer_equiv_applyPointerFields_structs
    -- * Static properties (exported for haddocks)
  , dict_point_hasField_x
  , dict_point_hasField_y
  ) where

import Control.Exception (evaluate)
import Data.Constraint (Dict (Dict))
import Data.Proxy (Proxy (Proxy))
import Foreign.C (CInt)
import GHC.Records (HasField)
import Optics ((%~), (&))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.QuickCheck

import Generated.PointerManipulation.Structs qualified as Types
import Test.PointerManipulation.Util (ComposableFunc (..), FieldFunc (..),
                                      prop_applyPointer_equiv_applyPointerFields,
                                      prop_applyValue_equiv_applyPointer)
import Test.Util.Orphans ()

tests :: TestTree
tests = testGroup "Test.PointerManipulation.Structs" [
      testProperty "prop_applyValue_equiv_applyPointer_structs"
        prop_applyValue_equiv_applyPointer_structs
    , testProperty "prop_applyValue_equiv_applyPointer_structs"
        prop_applyPointer_equiv_applyPointerFields_structs

    , testDict "prop_point_hasField_x" dict_point_hasField_x
    , testDict "prop_point_hasField_y" dict_point_hasField_y
    ]

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- | See 'prop_applyValue_equiv_applyPointer'
prop_applyValue_equiv_applyPointer_structs :: Func Types.Point -> Types.Point -> Property
prop_applyValue_equiv_applyPointer_structs =
    prop_applyValue_equiv_applyPointer @Types.Point

-- | See 'prop_applyPointer_equiv_applyPointerFields'
prop_applyPointer_equiv_applyPointerFields_structs :: Func Types.Point -> Types.Point -> Property
prop_applyPointer_equiv_applyPointerFields_structs =
    prop_applyPointer_equiv_applyPointerFields @Types.Point

{-------------------------------------------------------------------------------
  Static properties
-------------------------------------------------------------------------------}

testDict :: TestName -> Dict c -> TestTree
testDict name d = testProperty name $ once $ ioProperty @Bool $ do
    Dict <- evaluate d
    pure True

dict_point_hasField_x :: Dict (HasField "point_x" Types.Point CInt)
dict_point_hasField_x = Dict

dict_point_hasField_y :: Dict (HasField "point_y" Types.Point CInt)
dict_point_hasField_y = Dict

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

instance Arbitrary Types.Point where
  arbitrary = Types.Point <$> arbitrary <*> arbitrary
  shrink (Types.Point x y) =
      [ Types.Point x' y' | (x', y') <- shrink (x, y) ]

instance ComposableFunc Types.Point where
  -- | A function of type point-to-point
  --
  -- Crucially, the function is decomposed into functions on the point's /fields/,
  -- so that we can apply them separately using the pointer manipulation API. See
  -- 'applyPointerFields'.
  data Func Types.Point = FuncPoint {
      point_x :: Fun CInt CInt
    , point_y :: Fun CInt CInt
    }

  composed :: Func Types.Point -> Types.Point -> Types.Point
  composed f p =
      p & #point_x %~ applyFun f.point_x
        & #point_y %~ applyFun f.point_y

  decomposed :: Func Types.Point -> [FieldFunc Types.Point]
  decomposed f = [
      FieldFunc (Proxy @"point_x") (applyFun f.point_x)
    , FieldFunc (Proxy @"point_y") (applyFun f.point_y)
    ]

deriving stock instance Show (Func Types.Point)

instance Arbitrary (Func Types.Point) where
  arbitrary = FuncPoint <$> arbitrary <*> arbitrary
  shrink (FuncPoint x y) =
      [ FuncPoint x' y'
      | (x', y') <- shrink (x, y)
      ]
