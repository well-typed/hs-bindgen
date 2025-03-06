module Example.Hs (
    -- * Definition
    Dim(..)
  , max
  , grow
    -- * Translation to C
  , toC
  , fromC
  ) where

import Prelude hiding (max)

import Foreign.C (CInt)
import Test.QuickCheck

import Example.C qualified as C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Dim =
    Dim2 Int Int
  | Dim3 Int Int Int
  deriving stock (Show, Eq)

max :: Dim -> Int
max (Dim2 x y)   = maximum [x, y]
max (Dim3 x y z) = maximum [x, y, z]

grow :: Dim -> Dim
grow dim = Dim3 (max dim) (max dim) (max dim)

{-------------------------------------------------------------------------------
  QuickCheck
-------------------------------------------------------------------------------}

instance Arbitrary Dim where
  arbitrary = fromEither <$> arbitrary
  shrink    = map fromEither . shrink . toEither

toEither :: Dim -> Either (Int, Int) (Int, Int, Int)
toEither (Dim2 x y)   = Left  (x, y)
toEither (Dim3 x y z) = Right (x, y, z)

fromEither :: Either (Int, Int) (Int, Int, Int) -> Dim
fromEither (Left  (x, y))    = Dim2 x y
fromEither (Right (x, y, z)) = Dim3 x y z

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

toC :: Dim -> C.Dim
toC = \case
     Dim2 x y   -> C.Dim 0 (C.set_DimPayload_dim2 $ C.Dim2 (c x) (c y))
     Dim3 x y z -> C.Dim 1 (C.set_DimPayload_dim3 $ C.Dim3 (c x) (c y) (c z))
  where
    c :: Int -> CInt
    c = fromIntegral

fromC :: C.Dim -> Dim
fromC = \case
    C.Dim 0 (C.get_DimPayload_dim2 -> C.Dim2 x y)   -> Dim2 (c x) (c y)
    C.Dim 1 (C.get_DimPayload_dim3 -> C.Dim3 x y z) -> Dim3 (c x) (c y) (c z)
    C.Dim tag _ -> error $ "fromC: unexpected tag " ++ show tag
  where
    c :: CInt -> Int
    c = fromIntegral

