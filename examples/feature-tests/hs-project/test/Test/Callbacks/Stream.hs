{-# LANGUAGE DataKinds #-}

module Test.Callbacks.Stream (
    tests
    -- * Exported for haddocks
  , prop_stream_fib_array_sum
  ) where

import Data.IORef
import Data.Vector.Storable qualified as VS
import Foreign.C.Types
import Test.Tasty
import Test.Tasty.QuickCheck

import HsBindgen.Runtime.ConstantArray qualified as CA
import HsBindgen.Runtime.Prelude
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import Generated.Callbacks.Stream qualified as Types
import Generated.Callbacks.Stream.Safe qualified as Safe
import Test.Util (showBucketsOf, showPowersOf10)

tests :: TestTree
tests = testGroup "Test.Callbacks.Stream" [
      testProperty "prop_stream_fib_array_sum" prop_stream_fib_array_sum
    ]

-- | \( \forall n. \text{sum} ~ (\text{take} ~ n ~ \text{fibs}) = \text{Safe.stream_fib_array} ~ n ~ (\text{toFunPtr} ~ \text{sum}) \)
prop_stream_fib_array_sum :: NonNegative (Small Int) -> Property
prop_stream_fib_array_sum (NonNegative (Small n)) = ioProperty $ do
      accRef <- newIORef 0
      view <- Types.View_chunk <$> toFunPtr (sumChunks accRef)
      ret <- Safe.stream_fib_array (fromIntegral nChunks) view
      cValue <- readIORef accRef
      pure
        $ tabulate "number of fibs" [showBucketsOf 5 n]
        $ tabulate "number of chunks" [showBucketsOf 5 nChunks]
        $ tabulate "sum of fibs" [showPowersOf10 $ fromIntegral hsValue]
        $ hsValue === cValue .&&. ret === 0
  where
    nChunks = ceilDivChunkSize n

    hsValue = sum $ take n fibs

    sumChunks :: IORef CLLong -> Types.View_chunk_Aux
    sumChunks accRef = Types.View_chunk_Aux $ \i chunkPtr -> do
        Types.Chunk chunk <- PtrConst.peek chunkPtr
        let chunk' = VS.take (n - fromIntegral i * chunkSize) $ snd $ CA.toVector chunk
        let x = VS.sum chunk'
        modifyIORef' accRef (+ x)
        pure 0

    ceilDivChunkSize :: Int -> Int
    ceilDivChunkSize x = (x + chunkSize - 1) `div` chunkSize

    chunkSize :: Int
    chunkSize = 4
      where
        _unused :: Types.Chunk -> ConstantArray 4 CLLong
        _unused = \case Types.Chunk xs -> xs

    -- | naive implementation, sufficient for testing
    fibs :: [CLLong]
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
