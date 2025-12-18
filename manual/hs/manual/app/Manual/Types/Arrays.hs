{-# LANGUAGE DataKinds #-}

module Manual.Types.Arrays (examples) where

import Foreign as F
import GHC.TypeNats

import HsBindgen.Runtime.ConstantArray qualified as CA
import HsBindgen.Runtime.ConstPtr
import HsBindgen.Runtime.IncompleteArray qualified as IA

import Arrays qualified
import Arrays.Global qualified as Arrays
import Arrays.Safe qualified as Arrays
import Manual.Tools

{-------------------------------------------------------------------------------
  Array utilities
-------------------------------------------------------------------------------}

reverseConstantArray :: (Storable a, Show a, KnownNat n) => Ptr (CA.ConstantArray n a) -> IO ()
reverseConstantArray ptr = do
    -- Print the input contents
    xs <- F.peek ptr
    print xs
    -- Reverse the array
    let ys = CA.fromList . reverse . CA.toList $ xs
    F.poke ptr ys
    -- Print the output contents
    zs <- F.peek ptr
    print zs

reverseConstantArrayElems :: (Storable a, Show a, KnownNat n) => Ptr (CA.ConstantArray n a) -> IO ()
reverseConstantArrayElems ptr = do
    let (p, ptr') = CA.toFirstElemPtr ptr
    -- Print the input contents
    xs <- F.peekArray (CA.intVal p) ptr'
    print xs
    -- Reverse the array
    let ys = reverse xs
    F.pokeArray ptr' ys
    -- Print the output contents
    zs <- F.peekArray (CA.intVal p) ptr'
    print zs

reverseIncompleteArray :: (Storable a, Show a) => Int -> Ptr (IA.IncompleteArray a) -> IO ()
reverseIncompleteArray n ptr = do
    -- Print the input contents
    xs <- IA.peekArray n ptr
    print xs
    -- Reverse the array
    let ys = IA.fromList . reverse . IA.toList $ xs
    IA.pokeArray ptr ys
    -- Print the output contents
    zs <- IA.peekArray n ptr
    print zs

reverseIncompleteArrayElems :: (Storable a, Show a) => Int -> Ptr (IA.IncompleteArray a) -> IO ()
reverseIncompleteArrayElems n ptr = do
    let ptr' = IA.toFirstElemPtr ptr
    -- Print the input contents
    xs <- F.peekArray n ptr'
    print xs
    -- Reverse the array
    let ys = reverse xs
    F.pokeArray ptr' ys
    -- Print the output contents
    zs <- F.peekArray n ptr'
    print zs

transposeMatrix :: Arrays.Matrix -> IO Arrays.Matrix
transposeMatrix inputMatrix =
    CA.withPtr inputMatrix $ \inputPtr -> do
      F.alloca $ \(outputPtr :: Ptr Arrays.Matrix) -> do
        Arrays.transpose_wrapper (ConstPtr inputPtr) (snd $ CA.toFirstElemPtr outputPtr)
        peek outputPtr

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: IO ()
examples = do
    section "Arrays"

    subsection "Global variables"
    reverseConstantArray Arrays.arr1
    reverseConstantArrayElems Arrays.arr1

    reverseConstantArray Arrays.arr2
    reverseConstantArrayElems Arrays.arr2

    reverseIncompleteArray 3 Arrays.arr3
    reverseIncompleteArrayElems 3 Arrays.arr3

    print =<< F.peek Arrays.sudoku
    print =<< IA.peekArray 2 Arrays.triplets

    subsection "Matrix transpose"
    let inputMatrix = Arrays.Matrix $
          CA.fromList [
              Arrays.Triplet $ CA.fromList [1, 2, 3]
            , Arrays.Triplet $ CA.fromList [4, 5, 6]
            , Arrays.Triplet $ CA.fromList [7, 8, 9]
            ]
    print inputMatrix
    outputMatrix <- transposeMatrix inputMatrix
    print outputMatrix

    subsection "Complex example"
    ts <- IA.peekArray 2 Arrays.triplets
    let tripletAddresses = [advancePtr (IA.toFirstElemPtr Arrays.triplets) n | n <- [0..]]
    print (zip (IA.toList ts) tripletAddresses)
    print =<< IA.peekArray 3 Arrays.global_triplet_ptrs
    Arrays.pretty_print_triplets (castPtr Arrays.global_triplet_ptrs)
