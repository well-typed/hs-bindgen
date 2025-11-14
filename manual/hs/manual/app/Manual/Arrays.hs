{-# LANGUAGE DataKinds #-}

module Manual.Arrays (examples) where

import Foreign as F
import GHC.TypeNats

import HsBindgen.Runtime.ConstantArray qualified as CA
import HsBindgen.Runtime.IncompleteArray qualified as IA

import Manual.Tools

import Arrays qualified
import Arrays.Global qualified as Arrays
import Arrays.Safe qualified as Arrays

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
        Arrays.transpose_wrapper (inputPtr) (snd $ CA.toFirstElemPtr outputPtr)
        peek outputPtr

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: IO ()
examples = do
    section "Arrays"

    subsection "Global variables"
    reverseConstantArray Arrays.arr1_ptr
    reverseConstantArrayElems Arrays.arr1_ptr

    reverseConstantArray Arrays.arr2_ptr
    reverseConstantArrayElems Arrays.arr2_ptr

    reverseIncompleteArray 3 Arrays.arr3_ptr
    reverseIncompleteArrayElems 3 Arrays.arr3_ptr

    print =<< F.peek Arrays.sudoku_ptr
    print =<< IA.peekArray 2 Arrays.triplets_ptr

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
    ts <- IA.peekArray 2 Arrays.triplets_ptr
    let tripletAddresses = [advancePtr (IA.toFirstElemPtr Arrays.triplets_ptr) n | n <- [0..]]
    print (zip (IA.toList ts) tripletAddresses)
    print =<< IA.peekArray 3 Arrays.global_triplet_ptrs_ptr
    Arrays.pretty_print_triplets (castPtr Arrays.global_triplet_ptrs_ptr)
