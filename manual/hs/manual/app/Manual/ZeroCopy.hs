{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module Manual.ZeroCopy (examples) where

import Foreign
import Optics

import HsBindgen.Runtime.BitfieldPtr qualified as BitfieldPtr
import HsBindgen.Runtime.ConstantArray qualified as CA
import HsBindgen.Runtime.Prelude
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import Manual.Tools
import ZeroCopy qualified as Gen
import ZeroCopy.Unsafe qualified as Gen

examples :: IO ()
examples = do
    section "Zero-copy bindings"
    do
      subsection "Introduction"
      introduction

      subsection "Structs"
      structs

      subsection "Unions"
      unions

      subsection "Bit-fields"
      bitFields

      subsection "Typedefs"
      typedefs

      subsection "Pointers"
      pointers

      subsection "Arrays"
      arrays

      subsection "Flexible array members"
      flexibleArrayMembers

      subsection "Multi-dimensional arrays (and typedefs)"
      multiDimensionalArrays

{-------------------------------------------------------------------------------
  Introduction
-------------------------------------------------------------------------------}

introduction :: IO ()
introduction =
    alloca $ \(rectPtr :: Ptr Gen.Rectangle) -> do
      rect <- peek rectPtr
      poke rectPtr $ rect & #rectangle_topleft % #point_x .~ 2

      poke rectPtr.rectangle_topleft.point_x 2

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

structs :: IO ()
structs = do
    let rect = Gen.Rectangle (Gen.Point 1 2) (Gen.Point 3 4)
    with rect $ \ptr -> do
      peek ptr >>= print
      scaleRectangle 2 ptr
      peek ptr >>= print

scaleRectangle :: Int -> Ptr Gen.Rectangle -> IO ()
scaleRectangle (fromIntegral -> c) ptr = do
    x1 <- peek ptr.rectangle_topleft.point_x
    poke ptr.rectangle_topleft.point_x (x1*c)

    y1 <- peek ptr.rectangle_topleft.point_y
    poke ptr.rectangle_topleft.point_y (y1*c)

    x2 <- peek ptr.rectangle_bottomright.point_x
    poke ptr.rectangle_bottomright.point_x (x2*c)

    y2 <- peek ptr.rectangle_bottomright.point_y
    poke ptr.rectangle_bottomright.point_y (y2*c)

{-------------------------------------------------------------------------------
  Unions
-------------------------------------------------------------------------------}

unions :: IO ()
unions = do
    let rect = Gen.Rectangle (Gen.Point 1 1) (Gen.Point 3 3)
        shape = Gen.set_shape_rectangle rect
    with shape $ \ptr -> do
      peek ptr.shape_rectangle >>= print
      poke ptr.shape_circle.circle_midpoint.point_x 5
      poke ptr.shape_circle.circle_midpoint.point_y 5
      poke ptr.shape_circle.circle_radius 2
      peek ptr.shape_circle >>= print

{-------------------------------------------------------------------------------
  Bit-fields
-------------------------------------------------------------------------------}

bitFields :: IO ()
bitFields = do
    let colour = Gen.Colour 0 6 127 255 127
    with colour $ \ptr -> do
      peek ptr >>= print
      uptickBrightness ptr
      peek ptr >>= print
      uptickBrightness ptr -- overflow!
      peek ptr >>= print


uptickBrightness :: Ptr Gen.Colour -> IO ()
uptickBrightness ptr = do
    brightness <- BitfieldPtr.peek ptr.colour_brightness
    BitfieldPtr.poke ptr.colour_brightness (brightness+1)

{-------------------------------------------------------------------------------
  Typedefs
-------------------------------------------------------------------------------}

typedefs :: IO ()
typedefs =
    with (Gen.MyInt 17) $ \ptr -> do
      peek ptr >>= print
      poke ptr (Gen.MyInt 18)
      peek ptr >>= print
      poke ptr.unwrapMyInt 19
      peek ptr >>= print
      poke ptr (Gen.MyInt 20)
      peek ptr.unwrapMyInt >>= print

{-------------------------------------------------------------------------------
  Pointers
-------------------------------------------------------------------------------}

pointers :: IO ()
pointers = do
    let shape  = Gen.set_shape_rectangle
                    (Gen.Rectangle (Gen.Point 0 0) (Gen.Point 10 1))
        colour = Gen.Colour 1 4 0 255 0
    withDrawing shape colour $ \drawing ->
      with drawing $ \ptr -> do
        peek ptr >>= print
        shapePtr <- peek ptr.drawing_shape
        peek shapePtr.shape_rectangle >>= print
        scaleRectangle 10 shapePtr.shape_rectangle
        peek shapePtr.shape_rectangle >>= print
        peek ptr >>= print

withDrawing :: Gen.Shape -> Gen.Colour -> (Gen.Drawing -> IO a) -> IO a
withDrawing shape colour k =
    with shape $ \shapePtr ->
    with colour $ \colourPtr ->
    k (Gen.Drawing shapePtr colourPtr)

{-------------------------------------------------------------------------------
  Arrays
-------------------------------------------------------------------------------}

arrays :: IO ()
arrays = do
    let ticTacToe = Gen.Tic_tac_toe
          (CA.fromList $ [0, 0, 0])
          (CA.fromList $ [0, 0, 0])
          (CA.fromList $ [0, 0, 0])
    with ticTacToe $ \ptr -> do
      peek ptr >>= print
      pokeElemOff ptr.tic_tac_toe_row1.toFirstElemPtr 2 1
      peek ptr >>= print
      pokeElemOff ptr.tic_tac_toe_row2.toFirstElemPtr 1 2
      peek ptr >>= print
      pokeElemOff (ptr.tic_tac_toe_row2.toFirstElemPtr) 2 1
      peek ptr >>= print

{-------------------------------------------------------------------------------
  Flexible array members
-------------------------------------------------------------------------------}

flexibleArrayMembers :: IO ()
flexibleArrayMembers = do
    -- TODO: add a section on zero-copy bindings for flexible array members.
    -- See issue #1286.
    print "TODO"

{-------------------------------------------------------------------------------
  Multi-dimensional arrays (and typedefs)
-------------------------------------------------------------------------------}

multiDimensionalArrays :: IO ()
multiDimensionalArrays = do
    let matrix = Gen.Matrix $ CA.fromList [
            Gen.Triplet $ CA.fromList [1, 2, 3]
          , Gen.Triplet $ CA.fromList [4, 5, 6]
          , Gen.Triplet $ CA.fromList [7, 8, 9]
          ]
    with matrix $ \ptr1 ->
      alloca $ \ptr2 -> do
        peek ptr1 >>= print

        transpose ptr1 ptr2
        peek ptr2 >>= print

        pokeElemOff
          (ptr2.unwrapMatrix.toFirstElemPtr `plusPtrElem` 2
          ).unwrapTriplet.toFirstElemPtr
          1
          0
        peek ptr2 >>= print

        transpose ptr2 ptr1
        peek ptr1 >>= print

transpose :: Ptr Gen.Matrix -> Ptr Gen.Matrix -> IO ()
transpose inputPtr outputPtr =
    Gen.transpose
      (PtrConst.unsafeFromPtr inputPtr.unwrapMatrix.toFirstElemPtr)
      outputPtr.unwrapMatrix.toFirstElemPtr
