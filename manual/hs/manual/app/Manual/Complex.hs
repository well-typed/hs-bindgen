module Manual.Complex (examples) where

import Control.Monad (forM_)
import Data.Complex
import Foreign as F
import Foreign.C qualified as FC

import HsBindgen.Runtime.ConstantArray qualified as CA

import Manual.Tools

import Complex qualified
import Complex.Global qualified as Complex
import Complex.Safe qualified as Complex

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: IO ()
examples = do
    section "Complex types"

    subsection "Basic Complex Type Read/Write"

    let complexEq :: RealFloat a => a -> Complex a -> Complex a -> Bool
        complexEq tol c1 c2 =
             abs (realPart c1 - realPart c2) < tol
          && abs (imagPart c1 - imagPart c2) < tol

        test_complex
          :: ( Show a
             , Eq a
             , Storable a
             )
          => String -> Ptr a -> a -> IO ()
        test_complex name ptr value = do
          read_val_1 <- F.peek ptr
          putStrLn $ name <> ": " <> show read_val_1
          F.poke ptr value
          read_val_2 <- F.peek ptr
          putStrLn $ name <> ": " <> show value <> " -> " <> show read_val_2 <>
                    " (match: " <> show (value == read_val_2) <> ")"

    test_complex "unsigned short" Complex.global_complex_unsigned_short_ptr (42 :+ 24 :: Complex FC.CUShort)
    test_complex "short"          Complex.global_complex_short_ptr ((-10) :+ 15 :: Complex FC.CShort)
    test_complex "unsigned int"   Complex.global_complex_unsigned_int_ptr (1000 :+ 2000 :: Complex FC.CUInt)
    test_complex "int"            Complex.global_complex_int_ptr ((-500) :+ 750 :: Complex FC.CInt)
    test_complex "char"           Complex.global_complex_char_ptr (65 :+ 90 :: Complex FC.CChar)
    test_complex "float"          Complex.global_complex_float_ptr (1.5 :+ 2.5 :: Complex FC.CFloat)
    test_complex "double"         Complex.global_complex_double_ptr (3.14159 :+ 2.71828 :: Complex FC.CDouble)

    subsection "Arithmetic Functions"

    let a, b, rab :: Complex FC.CDouble
        a = 1.5 :+ 2.5
        b = 4.7 :+ 4.2
        rab = a + b

        c, d, rcd :: Complex FC.CFloat
        c = 2.0 :+ 3.0
        d = 4.0 :+ 5.0
        rcd = c * d

    result_add <- Complex.add_complex a b
    result_mult <- Complex.multiply_complex_f c d

    putStrLn $ "add(" <> show a <> ", " <> show b <> ") = " <> show result_add
    putStrLn $ "  Expected: " <> show rab <>
               ", Match: " <> show (complexEq 1e-6 result_add rab)

    putStrLn $ "multiply(" <> show c <> ", " <> show d <> ") = " <> show result_mult
    putStrLn $ "  Expected: " <> show rcd <>
               ", Match: " <> show (complexEq 1e-6 result_mult rcd)

    subsection "Complex Struct"

    let test_obj = Complex.Complex_object_t
          { Complex.complex_object_t_velocity = 10.0 :+ 20.0
          , Complex.complex_object_t_position = 100.0 :+ 200.0
          , Complex.complex_object_t_id = 42
          }

    putStrLn $ "Created object: " <> show test_obj

    F.with test_obj $ \ptr -> do
      read_obj_1 <- F.peek ptr
      Complex.swap_velocity_position ptr
      read_obj_2 <- F.peek ptr
      putStrLn $ "Before swap: " <> show read_obj_1
      putStrLn $ "After swap: " <> show read_obj_2
      putStrLn $ "  Size: " <> show (F.sizeOf test_obj) <> " bytes"
      putStrLn $ "  Alignment: " <> show (F.alignment test_obj) <> " bytes"

    subsection "Complex Arrays"

    putStrLn "Reading first 5 elements of arrays:"
    putStrLn "Float array:"

    forM_ [0..4] $ \i -> do
      val <- F.peekElemOff (F.castPtr Complex.complex_float_array_ptr) i
      putStrLn $ "  [" <> show i <> "] = " <> show (val :: Complex FC.CFloat)

    putStrLn "Double array:"
    forM_ [0..4] $ \i -> do
      val <- F.peekElemOff (F.castPtr Complex.complex_double_array_ptr) i
      putStrLn $ "  [" <> show i <> "] = " <> show (val :: Complex FC.CDouble)

    -- Modify and verify
    putStrLn "Writing new values to index 3:"
    let new_float :: Complex FC.CFloat
        new_float = 99.9 :+ 88.8

        new_double :: Complex FC.CDouble
        new_double = 77.7 :+ 66.6

    orig_float  <- F.peekElemOff (F.castPtr Complex.complex_float_array_ptr) 3
    orig_double <- F.peekElemOff (F.castPtr Complex.complex_double_array_ptr) 3

    F.pokeElemOff (F.castPtr Complex.complex_float_array_ptr) 0 new_float
    F.pokeElemOff (F.castPtr Complex.complex_double_array_ptr) 0 new_double

    read_new_float :: Complex FC.CFloat   <- F.peekElemOff (F.castPtr Complex.complex_float_array_ptr) 3
    read_new_double :: Complex FC.CDouble <- F.peekElemOff (F.castPtr Complex.complex_double_array_ptr) 3

    putStrLn $ "  Float: " <> show (orig_float :: Complex FC.CFloat) <> " -> " <> show read_new_float
    putStrLn $ "  Double: " <> show (orig_double :: Complex FC.CDouble) <> " -> " <> show read_new_double

    putStrLn "Summing complex arrays"

    let complexEq' :: RealFloat a => a -> Complex a -> Complex a -> Bool
        complexEq' tol c1 c2 =
             abs (realPart c1 - realPart c2) < tol
          && abs (imagPart c1 - imagPart c2) < tol

    doubleArrayExpectedSum <- do
      xs <- F.peek Complex.complex_double_array_ptr
      pure $ sum $ CA.toList $ xs
    doubleArraySum <- do
      array <- F.peek Complex.complex_double_array_ptr
      Complex.sum_complex_array array
    putStrLn $ "  Sum of complex_double_array_ptr: " <> show doubleArraySum
    putStrLn $ "  Expected: " <> show doubleArrayExpectedSum
    putStrLn $ "  Match: " <> show (complexEq' 1e-12 doubleArraySum doubleArrayExpectedSum)
