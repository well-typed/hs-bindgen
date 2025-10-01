{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Manual (main) where

import Control.Exception (bracket)
import Control.Monad (forM_, (<=<))
import Data.Complex
import Data.Vector.Storable qualified as VS
import Foreign as F
import Foreign.C (castCCharToChar, withCString)
import Foreign.C qualified as FC
import GHC.TypeNats
import System.IO.Unsafe
import Text.Read (readEither)

import Arrays.Global qualified as Arrays
import Arrays.Safe qualified as Arrays

import Complex.Global qualified as Complex
import Complex.Safe qualified as Complex

import Example.Global
import Example.Unsafe

import FunctionPointers qualified as FunPtr
import FunctionPointers.FunPtr qualified as FunPtr
import FunctionPointers.Safe qualified as FunPtr
import FunctionPointers.Global qualified as FunPtr

import Game.Player.Safe
import Game.State
import Game.World.Safe

import Globals.Global qualified as Globals

import HsBindgen.Runtime.CEnum (AsCEnum (..), AsSequentialCEnum (..))
import HsBindgen.Runtime.ConstantArray qualified as CA
import HsBindgen.Runtime.FlexibleArrayMember qualified as FLAM
import HsBindgen.Runtime.IncompleteArray qualified as IA

import Manual.Tools

import Structs.Safe

import Vector.Length.Safe
import Vector.Rotate.Safe
import Vector.Safe

import Arrays qualified
import Complex qualified
import Example
import Globals qualified
import Structs

{-------------------------------------------------------------------------------
  Simple struct
-------------------------------------------------------------------------------}

mkTriple :: Int -> Int -> Int -> Triple
mkTriple a b c = unsafePerformIO $
    alloca $ \ptr -> do
      mk_triple (fromIntegral a) (fromIntegral b) (fromIntegral c) ptr
      peek ptr

{-------------------------------------------------------------------------------
  Simple enum
-------------------------------------------------------------------------------}

indexTriple :: Triple -> Index -> Int
indexTriple triple ix = unsafePerformIO $
    with triple $ \ptr -> fromIntegral <$> index_triple ptr ix

{-------------------------------------------------------------------------------
  Typedefs
-------------------------------------------------------------------------------}

sumTriple :: Triple -> Sum
sumTriple triple = unsafePerformIO $
    with triple $ \ptr -> sum_triple ptr

averageTriple :: Triple -> Average
averageTriple triple = unsafePerformIO $
    with triple $ \ptr -> average_triple ptr

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

instance FLAM.HasFlexibleArrayLength FC.CChar Surname where
  flexibleArrayMemberLength x = fromIntegral (surname_len x)

{-------------------------------------------------------------------------------
  Enums
-------------------------------------------------------------------------------}

deriving via AsCEnum HTTP_status instance Enum HTTP_status
deriving newtype instance Bounded HTTP_status

deriving via AsSequentialCEnum Vote instance Enum    Vote
deriving via AsSequentialCEnum Vote instance Bounded Vote

deriving via AsCEnum Descending instance Enum Descending

showCursorKind :: CXCursorKind -> String
showCursorKind = \case
    CXCursor_UnexposedExpr -> "CXCursor_UnexposedExpr"
    CXCursor_UnexposedStmt -> "CXCursor_UnexposedStmt"
    kind -> show kind

-- On Windows the underlying data type generated for `Index` is FC.CInt
-- instead of FC.CUInt.
readEitherIndexWith :: FC.CUInt -> String -> Either String Index
readEitherIndexWith upperBound x = case readEither x of
  Right (Index v) | v > upperBound -> Left $ "index out of bounds: " <> show v
  other                            -> other

{-------------------------------------------------------------------------------
  Function attributes
-------------------------------------------------------------------------------}

hashSafe :: String -> Int
hashSafe s = fromIntegral $ unsafePerformIO $ withCString s hash

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = do

--------------------------------------------------------------------------------
    section "Simple"
    subsection "Simple struct"

    let triple = mkTriple 1 2 3
    print triple

    subsection "Simple enum"

    print (indexTriple triple A)

--------------------------------------------------------------------------------
    section "Typedefs"

    print (sumTriple triple)
    print (averageTriple triple)

--------------------------------------------------------------------------------
    section "Macros"

    buffer <- mallocForeignPtrBytes 8
    (x :: Word32) <- withForeignPtr buffer $ \ptr -> do
      poke (plusPtr ptr (fromIntegral fIELD_OFFSET)) (1234 :: Word32)
      peek (pTR_TO_FIELD ptr)
    print x
    print (pTR_TO_FIELD (1 :: FC.CLong))

    year :: YEAR <- alloca $ \ptr -> do
      poke ptr $ Date (YEAR 2025) (MONTH 12) (DAY 25)
      getYear ptr
    print year

--------------------------------------------------------------------------------
    section "Unions"

    do let occupation = set_occupation_student Student{
           student_university = nullPtr
         , student_year       = 2000
         }
       with occupation $ print_occupation 0

    do let occupation = set_occupation_employee Employee{
           employee_company    = nullPtr
         , employee_supervisor = nullPtr
         , employee_salary     = 100_000
         }
       with occupation $ print_occupation 1

--------------------------------------------------------------------------------
    section "Awkward names"

-- There's a quirk with Apple assembler and LLVM IR that do not accept
-- Unicode characters. So make sure to set SUPPORTS_UNICODE environment
-- variable only if you know your system supports it.
#if defined(SUPPORTS_UNICODE)
    -- On supporting platforms, call the functions with Unicode names.
    拜拜
    cϒ
#else
    -- On macOS/LLVM (e.g.), call the safe functions defined in your bindings module.
    -- We assume they are named `gamma` and `byeBye` in Haskell.
    byeBye
    gamma
#endif

    import'

--------------------------------------------------------------------------------
    section "External binding specifications"

    v <- new_vector 2 1
    print =<< peek v
    print =<< vector_length v
    v' <- vector_rotate v (30 * pi / 180)
    print =<< peek v'
    print =<< vector_length v'

    move_world  $ Game_state nullPtr
    move_player $ Game_state nullPtr

--------------------------------------------------------------------------------
    section "Structs"

    bracket (withCString "Rich" $ \cstr -> surname_alloc_wrapper cstr) surname_free $
      \ptr -> do
        (surname :: Structs.Surname) <- peek ptr
        putStrLn $ "The length of the surname is: " <> show (surname_len surname)
        (surnameWithFlam :: FLAM.WithFlexibleArrayMember FC.CChar Structs.Surname) <-
          FLAM.peekWithFLAM ptr
        let name :: VS.Vector FC.CChar
            name = FLAM.flamExtra surnameWithFlam
        print $ VS.map castCCharToChar name

--------------------------------------------------------------------------------
    section "Enums"

    print [Ok, minBound]
    putStrLn $ "After " ++ show Moved ++ " comes " ++ show (succ Moved)
    putStrLn $ "Possible votes: " ++ show ([minBound .. maxBound] :: [Example.Vote])
    print CXCursor_UnexposedExpr
    putStrLn $ showCursorKind CXCursor_UnexposedExpr
    print (succ Y, pred Y)

    -- Read instance (Index).
    print $ "Read declared (A ~ Index 0): " <> show (read "A" :: Example.Index)
    print $ "Read declared but using undeclared string (Index 0): "
      <> show (read "Index 0" :: Example.Index)
    print $ "Read undeclared (Index 10): " <> show (read "Index 10" :: Example.Index)
    -- Read instance (HTTP_status).
    print $ (read "HTTP_status 200" :: Example.HTTP_status)
    print $ (read "Ok" :: Example.HTTP_status)
    print $ (read "HTTP_status 200" :: Example.HTTP_status) == (read "Ok" :: Example.HTTP_status)
    -- Read instance (overriding).
    print $ (readEitherIndexWith 100 "Index (-1)")

    -- Static inline function
    print =<< mod_10 123

--------------------------------------------------------------------------------
    section "Function attributes"

    withCString "\DC1" $ (print <=< hash)
    print (hashSafe "abc")
    print (square 2)

--------------------------------------------------------------------------------
    section "Globals"
    do
      subsection "Variables"
      config <- peek globalConfig_ptr
      print config
      poke globalConfig_ptr $ config{globalConfig_numThreads = 3}
      printGlobalConfig
      config' <- peek globalConfig_ptr
      print config'

      print =<< peek Globals.globalInt_ptr
      print =<< peek Globals.externGlobalInt_ptr

      -- Constants
      subsection "Constants"
      print Globals.globalConstant
      print Globals.anotherGlobalConstant
      print Globals.staticConst
      print Globals.classless
      print Globals.constArray1
      print =<< IA.peekArray 5 Globals.constArray2_ptr
      print Globals.constTuple
      print =<< F.peek Globals.nonConstTuple_ptr
      -- TODO: the stub loses type qualifier information for everything but the
      -- outer type, so we get type warnings here. See issue #994.
      print =<< F.peek Globals.ptrToConstInt_ptr
      print Globals.constPtrToInt
      print Globals.constPtrToConstInt

--------------------------------------------------------------------------------
    section "Arrays"
    do
      -- Global array variables
      subsection "Global variables"
      reverseConstantArray Arrays.arr1_ptr
      reverseConstantArrayElems Arrays.arr1_ptr

      reverseConstantArray Arrays.arr2_ptr
      reverseConstantArrayElems Arrays.arr2_ptr

      reverseIncompleteArray 3 Arrays.arr3_ptr
      reverseIncompleteArrayElems 3 Arrays.arr3_ptr

      print =<< F.peek Arrays.sudoku_ptr
      print =<< IA.peekArray 2 Arrays.triplets_ptr

      -- Matrix transpose
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

      -- Complex example
      subsection "Complex example"
      ts <- IA.peekArray 2 Arrays.triplets_ptr
      let tripletAddresses = [advancePtr (IA.isFirstElem Arrays.triplets_ptr) n | n <- [0..]]
      print (zip (IA.toList ts) tripletAddresses)
      print =<< IA.peekArray 3 Arrays.global_triplet_ptrs_ptr
      Arrays.pretty_print_triplets_wrapper (castPtr Arrays.global_triplet_ptrs_ptr)

--------------------------------------------------------------------------------
    section "Function pointers"
    do
      print =<< FunPtr.apply1 FunPtr.square_ptr 4
      print =<< FunPtr.apply1 FunPtr.square_ptr 5
      print =<< FunPtr.apply1 FunPtr.square_ptr 6

      print =<< FunPtr.apply2 FunPtr.plus_ptr 7 8
      print =<< FunPtr.apply2 FunPtr.plus_ptr 9 10
      print =<< FunPtr.apply2 FunPtr.plus_ptr 11 12

      subsection "Implicit function to pointer conversion"
      do
        -- function pointer type in function parameter
        print =<< FunPtr.apply1_pointer_arg FunPtr.square_ptr 4
        print =<< FunPtr.apply1_pointer_arg FunPtr.square_ptr 5
        print =<< FunPtr.apply1_pointer_arg FunPtr.square_ptr 6

        -- function type in function parameter
        print =<< FunPtr.apply1_nopointer_arg FunPtr.square_ptr 4
        print =<< FunPtr.apply1_nopointer_arg FunPtr.square_ptr 5
        print =<< FunPtr.apply1_nopointer_arg FunPtr.square_ptr 6

        subsubsection "Parameters of function type can occur almost anywhere!"
        do -- function type in function result
          apply1FunPtr <- FunPtr.apply1_nopointer_res
          let apply1Fun = mkApply1Fun apply1FunPtr
          print =<< apply1Fun FunPtr.square_ptr 4
        do -- function type in global
          let apply1FunPtr = FunPtr.apply1_nopointer_var
              apply1Fun = mkApply1Fun apply1FunPtr
          print =<< apply1Fun FunPtr.square_ptr 5
        do -- function type in struct field
          let apply1FunPtr = FunPtr.apply1Struct_apply1_nopointer_struct_field FunPtr.apply1_struct
              apply1Fun = mkApply1Fun apply1FunPtr
          print =<< apply1Fun FunPtr.square_ptr 6
        do -- function type in union field
          let apply1FunPtr = FunPtr.get_apply1Union_apply1_nopointer_union_field FunPtr.apply1_union
              apply1Fun = mkApply1Fun apply1FunPtr
          print =<< apply1Fun FunPtr.square_ptr 7

--------------------------------------------------------------------------------
    section "Complex types"
    do
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

      doubleArrayExpectedSum <- do
        xs <- F.peek Complex.complex_double_array_ptr
        pure $ sum $ CA.toList $ xs
      doubleArraySum <- do
        array <- F.peek Complex.complex_double_array_ptr
        Complex.sum_complex_array array
      putStrLn $ "  Sum of complex_double_array_ptr: " <> show doubleArraySum
      putStrLn $ "  Expected: " <> show doubleArrayExpectedSum
      putStrLn $ "  Match: " <> show (complexEq 1e-12 doubleArraySum doubleArrayExpectedSum)

{-------------------------------------------------------------------------------
  Arrays
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
    let (p, ptr') = CA.isFirstElem ptr
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
    let ptr' = IA.isFirstElem ptr
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
        Arrays.transpose_wrapper (inputPtr) (snd $ CA.isFirstElem outputPtr)
        peek outputPtr

{-------------------------------------------------------------------------------
  Function pointers
-------------------------------------------------------------------------------}

foreign import ccall "dynamic" mkApply1Fun ::
     F.FunPtr ((F.FunPtr (FC.CInt -> IO FC.CInt)) -> FC.CInt -> IO FC.CInt)
  -> F.FunPtr (FC.CInt -> IO FC.CInt)
  -> FC.CInt
  -> IO FC.CInt
