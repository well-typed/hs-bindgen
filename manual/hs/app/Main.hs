module Main where

import Foreign
import System.IO.Unsafe

import Example
import Foreign.C

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
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    --
    -- Simple struct
    --

    let triple = mkTriple 1 2 3
    print triple

    --
    -- Simple enum
    --

    print (indexTriple triple A)

    --
    -- Typedefs
    --

    print (sumTriple triple)
    print (averageTriple triple)

    --
    -- Macros
    --

    buffer <- mallocForeignPtrBytes 8
    (x :: Word32) <- withForeignPtr buffer $ \ptr -> do
      poke (plusPtr ptr (fromIntegral fIELD_OFFSET)) (1234 :: Word32)
      peek (pTR_TO_FIELD ptr)
    print x
    print (pTR_TO_FIELD (1 :: CLong))

    year :: YEAR <- alloca $ \ptr -> do
      poke ptr $ Date (YEAR 2025) (MONTH 12) (DAY 25)
      getYear ptr
    print year

    --
    -- Unions
    --

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

    --
    -- Awkward names
    --

    拜拜
    cϒ
    import'
