{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RunManual (main) where

import Control.Exception (bracket)
import Data.Vector.Storable qualified as VS
import Foreign
import Foreign.C (castCCharToChar, withCString)
import Foreign.C qualified as FC
import System.IO.Unsafe
import Text.Read (readEither)

import HsBindgen.Runtime.CEnum (AsCEnum (..), AsSequentialCEnum (..))
import HsBindgen.Runtime.FlexibleArrayMember qualified as FLAM

import Example
import Structs

import Game.Player
import Game.State
import Game.World
import Vector
import Vector.Length
import Vector.Rotate

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
#if defined(mingw32_HOST_OS)
readEitherIndexWith :: FC.CInt -> String -> Either String Index
#else
readEitherIndexWith :: FC.CUInt -> String -> Either String Index
#endif
readEitherIndexWith upperBound x = case readEither x of
  Right (Index v) | v > upperBound -> Left $ "index out of bounds: " <> show v
  other                            -> other

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = do

    section "Simple"

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
    section "Typedefs"
    --

    print (sumTriple triple)
    print (averageTriple triple)


    --
    -- Macros
    section "Macros"
    --

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

    --
    -- Unions
    section "Unions"
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
    section "Awkward names"
    --

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

    --
    -- External binding specifications
    section "External binding specifications"
    --

    v <- new_vector 2 1
    print =<< peek v
    print =<< vector_length v
    v' <- vector_rotate v (30 * pi / 180)
    print =<< peek v'
    print =<< vector_length v'

    move_world  $ Game_state nullPtr
    move_player $ Game_state nullPtr

    --
    -- Structs
    section "Structs"
    --

    bracket (withCString "Rich" $ \cstr -> surname_alloc cstr) surname_free $
      \ptr -> do
        (surname :: Surname) <- peek ptr
        putStrLn $ "The length of the surname is: " <> show (surname_len surname)
        (surnameWithFlam :: FLAM.WithFlexibleArrayMember FC.CChar Surname) <-
          FLAM.peekWithFLAM ptr
        let name :: VS.Vector FC.CChar
            name = FLAM.flamExtra surnameWithFlam
        print $ VS.map castCCharToChar name

    --
    -- Enums
    section "Enums"
    --

    print [Ok, minBound]
    putStrLn $ "After " ++ show Moved ++ " comes " ++ show (succ Moved)
    putStrLn $ "Possible votes: " ++ show ([minBound .. maxBound] :: [Vote])
    print CXCursor_UnexposedExpr
    putStrLn $ showCursorKind CXCursor_UnexposedExpr
    print (succ Y, pred Y)

    -- Read instance (Index).
    print $ "Read declared (A ~ Index 0): " <> show (read "A" :: Index)
    print $ "Read declared but using undeclared string (Index 0): "
      <> show (read "Index 0" :: Index)
    print $ "Read undeclared (Index 10): " <> show (read "Index 10" :: Index)
    -- Read instance (HTTP_status).
    print $ (read "HTTP_status 200" :: HTTP_status)
    print $ (read "Ok" :: HTTP_status)
    print $ (read "HTTP_status 200" :: HTTP_status) == (read "Ok" :: HTTP_status)
    -- Read instance (overriding).
    print $ (readEitherIndexWith 100 "Index (-1)")

    -- Static inline function
    print =<< mod_10 123

{-------------------------------------------------------------------------------
  Aux
-------------------------------------------------------------------------------}

section :: String -> IO ()
section s = do
  putStrLn ""
  putStrLn $ "*** " <> s <> " ***"
  putStrLn ""
