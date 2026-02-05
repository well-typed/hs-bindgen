module Main (main) where

import Foreign
import Foreign.C

import Test.Literate.Test01 qualified as Test01
import Test.Literate.Test02 qualified as Test02
import Test.Literate.TestPointer qualified as TestPointer
import Test.Literate.TestSafe qualified as TestSafe
import Test.Literate.TestSafeAndUnsafe qualified as TestSafeAndUnsafe
import Test.Literate.TestUnsafe qualified as TestUnsafe

{-------------------------------------------------------------------------------
  These are just sanity checks that literate mode works at all.

  * Test01 and Test02 are the minimal hs-bindgen invocation
  * TestSafe explicitly uses the @--safe@ option (which is anyway default)
  * TestUnsafe is similar, but using @--unsafe@
  * TestPointer similar again, using @--pointer@
  * TestSafeAndUnsafe generates /both/ safe /and unsafe functions

  NOTE: Although @--single-file@ is the default in literate mode, if users want
  to override which categories they want, or use suffixes, then @--single-file@
  must still be explicitly passed as an argument (otherwise the parser won't
  accept arguments like @--safe@ and friends).
-------------------------------------------------------------------------------}

_test01_data :: Test01.StructBasic
_test01_data = Test01.StructBasic 0 0

_test01_fun :: CInt -> IO Test01.Thing
_test01_fun = Test01.thing_fun_2

_test02_data :: Test02.Event
_test02_data = Test02.Event 0 nullPtr (CTime 0)

_testSafe_data :: TestSafe.StructBasic
_testSafe_data = TestSafe.StructBasic 0 0

_testSafe_fun :: CInt -> IO TestSafe.Thing
_testSafe_fun = TestSafe.thing_fun_2

_testUnsafe_data :: TestUnsafe.StructBasic
_testUnsafe_data = TestUnsafe.StructBasic 0 0

_testUnsafe_fun :: CInt -> IO TestUnsafe.Thing
_testUnsafe_fun = TestUnsafe.thing_fun_2

_testPointer_data :: TestPointer.StructBasic
_testPointer_data = TestPointer.StructBasic 0 0

_testPointer_fun :: FunPtr (CInt -> IO TestPointer.Thing)
_testPointer_fun = TestPointer.thing_fun_2

_testSafeAndUnsafe_data :: TestSafeAndUnsafe.StructBasic
_testSafeAndUnsafe_data = TestSafeAndUnsafe.StructBasic 0 0

_testSafeAndUnsafe_fun_safe :: CInt -> IO TestSafeAndUnsafe.Thing
_testSafeAndUnsafe_fun_safe = TestSafeAndUnsafe.thing_fun_2__safe

_testSafeAndUnsafe_fun_unsafe :: CInt -> IO TestSafeAndUnsafe.Thing
_testSafeAndUnsafe_fun_unsafe = TestSafeAndUnsafe.thing_fun_2__unsafe

{-------------------------------------------------------------------------------
  Main

  The fact that this compiles at all suffices.
-------------------------------------------------------------------------------}

main :: IO ()
main = return ()
