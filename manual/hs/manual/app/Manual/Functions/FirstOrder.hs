{-# OPTIONS_GHC -Wno-orphans #-}

module Manual.Functions.FirstOrder (examples) where

import Control.Monad ((<=<))
import Foreign as F
import Foreign.C (withCString)
import System.IO.Unsafe

import HsBindgen.Runtime.FunPtr

import Manual.Tools

import Example.Unsafe
import FunctionPointers qualified as FunPtr
import FunctionPointers.FunPtr qualified as FunPtr
import FunctionPointers.Global qualified as FunPtr
import FunctionPointers.Safe qualified as FunPtr

{-------------------------------------------------------------------------------
  Function attributes
-------------------------------------------------------------------------------}

hashSafe :: String -> Int
hashSafe s = fromIntegral $ unsafePerformIO $ withCString s hash

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: IO ()
examples = do
    section "Function attributes"

    withCString "\DC1" $ (print <=< hash)
    print (hashSafe "abc")
    print (square 2)

    section "Function pointers"

    print =<< FunPtr.apply1 FunPtr.square_ptr 4
    print =<< FunPtr.apply1 FunPtr.square_ptr 5
    print =<< FunPtr.apply1 FunPtr.square_ptr 6

    print =<< FunPtr.apply2 FunPtr.plus_ptr 7 8
    print =<< FunPtr.apply2 FunPtr.plus_ptr 9 10
    print =<< FunPtr.apply2 FunPtr.plus_ptr 11 12

    subsection "Implicit function to pointer conversion"
    do
      -- function pointer type in function parameter
      print =<< FunPtr.apply1_pointer_arg (F.castFunPtr FunPtr.square_ptr) 4
      print =<< FunPtr.apply1_pointer_arg (F.castFunPtr FunPtr.square_ptr) 5
      print =<< FunPtr.apply1_pointer_arg (F.castFunPtr FunPtr.square_ptr) 6

      -- function type in function parameter
      print =<< FunPtr.apply1_nopointer_arg (F.castFunPtr FunPtr.square_ptr) 4
      print =<< FunPtr.apply1_nopointer_arg (F.castFunPtr FunPtr.square_ptr) 5
      print =<< FunPtr.apply1_nopointer_arg (F.castFunPtr FunPtr.square_ptr) 6

      subsubsection "Parameters of function type can occur almost anywhere!"
      do -- function type in function result
        apply1FunPtr <- FunPtr.apply1_nopointer_res
        let apply1Fun = fromFunPtr apply1FunPtr
        print =<< apply1Fun (F.castFunPtr FunPtr.square_ptr) 4
      do -- function type in global
        let apply1FunPtr = FunPtr.apply1_nopointer_var
            apply1Fun = fromFunPtr apply1FunPtr
        print =<< apply1Fun (F.castFunPtr FunPtr.square_ptr) 5
      do -- function type in struct field
        let apply1FunPtr = FunPtr.apply1Struct_apply1_nopointer_struct_field FunPtr.apply1_struct
            apply1Fun = fromFunPtr apply1FunPtr
        print =<< apply1Fun (F.castFunPtr FunPtr.square_ptr) 6
      do -- function type in union field
        let apply1FunPtr = FunPtr.get_apply1Union_apply1_nopointer_union_field FunPtr.apply1_union
            apply1Fun = fromFunPtr apply1FunPtr
        print =<< apply1Fun (F.castFunPtr FunPtr.square_ptr) 7
