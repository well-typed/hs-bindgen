{-# OPTIONS_GHC -Wno-orphans #-}

module Manual.Functions.FirstOrder (examples) where

import Control.Monad ((<=<))
import Foreign.C (withCString)
import System.IO.Unsafe

import HsBindgen.Runtime.Prelude

import Example.Unsafe
import FunctionPointers qualified as Fun
import FunctionPointers.FunPtr qualified as FunPtr
import FunctionPointers.Global qualified as Fun
import FunctionPointers.Safe qualified as Fun
import Manual.Tools

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

    print =<< Fun.apply1 FunPtr.square 4
    print =<< Fun.apply1 FunPtr.square 5
    print =<< Fun.apply1 FunPtr.square 6

    print =<< Fun.apply2 FunPtr.plus 7 8
    print =<< Fun.apply2 FunPtr.plus 9 10
    print =<< Fun.apply2 FunPtr.plus 11 12

    subsection "Implicit function to pointer conversion"
    do
      -- function pointer type in function parameter
      print =<< Fun.apply1_pointer_arg (safeCastFunPtr FunPtr.square) 4
      print =<< Fun.apply1_pointer_arg (safeCastFunPtr FunPtr.square) 5
      print =<< Fun.apply1_pointer_arg (safeCastFunPtr FunPtr.square) 6

      -- function type in function parameter
      print =<< Fun.apply1_nopointer_arg (safeCastFunPtr FunPtr.square) 4
      print =<< Fun.apply1_nopointer_arg (safeCastFunPtr FunPtr.square) 5
      print =<< Fun.apply1_nopointer_arg (safeCastFunPtr FunPtr.square) 6

      subsubsection "Parameters of function type can occur almost anywhere!"
      do -- function type in function result
        apply1FunPtr <- Fun.apply1_nopointer_res
        let apply1Fun = fromFunPtr apply1FunPtr
        print =<< apply1Fun (safeCastFunPtr FunPtr.square) 4
      do -- function type in global
        let apply1FunPtr = Fun.apply1_nopointer_var
            apply1Fun = fromFunPtr apply1FunPtr
        print =<< apply1Fun (safeCastFunPtr FunPtr.square) 5
      do -- function type in struct field
        let apply1FunPtr = Fun.apply1Struct_apply1_nopointer_struct_field Fun.apply1_struct
            apply1Fun = fromFunPtr apply1FunPtr
        print =<< apply1Fun (safeCastFunPtr FunPtr.square) 6
      do -- function type in union field
        let apply1FunPtr = Fun.get_apply1Union_apply1_nopointer_union_field Fun.apply1_union
            apply1Fun = fromFunPtr apply1FunPtr
        print =<< apply1Fun (safeCastFunPtr FunPtr.square) 7
