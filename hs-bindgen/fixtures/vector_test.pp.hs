{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude ((<*>), (>>), Eq, IO, Int, Show, pure)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <vector_test.h>"
  , "vector *hs_bindgen_test_vector_test_c8cd49ec7dbcac25 ("
  , "  double arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return new_vector(arg1, arg2);"
  , "}"
  , "/* get_new_vector_ptr */"
  , "__attribute__ ((const))"
  , "vector *(*hs_bindgen_test_vector_test_7672b9e7f001c998 (void)) ("
  , "  double arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &new_vector;"
  , "}"
  ]))

{-| __defined at:__ @vector_test.h:1:9@

    __exported by:__ @vector_test.h@
-}
data Vector = Vector
  { vector_x :: FC.CDouble
    {- ^ __C declaration:__ @x@

         __defined at:__ @vector_test.h:2:12@

         __exported by:__ @vector_test.h@
    -}
  , vector_y :: FC.CDouble
    {- ^ __C declaration:__ @y@

         __defined at:__ @vector_test.h:3:12@

         __exported by:__ @vector_test.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Vector where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Vector
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Vector vector_x2 vector_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) vector_x2
            >> F.pokeByteOff ptr0 (8 :: Int) vector_y3

{-| __C declaration:__ @new_vector@

    __defined at:__ @vector_test.h:6:9@

    __exported by:__ @vector_test.h@
-}
foreign import ccall safe "hs_bindgen_test_vector_test_c8cd49ec7dbcac25" new_vector ::
     FC.CDouble
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @y@
     -}
  -> IO (Ptr.Ptr Vector)

foreign import ccall unsafe "hs_bindgen_test_vector_test_7672b9e7f001c998" hs_bindgen_test_vector_test_7672b9e7f001c998 ::
     IO (Ptr.FunPtr (FC.CDouble -> FC.CDouble -> IO (Ptr.Ptr Vector)))

{-# NOINLINE new_vector_ptr #-}

{-| __C declaration:__ @new_vector@

    __defined at:__ @vector_test.h:6:9@

    __exported by:__ @vector_test.h@
-}
new_vector_ptr :: Ptr.FunPtr (FC.CDouble -> FC.CDouble -> IO (Ptr.Ptr Vector))
new_vector_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_vector_test_7672b9e7f001c998
