{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Argv.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (IO)
import RPM.Argv

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/argv.h>"
  , "/* get_argvPrint_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c9aaabc38e2ae499 (void)) ("
  , "  char const *arg1,"
  , "  ARGV_const_t arg2,"
  , "  FILE *arg3"
  , ")"
  , "{"
  , "  return &argvPrint;"
  , "}"
  , "/* get_argiFree_ptr */"
  , "__attribute__ ((const))"
  , "ARGI_t (*hs_bindgen_30cde372d0112e3b (void)) ("
  , "  ARGI_t arg1"
  , ")"
  , "{"
  , "  return &argiFree;"
  , "}"
  , "/* get_argvNew_ptr */"
  , "__attribute__ ((const))"
  , "ARGV_t (*hs_bindgen_f336a566814716f4 (void)) (void)"
  , "{"
  , "  return &argvNew;"
  , "}"
  , "/* get_argvFree_ptr */"
  , "__attribute__ ((const))"
  , "ARGV_t (*hs_bindgen_95bc6e5b53eecbf5 (void)) ("
  , "  ARGV_t arg1"
  , ")"
  , "{"
  , "  return &argvFree;"
  , "}"
  , "/* get_argiCount_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_a058653f2a26113f (void)) ("
  , "  ARGI_const_t arg1"
  , ")"
  , "{"
  , "  return &argiCount;"
  , "}"
  , "/* get_argiData_ptr */"
  , "__attribute__ ((const))"
  , "ARGint_t (*hs_bindgen_e134c3aeeb0ae512 (void)) ("
  , "  ARGI_const_t arg1"
  , ")"
  , "{"
  , "  return &argiData;"
  , "}"
  , "/* get_argvCount_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_129b01445cc89fc5 (void)) ("
  , "  ARGV_const_t arg1"
  , ")"
  , "{"
  , "  return &argvCount;"
  , "}"
  , "/* get_argvData_ptr */"
  , "__attribute__ ((const))"
  , "ARGV_t (*hs_bindgen_18321d7413886c0a (void)) ("
  , "  ARGV_t arg1"
  , ")"
  , "{"
  , "  return &argvData;"
  , "}"
  , "/* get_argvCmp_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_931c6381058a8329 (void)) ("
  , "  void const *arg1,"
  , "  void const *arg2"
  , ")"
  , "{"
  , "  return &argvCmp;"
  , "}"
  , "/* get_argvSort_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_8183150be5aaf9c7 (void)) ("
  , "  ARGV_t arg1,"
  , "  signed int (*arg2) ("
  , "  void const *arg1,"
  , "  void const *arg2"
  , ")"
  , ")"
  , "{"
  , "  return &argvSort;"
  , "}"
  , "/* get_argvSearch_ptr */"
  , "__attribute__ ((const))"
  , "ARGV_t (*hs_bindgen_6ef1b012357c3fab (void)) ("
  , "  ARGV_const_t arg1,"
  , "  char const *arg2,"
  , "  signed int (*arg3) ("
  , "  void const *arg1,"
  , "  void const *arg2"
  , ")"
  , ")"
  , "{"
  , "  return &argvSearch;"
  , "}"
  , "/* get_argiAdd_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_177a5b4e6d28389a (void)) ("
  , "  ARGI_t *arg1,"
  , "  signed int arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return &argiAdd;"
  , "}"
  , "/* get_argvAdd_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_ceb2df925d8fbd2c (void)) ("
  , "  ARGV_t *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return &argvAdd;"
  , "}"
  , "/* get_argvAddN_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_9ca30fdb1b30e09a (void)) ("
  , "  ARGV_t *arg1,"
  , "  char const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return &argvAddN;"
  , "}"
  , "/* get_argvAddNum_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_dae6a9a07e6d70f4 (void)) ("
  , "  ARGV_t *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &argvAddNum;"
  , "}"
  , "/* get_argvAppend_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_4f495d86afe06c82 (void)) ("
  , "  ARGV_t *arg1,"
  , "  ARGV_const_t arg2"
  , ")"
  , "{"
  , "  return &argvAppend;"
  , "}"
  , "/* get_argvSplitString_ptr */"
  , "__attribute__ ((const))"
  , "ARGV_t (*hs_bindgen_bbd6eb28a36917ea (void)) ("
  , "  char const *arg1,"
  , "  char const *arg2,"
  , "  argvFlags arg3"
  , ")"
  , "{"
  , "  return &argvSplitString;"
  , "}"
  , "/* get_argvSplit_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_8297fa6553305d15 (void)) ("
  , "  ARGV_t *arg1,"
  , "  char const *arg2,"
  , "  char const *arg3"
  , ")"
  , "{"
  , "  return &argvSplit;"
  , "}"
  , "/* get_argvJoin_ptr */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_73cedfef9166d6a0 (void)) ("
  , "  ARGV_const_t arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return &argvJoin;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_c9aaabc38e2ae499" hs_bindgen_c9aaabc38e2ae499 ::
     IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> ARGV_const_t -> (Ptr.Ptr HsBindgen.Runtime.Prelude.CFile) -> IO ()))

{-# NOINLINE argvPrint_ptr #-}

{-|

  > rpmargv

  Print argv array elements.

  [__@msg@ /(input)/__]: output message prefix (or NULL)

  [__@argv@ /(input)/__]: argv array

  [__@fp@ /(input)/__]: output file handle (NULL uses stderr)

__C declaration:__ @argvPrint@

__defined at:__ @rpm\/argv.h:34:6@

__exported by:__ @rpm\/argv.h@
-}
argvPrint_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> ARGV_const_t -> (Ptr.Ptr HsBindgen.Runtime.Prelude.CFile) -> IO ())
argvPrint_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c9aaabc38e2ae499

foreign import ccall unsafe "hs_bindgen_30cde372d0112e3b" hs_bindgen_30cde372d0112e3b ::
     IO (Ptr.FunPtr (ARGI_t -> IO ARGI_t))

{-# NOINLINE argiFree_ptr #-}

{-|

  > rpmargv

  Destroy an argi array.

  [__@argi@ /(input)/__]: argi array

  __returns:__ NULL always

__C declaration:__ @argiFree@

__defined at:__ @rpm\/argv.h:41:8@

__exported by:__ @rpm\/argv.h@
-}
argiFree_ptr :: Ptr.FunPtr (ARGI_t -> IO ARGI_t)
argiFree_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_30cde372d0112e3b

foreign import ccall unsafe "hs_bindgen_f336a566814716f4" hs_bindgen_f336a566814716f4 ::
     IO (Ptr.FunPtr (IO ARGV_t))

{-# NOINLINE argvNew_ptr #-}

{-|

  > rpmargv

  Create an empty argv array.

  __returns:__ pointer to empty argv

__C declaration:__ @argvNew@

__defined at:__ @rpm\/argv.h:48:8@

__exported by:__ @rpm\/argv.h@
-}
argvNew_ptr :: Ptr.FunPtr (IO ARGV_t)
argvNew_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f336a566814716f4

foreign import ccall unsafe "hs_bindgen_95bc6e5b53eecbf5" hs_bindgen_95bc6e5b53eecbf5 ::
     IO (Ptr.FunPtr (ARGV_t -> IO ARGV_t))

{-# NOINLINE argvFree_ptr #-}

{-|

  > rpmargv

  Destroy an argv array.

  [__@argv@ /(input)/__]: argv array

  __returns:__ NULL always

__C declaration:__ @argvFree@

__defined at:__ @rpm\/argv.h:55:8@

__exported by:__ @rpm\/argv.h@
-}
argvFree_ptr :: Ptr.FunPtr (ARGV_t -> IO ARGV_t)
argvFree_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_95bc6e5b53eecbf5

foreign import ccall unsafe "hs_bindgen_a058653f2a26113f" hs_bindgen_a058653f2a26113f ::
     IO (Ptr.FunPtr (ARGI_const_t -> IO FC.CInt))

{-# NOINLINE argiCount_ptr #-}

{-|

  > rpmargv

  Return no. of elements in argi array.

  [__@argi@ /(input)/__]: argi array

  __returns:__ no. of elements

__C declaration:__ @argiCount@

__defined at:__ @rpm\/argv.h:62:5@

__exported by:__ @rpm\/argv.h@
-}
argiCount_ptr :: Ptr.FunPtr (ARGI_const_t -> IO FC.CInt)
argiCount_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a058653f2a26113f

foreign import ccall unsafe "hs_bindgen_e134c3aeeb0ae512" hs_bindgen_e134c3aeeb0ae512 ::
     IO (Ptr.FunPtr (ARGI_const_t -> IO ARGint_t))

{-# NOINLINE argiData_ptr #-}

{-|

  > rpmargv

  Return data from argi array.

  [__@argi@ /(input)/__]: argi array

  __returns:__ argi array data address

__C declaration:__ @argiData@

__defined at:__ @rpm\/argv.h:69:10@

__exported by:__ @rpm\/argv.h@
-}
argiData_ptr :: Ptr.FunPtr (ARGI_const_t -> IO ARGint_t)
argiData_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e134c3aeeb0ae512

foreign import ccall unsafe "hs_bindgen_129b01445cc89fc5" hs_bindgen_129b01445cc89fc5 ::
     IO (Ptr.FunPtr (ARGV_const_t -> IO FC.CInt))

{-# NOINLINE argvCount_ptr #-}

{-|

  > rpmargv

  Return no. of elements in argv array.

  [__@argv@ /(input)/__]: argv array

  __returns:__ no. of elements

__C declaration:__ @argvCount@

__defined at:__ @rpm\/argv.h:76:5@

__exported by:__ @rpm\/argv.h@
-}
argvCount_ptr :: Ptr.FunPtr (ARGV_const_t -> IO FC.CInt)
argvCount_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_129b01445cc89fc5

foreign import ccall unsafe "hs_bindgen_18321d7413886c0a" hs_bindgen_18321d7413886c0a ::
     IO (Ptr.FunPtr (ARGV_t -> IO ARGV_t))

{-# NOINLINE argvData_ptr #-}

{-|

  > rpmargv

  Return data from argv array.

  [__@argv@ /(input)/__]: argv array

  __returns:__ argv array data address

__C declaration:__ @argvData@

__defined at:__ @rpm\/argv.h:83:8@

__exported by:__ @rpm\/argv.h@
-}
argvData_ptr :: Ptr.FunPtr (ARGV_t -> IO ARGV_t)
argvData_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_18321d7413886c0a

foreign import ccall unsafe "hs_bindgen_931c6381058a8329" hs_bindgen_931c6381058a8329 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.Ptr Void) -> IO FC.CInt))

{-# NOINLINE argvCmp_ptr #-}

{-|

  > rpmargv

  Compare argv arrays (qsort/bsearch).

  [__@a@ /(input)/__]: 1st instance address

  [__@b@ /(input)/__]: 2nd instance address

  __returns:__ result of comparison

__C declaration:__ @argvCmp@

__defined at:__ @rpm\/argv.h:91:5@

__exported by:__ @rpm\/argv.h@
-}
argvCmp_ptr :: Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.Ptr Void) -> IO FC.CInt)
argvCmp_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_931c6381058a8329

foreign import ccall unsafe "hs_bindgen_8183150be5aaf9c7" hs_bindgen_8183150be5aaf9c7 ::
     IO (Ptr.FunPtr (ARGV_t -> (Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.Ptr Void) -> IO FC.CInt)) -> IO FC.CInt))

{-# NOINLINE argvSort_ptr #-}

{-|

  > rpmargv

  Sort an argv array.

  [__@argv@ /(input)/__]: argv array

  [__@compar@ /(input)/__]: strcmp-like comparison function, or NULL for argvCmp()

  __returns:__ 0 always

__C declaration:__ @argvSort@

__defined at:__ @rpm\/argv.h:99:5@

__exported by:__ @rpm\/argv.h@
-}
argvSort_ptr :: Ptr.FunPtr (ARGV_t -> (Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.Ptr Void) -> IO FC.CInt)) -> IO FC.CInt)
argvSort_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8183150be5aaf9c7

foreign import ccall unsafe "hs_bindgen_6ef1b012357c3fab" hs_bindgen_6ef1b012357c3fab ::
     IO (Ptr.FunPtr (ARGV_const_t -> (Ptr.Ptr FC.CChar) -> (Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.Ptr Void) -> IO FC.CInt)) -> IO ARGV_t))

{-# NOINLINE argvSearch_ptr #-}

{-|

  > rpmargv

  Find an element in an argv array.

  [__@argv@ /(input)/__]: argv array

  [__@val@ /(input)/__]: string to find

  [__@compar@ /(input)/__]: strcmp-like comparison function, or NULL for argvCmp()

  __returns:__ found string (NULL on failure)

__C declaration:__ @argvSearch@

__defined at:__ @rpm\/argv.h:108:8@

__exported by:__ @rpm\/argv.h@
-}
argvSearch_ptr :: Ptr.FunPtr (ARGV_const_t -> (Ptr.Ptr FC.CChar) -> (Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.Ptr Void) -> IO FC.CInt)) -> IO ARGV_t)
argvSearch_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6ef1b012357c3fab

foreign import ccall unsafe "hs_bindgen_177a5b4e6d28389a" hs_bindgen_177a5b4e6d28389a ::
     IO (Ptr.FunPtr ((Ptr.Ptr ARGI_t) -> FC.CInt -> FC.CInt -> IO FC.CInt))

{-# NOINLINE argiAdd_ptr #-}

{-|

  > rpmargv

  Add an int to an argi array.

  [__@*argip@ /(output)/__]: argi array

  [__@ix@ /(input)/__]: argi array index (or -1 to append)

  [__@val@ /(input)/__]: int arg to add

  __returns:__ 0 always

__C declaration:__ @argiAdd@

__defined at:__ @rpm\/argv.h:118:5@

__exported by:__ @rpm\/argv.h@
-}
argiAdd_ptr :: Ptr.FunPtr ((Ptr.Ptr ARGI_t) -> FC.CInt -> FC.CInt -> IO FC.CInt)
argiAdd_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_177a5b4e6d28389a

foreign import ccall unsafe "hs_bindgen_ceb2df925d8fbd2c" hs_bindgen_ceb2df925d8fbd2c ::
     IO (Ptr.FunPtr ((Ptr.Ptr ARGV_t) -> (Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE argvAdd_ptr #-}

{-|

  > rpmargv

  Add a string to an argv array.

  [__@*argvp@ /(output)/__]: argv array

  [__@val@ /(input)/__]: string arg to append

  __returns:__ 0 always

__C declaration:__ @argvAdd@

__defined at:__ @rpm\/argv.h:126:5@

__exported by:__ @rpm\/argv.h@
-}
argvAdd_ptr :: Ptr.FunPtr ((Ptr.Ptr ARGV_t) -> (Ptr.Ptr FC.CChar) -> IO FC.CInt)
argvAdd_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ceb2df925d8fbd2c

foreign import ccall unsafe "hs_bindgen_9ca30fdb1b30e09a" hs_bindgen_9ca30fdb1b30e09a ::
     IO (Ptr.FunPtr ((Ptr.Ptr ARGV_t) -> (Ptr.Ptr FC.CChar) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CInt))

{-# NOINLINE argvAddN_ptr #-}

{-|

  > rpmargv

  Add a string to an argv array, does not need to be nil-terminated.

  [__@*argvp@ /(output)/__]: argv array

  [__@val@ /(input)/__]: string arg to append

  [__@len@ /(input)/__]: string arg length

  __returns:__ 0 always

__C declaration:__ @argvAddN@

__defined at:__ @rpm\/argv.h:135:5@

__exported by:__ @rpm\/argv.h@
-}
argvAddN_ptr :: Ptr.FunPtr ((Ptr.Ptr ARGV_t) -> (Ptr.Ptr FC.CChar) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CInt)
argvAddN_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9ca30fdb1b30e09a

foreign import ccall unsafe "hs_bindgen_dae6a9a07e6d70f4" hs_bindgen_dae6a9a07e6d70f4 ::
     IO (Ptr.FunPtr ((Ptr.Ptr ARGV_t) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE argvAddNum_ptr #-}

{-|

  > rpmargv

  Add a number to an argv array (converting to a string).

  [__@*argvp@ /(output)/__]: argv array

  [__@val@ /(input)/__]: numeric arg to append

  __returns:__ 0 always

__C declaration:__ @argvAddNum@

__defined at:__ @rpm\/argv.h:143:5@

__exported by:__ @rpm\/argv.h@
-}
argvAddNum_ptr :: Ptr.FunPtr ((Ptr.Ptr ARGV_t) -> FC.CInt -> IO FC.CInt)
argvAddNum_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dae6a9a07e6d70f4

foreign import ccall unsafe "hs_bindgen_4f495d86afe06c82" hs_bindgen_4f495d86afe06c82 ::
     IO (Ptr.FunPtr ((Ptr.Ptr ARGV_t) -> ARGV_const_t -> IO FC.CInt))

{-# NOINLINE argvAppend_ptr #-}

{-|

  > rpmargv

  Append one argv array to another.

  [__@*argvp@ /(output)/__]: argv array

  [__@av@ /(input)/__]: argv array to append

  __returns:__ 0 always

__C declaration:__ @argvAppend@

__defined at:__ @rpm\/argv.h:151:5@

__exported by:__ @rpm\/argv.h@
-}
argvAppend_ptr :: Ptr.FunPtr ((Ptr.Ptr ARGV_t) -> ARGV_const_t -> IO FC.CInt)
argvAppend_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4f495d86afe06c82

foreign import ccall unsafe "hs_bindgen_bbd6eb28a36917ea" hs_bindgen_bbd6eb28a36917ea ::
     IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> ArgvFlags -> IO ARGV_t))

{-# NOINLINE argvSplitString_ptr #-}

{-|

  > rpmargv

  Split a string into an argv array.

  [__@str@ /(input)/__]: string arg to split

  [__@seps@ /(input)/__]: separator characters

  [__@flags@ /(input)/__]: flags to control behavior

  __returns:__ argv array

__C declaration:__ @argvSplitString@

__defined at:__ @rpm\/argv.h:167:8@

__exported by:__ @rpm\/argv.h@
-}
argvSplitString_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> ArgvFlags -> IO ARGV_t)
argvSplitString_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_bbd6eb28a36917ea

foreign import ccall unsafe "hs_bindgen_8297fa6553305d15" hs_bindgen_8297fa6553305d15 ::
     IO (Ptr.FunPtr ((Ptr.Ptr ARGV_t) -> (Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE argvSplit_ptr #-}

{-|

  > rpmargv

  Split a string into an argv array.

  [__@*argvp@ /(output)/__]: argv array

  [__@str@ /(input)/__]: string arg to split

  [__@seps@ /(input)/__]: separator characters

  __returns:__ 0 always

__C declaration:__ @argvSplit@

__defined at:__ @rpm\/argv.h:176:5@

__exported by:__ @rpm\/argv.h@
-}
argvSplit_ptr :: Ptr.FunPtr ((Ptr.Ptr ARGV_t) -> (Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> IO FC.CInt)
argvSplit_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8297fa6553305d15

foreign import ccall unsafe "hs_bindgen_73cedfef9166d6a0" hs_bindgen_73cedfef9166d6a0 ::
     IO (Ptr.FunPtr (ARGV_const_t -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE argvJoin_ptr #-}

{-|

  > rpmargv

  Join an argv array into a string.

  [__@*argv@ /(input)/__]: argv array to join

  [__@sep@ /(input)/__]: separator string to use

  __returns:__ malloc'ed string

__C declaration:__ @argvJoin@

__defined at:__ @rpm\/argv.h:184:7@

__exported by:__ @rpm\/argv.h@
-}
argvJoin_ptr :: Ptr.FunPtr (ARGV_const_t -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr FC.CChar))
argvJoin_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_73cedfef9166d6a0
