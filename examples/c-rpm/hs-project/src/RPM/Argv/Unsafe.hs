{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Argv.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (IO)
import RPM.Argv

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/argv.h>"
  , "void hs_bindgen_64dd8a7e068379d1 ("
  , "  char const *arg1,"
  , "  ARGV_const_t arg2,"
  , "  FILE *arg3"
  , ")"
  , "{"
  , "  argvPrint(arg1, arg2, arg3);"
  , "}"
  , "ARGI_t hs_bindgen_407c4d68b5937a17 ("
  , "  ARGI_t arg1"
  , ")"
  , "{"
  , "  return argiFree(arg1);"
  , "}"
  , "ARGV_t hs_bindgen_ee91d36ccd468b1c (void)"
  , "{"
  , "  return argvNew();"
  , "}"
  , "ARGV_t hs_bindgen_35924d824e2f533e ("
  , "  ARGV_t arg1"
  , ")"
  , "{"
  , "  return argvFree(arg1);"
  , "}"
  , "signed int hs_bindgen_cbfb0db02c36575b ("
  , "  ARGI_const_t arg1"
  , ")"
  , "{"
  , "  return argiCount(arg1);"
  , "}"
  , "ARGint_t hs_bindgen_d0af4c5191692b48 ("
  , "  ARGI_const_t arg1"
  , ")"
  , "{"
  , "  return argiData(arg1);"
  , "}"
  , "signed int hs_bindgen_291d969fe4b49945 ("
  , "  ARGV_const_t arg1"
  , ")"
  , "{"
  , "  return argvCount(arg1);"
  , "}"
  , "ARGV_t hs_bindgen_d01d454c69796fd1 ("
  , "  ARGV_t arg1"
  , ")"
  , "{"
  , "  return argvData(arg1);"
  , "}"
  , "signed int hs_bindgen_e382788808b958ff ("
  , "  void const *arg1,"
  , "  void const *arg2"
  , ")"
  , "{"
  , "  return argvCmp(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_71e0648e19494003 ("
  , "  ARGV_t arg1,"
  , "  signed int (*arg2) ("
  , "  void const *arg1,"
  , "  void const *arg2"
  , ")"
  , ")"
  , "{"
  , "  return argvSort(arg1, arg2);"
  , "}"
  , "ARGV_t hs_bindgen_4f0ee964919bbca9 ("
  , "  ARGV_const_t arg1,"
  , "  char const *arg2,"
  , "  signed int (*arg3) ("
  , "  void const *arg1,"
  , "  void const *arg2"
  , ")"
  , ")"
  , "{"
  , "  return argvSearch(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_1493b6a460ec58d9 ("
  , "  ARGI_t *arg1,"
  , "  signed int arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return argiAdd(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_fb2e74455f41e9a4 ("
  , "  ARGV_t *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return argvAdd(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_fe733245f358b9d6 ("
  , "  ARGV_t *arg1,"
  , "  char const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return argvAddN(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_94a3a9356aa548be ("
  , "  ARGV_t *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return argvAddNum(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_c30884d222a0b7f5 ("
  , "  ARGV_t *arg1,"
  , "  ARGV_const_t arg2"
  , ")"
  , "{"
  , "  return argvAppend(arg1, arg2);"
  , "}"
  , "ARGV_t hs_bindgen_8116229e34db565d ("
  , "  char const *arg1,"
  , "  char const *arg2,"
  , "  argvFlags arg3"
  , ")"
  , "{"
  , "  return argvSplitString(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_04010c414530dc2f ("
  , "  ARGV_t *arg1,"
  , "  char const *arg2,"
  , "  char const *arg3"
  , ")"
  , "{"
  , "  return argvSplit(arg1, arg2, arg3);"
  , "}"
  , "char *hs_bindgen_1fd5c96c81968d3f ("
  , "  ARGV_const_t arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return argvJoin(arg1, arg2);"
  , "}"
  ]))

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
foreign import ccall unsafe "hs_bindgen_64dd8a7e068379d1" argvPrint ::
     Ptr.Ptr FC.CChar
     {- ^

        [__@msg@ /(input)/__]: output message prefix (or NULL)

     __C declaration:__ @msg@
     -}
  -> ARGV_const_t
     {- ^

        [__@argv@ /(input)/__]: argv array

     __C declaration:__ @argv@
     -}
  -> Ptr.Ptr HsBindgen.Runtime.Prelude.CFile
     {- ^

        [__@fp@ /(input)/__]: output file handle (NULL uses stderr)

     __C declaration:__ @fp@
     -}
  -> IO ()

{-|

  > rpmargv

  Destroy an argi array.

  [__@argi@ /(input)/__]: argi array

  __returns:__ NULL always

__C declaration:__ @argiFree@

__defined at:__ @rpm\/argv.h:41:8@

__exported by:__ @rpm\/argv.h@
-}
foreign import ccall unsafe "hs_bindgen_407c4d68b5937a17" argiFree ::
     ARGI_t
     {- ^

        [__@argi@ /(input)/__]: argi array

     __C declaration:__ @argi@
     -}
  -> IO ARGI_t

{-|

  > rpmargv

  Create an empty argv array.

  __returns:__ pointer to empty argv

__C declaration:__ @argvNew@

__defined at:__ @rpm\/argv.h:48:8@

__exported by:__ @rpm\/argv.h@
-}
foreign import ccall unsafe "hs_bindgen_ee91d36ccd468b1c" argvNew ::
     IO ARGV_t

{-|

  > rpmargv

  Destroy an argv array.

  [__@argv@ /(input)/__]: argv array

  __returns:__ NULL always

__C declaration:__ @argvFree@

__defined at:__ @rpm\/argv.h:55:8@

__exported by:__ @rpm\/argv.h@
-}
foreign import ccall unsafe "hs_bindgen_35924d824e2f533e" argvFree ::
     ARGV_t
     {- ^

        [__@argv@ /(input)/__]: argv array

     __C declaration:__ @argv@
     -}
  -> IO ARGV_t

{-|

  > rpmargv

  Return no. of elements in argi array.

  [__@argi@ /(input)/__]: argi array

  __returns:__ no. of elements

__C declaration:__ @argiCount@

__defined at:__ @rpm\/argv.h:62:5@

__exported by:__ @rpm\/argv.h@
-}
foreign import ccall unsafe "hs_bindgen_cbfb0db02c36575b" argiCount ::
     ARGI_const_t
     {- ^

        [__@argi@ /(input)/__]: argi array

     __C declaration:__ @argi@
     -}
  -> IO FC.CInt

{-|

  > rpmargv

  Return data from argi array.

  [__@argi@ /(input)/__]: argi array

  __returns:__ argi array data address

__C declaration:__ @argiData@

__defined at:__ @rpm\/argv.h:69:10@

__exported by:__ @rpm\/argv.h@
-}
foreign import ccall unsafe "hs_bindgen_d0af4c5191692b48" argiData ::
     ARGI_const_t
     {- ^

        [__@argi@ /(input)/__]: argi array

     __C declaration:__ @argi@
     -}
  -> IO ARGint_t

{-|

  > rpmargv

  Return no. of elements in argv array.

  [__@argv@ /(input)/__]: argv array

  __returns:__ no. of elements

__C declaration:__ @argvCount@

__defined at:__ @rpm\/argv.h:76:5@

__exported by:__ @rpm\/argv.h@
-}
foreign import ccall unsafe "hs_bindgen_291d969fe4b49945" argvCount ::
     ARGV_const_t
     {- ^

        [__@argv@ /(input)/__]: argv array

     __C declaration:__ @argv@
     -}
  -> IO FC.CInt

{-|

  > rpmargv

  Return data from argv array.

  [__@argv@ /(input)/__]: argv array

  __returns:__ argv array data address

__C declaration:__ @argvData@

__defined at:__ @rpm\/argv.h:83:8@

__exported by:__ @rpm\/argv.h@
-}
foreign import ccall unsafe "hs_bindgen_d01d454c69796fd1" argvData ::
     ARGV_t
     {- ^

        [__@argv@ /(input)/__]: argv array

     __C declaration:__ @argv@
     -}
  -> IO ARGV_t

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
foreign import ccall unsafe "hs_bindgen_e382788808b958ff" argvCmp ::
     Ptr.Ptr Void
     {- ^

        [__@a@ /(input)/__]: 1st instance address

     __C declaration:__ @a@
     -}
  -> Ptr.Ptr Void
     {- ^

        [__@b@ /(input)/__]: 2nd instance address

     __C declaration:__ @b@
     -}
  -> IO FC.CInt

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
foreign import ccall unsafe "hs_bindgen_71e0648e19494003" argvSort ::
     ARGV_t
     {- ^

        [__@argv@ /(input)/__]: argv array

     __C declaration:__ @argv@
     -}
  -> Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.Ptr Void) -> IO FC.CInt)
     {- ^

        [__@compar@ /(input)/__]: strcmp-like comparison function, or NULL for argvCmp()

     __C declaration:__ @compar@
     -}
  -> IO FC.CInt

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
foreign import ccall unsafe "hs_bindgen_4f0ee964919bbca9" argvSearch ::
     ARGV_const_t
     {- ^

        [__@argv@ /(input)/__]: argv array

     __C declaration:__ @argv@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^

        [__@val@ /(input)/__]: string to find

     __C declaration:__ @val@
     -}
  -> Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.Ptr Void) -> IO FC.CInt)
     {- ^

        [__@compar@ /(input)/__]: strcmp-like comparison function, or NULL for argvCmp()

     __C declaration:__ @compar@
     -}
  -> IO ARGV_t

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
foreign import ccall unsafe "hs_bindgen_1493b6a460ec58d9" argiAdd ::
     Ptr.Ptr ARGI_t
     {- ^ __C declaration:__ @argip@
     -}
  -> FC.CInt
     {- ^

        [__@ix@ /(input)/__]: argi array index (or -1 to append)

     __C declaration:__ @ix@
     -}
  -> FC.CInt
     {- ^

        [__@val@ /(input)/__]: int arg to add

     __C declaration:__ @val@
     -}
  -> IO FC.CInt

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
foreign import ccall unsafe "hs_bindgen_fb2e74455f41e9a4" argvAdd ::
     Ptr.Ptr ARGV_t
     {- ^ __C declaration:__ @argvp@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^

        [__@val@ /(input)/__]: string arg to append

     __C declaration:__ @val@
     -}
  -> IO FC.CInt

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
foreign import ccall unsafe "hs_bindgen_fe733245f358b9d6" argvAddN ::
     Ptr.Ptr ARGV_t
     {- ^ __C declaration:__ @argvp@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^

        [__@val@ /(input)/__]: string arg to append

     __C declaration:__ @val@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^

        [__@len@ /(input)/__]: string arg length

     __C declaration:__ @len@
     -}
  -> IO FC.CInt

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
foreign import ccall unsafe "hs_bindgen_94a3a9356aa548be" argvAddNum ::
     Ptr.Ptr ARGV_t
     {- ^ __C declaration:__ @argvp@
     -}
  -> FC.CInt
     {- ^

        [__@val@ /(input)/__]: numeric arg to append

     __C declaration:__ @val@
     -}
  -> IO FC.CInt

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
foreign import ccall unsafe "hs_bindgen_c30884d222a0b7f5" argvAppend ::
     Ptr.Ptr ARGV_t
     {- ^ __C declaration:__ @argvp@
     -}
  -> ARGV_const_t
     {- ^

        [__@av@ /(input)/__]: argv array to append

     __C declaration:__ @av@
     -}
  -> IO FC.CInt

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
foreign import ccall unsafe "hs_bindgen_8116229e34db565d" argvSplitString ::
     Ptr.Ptr FC.CChar
     {- ^

        [__@str@ /(input)/__]: string arg to split

     __C declaration:__ @str@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^

        [__@seps@ /(input)/__]: separator characters

     __C declaration:__ @seps@
     -}
  -> ArgvFlags
     {- ^

        [__@flags@ /(input)/__]: flags to control behavior

     __C declaration:__ @flags@
     -}
  -> IO ARGV_t

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
foreign import ccall unsafe "hs_bindgen_04010c414530dc2f" argvSplit ::
     Ptr.Ptr ARGV_t
     {- ^ __C declaration:__ @argvp@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^

        [__@str@ /(input)/__]: string arg to split

     __C declaration:__ @str@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^

        [__@seps@ /(input)/__]: separator characters

     __C declaration:__ @seps@
     -}
  -> IO FC.CInt

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
foreign import ccall unsafe "hs_bindgen_1fd5c96c81968d3f" argvJoin ::
     ARGV_const_t
     {- ^ __C declaration:__ @argv@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^

        [__@sep@ /(input)/__]: separator string to use

     __C declaration:__ @sep@
     -}
  -> IO (Ptr.Ptr FC.CChar)
