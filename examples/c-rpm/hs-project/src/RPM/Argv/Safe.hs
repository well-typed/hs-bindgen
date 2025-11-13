{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Argv.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (IO)
import RPM.Argv

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/argv.h>"
  , "void hs_bindgen_02e520945a8c6e95 ("
  , "  char const *arg1,"
  , "  ARGV_const_t arg2,"
  , "  FILE *arg3"
  , ")"
  , "{"
  , "  argvPrint(arg1, arg2, arg3);"
  , "}"
  , "ARGI_t hs_bindgen_23e1a795bb03acb2 ("
  , "  ARGI_t arg1"
  , ")"
  , "{"
  , "  return argiFree(arg1);"
  , "}"
  , "ARGV_t hs_bindgen_d642aedc85922910 (void)"
  , "{"
  , "  return argvNew();"
  , "}"
  , "ARGV_t hs_bindgen_b86ad38979bda1a0 ("
  , "  ARGV_t arg1"
  , ")"
  , "{"
  , "  return argvFree(arg1);"
  , "}"
  , "signed int hs_bindgen_4ab2bca823fb70f1 ("
  , "  ARGI_const_t arg1"
  , ")"
  , "{"
  , "  return argiCount(arg1);"
  , "}"
  , "ARGint_t hs_bindgen_1278ee0f62bff5c8 ("
  , "  ARGI_const_t arg1"
  , ")"
  , "{"
  , "  return argiData(arg1);"
  , "}"
  , "signed int hs_bindgen_9bcc5c599ee473f4 ("
  , "  ARGV_const_t arg1"
  , ")"
  , "{"
  , "  return argvCount(arg1);"
  , "}"
  , "ARGV_t hs_bindgen_9330b1d1eba93e8b ("
  , "  ARGV_t arg1"
  , ")"
  , "{"
  , "  return argvData(arg1);"
  , "}"
  , "signed int hs_bindgen_ce0a24d35dd70c9c ("
  , "  void const *arg1,"
  , "  void const *arg2"
  , ")"
  , "{"
  , "  return argvCmp(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_3523f4c682cb649e ("
  , "  ARGV_t arg1,"
  , "  signed int (*arg2) ("
  , "  void const *arg1,"
  , "  void const *arg2"
  , ")"
  , ")"
  , "{"
  , "  return argvSort(arg1, arg2);"
  , "}"
  , "ARGV_t hs_bindgen_d56f141799d52b31 ("
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
  , "signed int hs_bindgen_77cb2086cb700231 ("
  , "  ARGI_t *arg1,"
  , "  signed int arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return argiAdd(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_d57fa87f315d977b ("
  , "  ARGV_t *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return argvAdd(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_c097782169fd09e1 ("
  , "  ARGV_t *arg1,"
  , "  char const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return argvAddN(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_372fd478e98362d7 ("
  , "  ARGV_t *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return argvAddNum(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_2f63e17c5b22d540 ("
  , "  ARGV_t *arg1,"
  , "  ARGV_const_t arg2"
  , ")"
  , "{"
  , "  return argvAppend(arg1, arg2);"
  , "}"
  , "ARGV_t hs_bindgen_fe5f7c8e9719789f ("
  , "  char const *arg1,"
  , "  char const *arg2,"
  , "  argvFlags arg3"
  , ")"
  , "{"
  , "  return argvSplitString(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_0be92efc80ced3a0 ("
  , "  ARGV_t *arg1,"
  , "  char const *arg2,"
  , "  char const *arg3"
  , ")"
  , "{"
  , "  return argvSplit(arg1, arg2, arg3);"
  , "}"
  , "char *hs_bindgen_9b9d41b8df381330 ("
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
foreign import ccall safe "hs_bindgen_02e520945a8c6e95" argvPrint ::
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
foreign import ccall safe "hs_bindgen_23e1a795bb03acb2" argiFree ::
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
foreign import ccall safe "hs_bindgen_d642aedc85922910" argvNew ::
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
foreign import ccall safe "hs_bindgen_b86ad38979bda1a0" argvFree ::
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
foreign import ccall safe "hs_bindgen_4ab2bca823fb70f1" argiCount ::
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
foreign import ccall safe "hs_bindgen_1278ee0f62bff5c8" argiData ::
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
foreign import ccall safe "hs_bindgen_9bcc5c599ee473f4" argvCount ::
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
foreign import ccall safe "hs_bindgen_9330b1d1eba93e8b" argvData ::
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
foreign import ccall safe "hs_bindgen_ce0a24d35dd70c9c" argvCmp ::
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
foreign import ccall safe "hs_bindgen_3523f4c682cb649e" argvSort ::
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
foreign import ccall safe "hs_bindgen_d56f141799d52b31" argvSearch ::
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
foreign import ccall safe "hs_bindgen_77cb2086cb700231" argiAdd ::
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
foreign import ccall safe "hs_bindgen_d57fa87f315d977b" argvAdd ::
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
foreign import ccall safe "hs_bindgen_c097782169fd09e1" argvAddN ::
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
foreign import ccall safe "hs_bindgen_372fd478e98362d7" argvAddNum ::
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
foreign import ccall safe "hs_bindgen_2f63e17c5b22d540" argvAppend ::
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
foreign import ccall safe "hs_bindgen_fe5f7c8e9719789f" argvSplitString ::
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
foreign import ccall safe "hs_bindgen_0be92efc80ced3a0" argvSplit ::
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
foreign import ccall safe "hs_bindgen_9b9d41b8df381330" argvJoin ::
     ARGV_const_t
     {- ^ __C declaration:__ @argv@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^

        [__@sep@ /(input)/__]: separator string to use

     __C declaration:__ @sep@
     -}
  -> IO (Ptr.Ptr FC.CChar)
