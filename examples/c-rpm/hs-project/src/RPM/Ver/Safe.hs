{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Ver.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified RPM.Ds
import qualified RPM.Types
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmver.h>"
  , "signed int hs_bindgen_b374a8e30bf66d42 ("
  , "  char const *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return rpmvercmp(arg1, arg2);"
  , "}"
  , "rpmver hs_bindgen_de854ba59cf5f392 ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return rpmverParse(arg1);"
  , "}"
  , "rpmver hs_bindgen_7f9f50de2bc0b43a ("
  , "  char const *arg1,"
  , "  char const *arg2,"
  , "  char const *arg3"
  , ")"
  , "{"
  , "  return rpmverNew(arg1, arg2, arg3);"
  , "}"
  , "rpmver hs_bindgen_78f5dd2da3df3e5a ("
  , "  rpmver arg1"
  , ")"
  , "{"
  , "  return rpmverFree(arg1);"
  , "}"
  , "uint32_t hs_bindgen_15c4e42000ce71af ("
  , "  rpmver arg1"
  , ")"
  , "{"
  , "  return rpmverEVal(arg1);"
  , "}"
  , "char const *hs_bindgen_36e8f8d983740465 ("
  , "  rpmver arg1"
  , ")"
  , "{"
  , "  return rpmverE(arg1);"
  , "}"
  , "char const *hs_bindgen_e4a4875b8ab81774 ("
  , "  rpmver arg1"
  , ")"
  , "{"
  , "  return rpmverV(arg1);"
  , "}"
  , "char const *hs_bindgen_726e6f72216c66bb ("
  , "  rpmver arg1"
  , ")"
  , "{"
  , "  return rpmverR(arg1);"
  , "}"
  , "char *hs_bindgen_1b31c664ebf67898 ("
  , "  rpmver arg1"
  , ")"
  , "{"
  , "  return rpmverEVR(arg1);"
  , "}"
  , "signed int hs_bindgen_5ea5db3831c86c34 ("
  , "  rpmver arg1,"
  , "  rpmver arg2"
  , ")"
  , "{"
  , "  return rpmverCmp(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_c123d4277869411c ("
  , "  rpmver arg1,"
  , "  rpmsenseFlags arg2,"
  , "  rpmver arg3,"
  , "  rpmsenseFlags arg4"
  , ")"
  , "{"
  , "  return rpmverOverlap(arg1, arg2, arg3, arg4);"
  , "}"
  ]))

{-|

  > rpmver

  Segmented string compare for version or release strings.

  [__@a@ /(input)/__]: 1st string

  [__@b@ /(input)/__]: 2nd string

  __returns:__ +1 if a is "newer", 0 if equal, -1 if b is "newer"

__C declaration:__ @rpmvercmp@

__defined at:__ @rpm\/rpmver.h:18:5@

__exported by:__ @rpm\/rpmver.h@
-}
foreign import ccall safe "hs_bindgen_b374a8e30bf66d42" rpmvercmp ::
     Ptr.Ptr FC.CChar
     {- ^

        [__@a@ /(input)/__]: 1st string

     __C declaration:__ @a@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^

        [__@b@ /(input)/__]: 2nd string

     __C declaration:__ @b@
     -}
  -> IO FC.CInt

{-|

  > rpmver

  Parse rpm version handle from evr string

  [__@evr@ /(input)/__]: [epoch:]version[-release] string

  __returns:__ rpm version, NULL on invalid evr

__C declaration:__ @rpmverParse@

__defined at:__ @rpm\/rpmver.h:26:8@

__exported by:__ @rpm\/rpmver.h@
-}
foreign import ccall safe "hs_bindgen_de854ba59cf5f392" rpmverParse ::
     Ptr.Ptr FC.CChar
     {- ^

        [__@evr@ /(input)/__]: [epoch:]version[-release] string

     __C declaration:__ @evr@
     -}
  -> IO RPM.Types.Rpmver

{-|

  > rpmver

  Create new rpm version handle from e, v, r components

  [__@e@ /(input)/__]: epoch (or NULL)

  [__@v@ /(input)/__]: version

  [__@r@ /(input)/__]: release (or NULL)

  __returns:__ rpm version, NULL on invalid

__C declaration:__ @rpmverNew@

__defined at:__ @rpm\/rpmver.h:36:8@

__exported by:__ @rpm\/rpmver.h@
-}
foreign import ccall safe "hs_bindgen_7f9f50de2bc0b43a" rpmverNew ::
     Ptr.Ptr FC.CChar
     {- ^

        [__@e@ /(input)/__]: epoch (or NULL)

     __C declaration:__ @e@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^

        [__@v@ /(input)/__]: version

     __C declaration:__ @v@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^

        [__@r@ /(input)/__]: release (or NULL)

     __C declaration:__ @r@
     -}
  -> IO RPM.Types.Rpmver

{-|

  > rpmver

  Free rpm version handle

  [__@rv@ /(input)/__]: rpm version handle

  __returns:__ NULL always

__C declaration:__ @rpmverFree@

__defined at:__ @rpm\/rpmver.h:44:8@

__exported by:__ @rpm\/rpmver.h@
-}
foreign import ccall safe "hs_bindgen_78f5dd2da3df3e5a" rpmverFree ::
     RPM.Types.Rpmver
     {- ^

        [__@rv@ /(input)/__]: rpm version handle

     __C declaration:__ @rv@
     -}
  -> IO RPM.Types.Rpmver

{-|

  > rpmver

  [__@rv@ /(input)/__]: rpm version handle

  __returns:__ numerical value of epoch

__C declaration:__ @rpmverEVal@

__defined at:__ @rpm\/rpmver.h:50:10@

__exported by:__ @rpm\/rpmver.h@
-}
foreign import ccall safe "hs_bindgen_15c4e42000ce71af" rpmverEVal ::
     RPM.Types.Rpmver
     {- ^

        [__@rv@ /(input)/__]: rpm version handle

     __C declaration:__ @rv@
     -}
  -> IO HsBindgen.Runtime.Prelude.Word32

{-|

  > rpmver

  [__@rv@ /(input)/__]: rpm version handle

  __returns:__ epoch portion

__C declaration:__ @rpmverE@

__defined at:__ @rpm\/rpmver.h:56:13@

__exported by:__ @rpm\/rpmver.h@
-}
foreign import ccall safe "hs_bindgen_36e8f8d983740465" rpmverE ::
     RPM.Types.Rpmver
     {- ^

        [__@rv@ /(input)/__]: rpm version handle

     __C declaration:__ @rv@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-|

  > rpmver

  [__@rv@ /(input)/__]: rpm version handle

  __returns:__ version portion

__C declaration:__ @rpmverV@

__defined at:__ @rpm\/rpmver.h:62:13@

__exported by:__ @rpm\/rpmver.h@
-}
foreign import ccall safe "hs_bindgen_e4a4875b8ab81774" rpmverV ::
     RPM.Types.Rpmver
     {- ^

        [__@rv@ /(input)/__]: rpm version handle

     __C declaration:__ @rv@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-|

  > rpmver

  [__@rv@ /(input)/__]: rpm version handle

  __returns:__ release portion

__C declaration:__ @rpmverR@

__defined at:__ @rpm\/rpmver.h:68:13@

__exported by:__ @rpm\/rpmver.h@
-}
foreign import ccall safe "hs_bindgen_726e6f72216c66bb" rpmverR ::
     RPM.Types.Rpmver
     {- ^

        [__@rv@ /(input)/__]: rpm version handle

     __C declaration:__ @rv@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-|

  > rpmver

  [__@rv@ /(input)/__]: rpm version handle

  __returns:__ formatted [E:]V[-R] string (malloced)

__C declaration:__ @rpmverEVR@

__defined at:__ @rpm\/rpmver.h:74:7@

__exported by:__ @rpm\/rpmver.h@
-}
foreign import ccall safe "hs_bindgen_1b31c664ebf67898" rpmverEVR ::
     RPM.Types.Rpmver
     {- ^

        [__@rv@ /(input)/__]: rpm version handle

     __C declaration:__ @rv@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-|

  > rpmver

  Compare two rpm version handles

  [__@v1@ /(input)/__]: 1st version handle

  [__@v2@ /(input)/__]: 2nd version handle

  __returns:__ 0 if equal, -1 if v1 smaller, 1 if greater, than v2

__C declaration:__ @rpmverCmp@

__defined at:__ @rpm\/rpmver.h:83:5@

__exported by:__ @rpm\/rpmver.h@
-}
foreign import ccall safe "hs_bindgen_5ea5db3831c86c34" rpmverCmp ::
     RPM.Types.Rpmver
     {- ^

        [__@v1@ /(input)/__]: 1st version handle

     __C declaration:__ @v1@
     -}
  -> RPM.Types.Rpmver
     {- ^

        [__@v2@ /(input)/__]: 2nd version handle

     __C declaration:__ @v2@
     -}
  -> IO FC.CInt

{-|

  > rpmver

  Determine whether two versioned ranges overlap.

  [__@v1@ /(input)/__]: 1st version

  [__@f1@ /(input)/__]: 1st sense flags

  [__@v2@ /(input)/__]: 2nd version

  [__@f2@ /(input)/__]: 2nd sense flags

  __returns:__ 1 if ranges overlap, 0 otherwise

__C declaration:__ @rpmverOverlap@

__defined at:__ @rpm\/rpmver.h:93:5@

__exported by:__ @rpm\/rpmver.h@
-}
foreign import ccall safe "hs_bindgen_c123d4277869411c" rpmverOverlap ::
     RPM.Types.Rpmver
     {- ^

        [__@v1@ /(input)/__]: 1st version

     __C declaration:__ @v1@
     -}
  -> RPM.Ds.RpmsenseFlags
     {- ^

        [__@f1@ /(input)/__]: 1st sense flags

     __C declaration:__ @f1@
     -}
  -> RPM.Types.Rpmver
     {- ^

        [__@v2@ /(input)/__]: 2nd version

     __C declaration:__ @v2@
     -}
  -> RPM.Ds.RpmsenseFlags
     {- ^

        [__@f2@ /(input)/__]: 2nd sense flags

     __C declaration:__ @f2@
     -}
  -> IO FC.CInt
