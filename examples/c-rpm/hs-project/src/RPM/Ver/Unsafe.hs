{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Ver.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified RPM.Ds
import qualified RPM.Types
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmver.h>"
  , "signed int hs_bindgen_1ccda481c00f6949 ("
  , "  char const *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return rpmvercmp(arg1, arg2);"
  , "}"
  , "rpmver hs_bindgen_bcd5ad9024f0eb6c ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return rpmverParse(arg1);"
  , "}"
  , "rpmver hs_bindgen_2ab62f79547e95b9 ("
  , "  char const *arg1,"
  , "  char const *arg2,"
  , "  char const *arg3"
  , ")"
  , "{"
  , "  return rpmverNew(arg1, arg2, arg3);"
  , "}"
  , "rpmver hs_bindgen_981472102715fdba ("
  , "  rpmver arg1"
  , ")"
  , "{"
  , "  return rpmverFree(arg1);"
  , "}"
  , "uint32_t hs_bindgen_f341c360cad2f13e ("
  , "  rpmver arg1"
  , ")"
  , "{"
  , "  return rpmverEVal(arg1);"
  , "}"
  , "char const *hs_bindgen_0a3e21b6eaea072c ("
  , "  rpmver arg1"
  , ")"
  , "{"
  , "  return rpmverE(arg1);"
  , "}"
  , "char const *hs_bindgen_fe7cc9413e27298b ("
  , "  rpmver arg1"
  , ")"
  , "{"
  , "  return rpmverV(arg1);"
  , "}"
  , "char const *hs_bindgen_3bcb404f361085e0 ("
  , "  rpmver arg1"
  , ")"
  , "{"
  , "  return rpmverR(arg1);"
  , "}"
  , "char *hs_bindgen_d597cff5cdee524b ("
  , "  rpmver arg1"
  , ")"
  , "{"
  , "  return rpmverEVR(arg1);"
  , "}"
  , "signed int hs_bindgen_601de8dbfdecd1a3 ("
  , "  rpmver arg1,"
  , "  rpmver arg2"
  , ")"
  , "{"
  , "  return rpmverCmp(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_d13fc55c5e9a8973 ("
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
foreign import ccall unsafe "hs_bindgen_1ccda481c00f6949" rpmvercmp ::
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
foreign import ccall unsafe "hs_bindgen_bcd5ad9024f0eb6c" rpmverParse ::
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
foreign import ccall unsafe "hs_bindgen_2ab62f79547e95b9" rpmverNew ::
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
foreign import ccall unsafe "hs_bindgen_981472102715fdba" rpmverFree ::
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
foreign import ccall unsafe "hs_bindgen_f341c360cad2f13e" rpmverEVal ::
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
foreign import ccall unsafe "hs_bindgen_0a3e21b6eaea072c" rpmverE ::
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
foreign import ccall unsafe "hs_bindgen_fe7cc9413e27298b" rpmverV ::
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
foreign import ccall unsafe "hs_bindgen_3bcb404f361085e0" rpmverR ::
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
foreign import ccall unsafe "hs_bindgen_d597cff5cdee524b" rpmverEVR ::
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
foreign import ccall unsafe "hs_bindgen_601de8dbfdecd1a3" rpmverCmp ::
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
foreign import ccall unsafe "hs_bindgen_d13fc55c5e9a8973" rpmverOverlap ::
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
