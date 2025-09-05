{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as F
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <names.h>\nvoid hs_bindgen_test_names_a08e966b63669ba8 (void) { by(); }\n/* get_by_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_bb58aea59417aa76 (void)) (void) { return &by; } \nvoid hs_bindgen_test_names_946148697abf4575 (void) { forall(); }\n/* get_forall_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_4a4cda3da32bed2b (void)) (void) { return &forall; } \nvoid hs_bindgen_test_names_a1ece6a6fa8e9763 (void) { mdo(); }\n/* get_mdo_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_f3f3b62a0c9a709c (void)) (void) { return &mdo; } \nvoid hs_bindgen_test_names_1e48788938305e48 (void) { pattern(); }\n/* get_pattern_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_d6de06049441d0d8 (void)) (void) { return &pattern; } \nvoid hs_bindgen_test_names_9bb16600c50998ed (void) { proc(); }\n/* get_proc_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_e5d464f87b7fe950 (void)) (void) { return &proc; } \nvoid hs_bindgen_test_names_8dfd7fa26e360d8e (void) { rec(); }\n/* get_rec_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_ab3827c2d532dee0 (void)) (void) { return &rec; } \nvoid hs_bindgen_test_names_57c5446244bcece1 (void) { using(); }\n/* get_using_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_c00c761386bb1622 (void)) (void) { return &using; } \nvoid hs_bindgen_test_names_a2a878b7e49c1a71 (void) { anyclass(); }\n/* get_anyclass_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_4a396f85749a2639 (void)) (void) { return &anyclass; } \nvoid hs_bindgen_test_names_2f73a4347f70a468 (void) { capi(); }\n/* get_capi_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_c343892e825ac083 (void)) (void) { return &capi; } \nvoid hs_bindgen_test_names_cb2993ce15743783 (void) { cases(); }\n/* get_cases_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_142ad3bb39343793 (void)) (void) { return &cases; } \nvoid hs_bindgen_test_names_954c564a2bd129a5 (void) { ccall(); }\n/* get_ccall_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_c601c7e6cf3e8093 (void)) (void) { return &ccall; } \nvoid hs_bindgen_test_names_3a96dcbee4c024e0 (void) { dynamic(); }\n/* get_dynamic_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_041d2882439ff8ed (void)) (void) { return &dynamic; } \nvoid hs_bindgen_test_names_2e78d006337ea551 (void) { export(); }\n/* get_export_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_8ab0ab2a4f7b30af (void)) (void) { return &export; } \nvoid hs_bindgen_test_names_10c8e433951b50a8 (void) { family(); }\n/* get_family_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_9c717aeb3b2a21a7 (void)) (void) { return &family; } \nvoid hs_bindgen_test_names_6217d85466fed7df (void) { group(); }\n/* get_group_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_21121f108b34a76f (void)) (void) { return &group; } \nvoid hs_bindgen_test_names_00164685ff44cb75 (void) { interruptible(); }\n/* get_interruptible_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_99f8d337ba1ebac6 (void)) (void) { return &interruptible; } \nvoid hs_bindgen_test_names_c810b13a75ea93bc (void) { javascript(); }\n/* get_javascript_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_4603bd39e6211039 (void)) (void) { return &javascript; } \nvoid hs_bindgen_test_names_41d6d93a2614dff5 (void) { label(); }\n/* get_label_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_a0fbab6bd4868b8a (void)) (void) { return &label; } \nvoid hs_bindgen_test_names_bdfec7f41298a418 (void) { prim(); }\n/* get_prim_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_a21123e386a2c566 (void)) (void) { return &prim; } \nvoid hs_bindgen_test_names_0d10aaa9ecbf0f2f (void) { role(); }\n/* get_role_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_da0964ddbc3ebb7a (void)) (void) { return &role; } \nvoid hs_bindgen_test_names_0ebc368c3818b18b (void) { safe(); }\n/* get_safe_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_b7ed55355edeb6b7 (void)) (void) { return &safe; } \nvoid hs_bindgen_test_names_764bfbe5fbb4fac9 (void) { stdcall(); }\n/* get_stdcall_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_71d621f5185b4a35 (void)) (void) { return &stdcall; } \nvoid hs_bindgen_test_names_6597a6eedc28f577 (void) { stock(); }\n/* get_stock_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_1956b7d5c507eed1 (void)) (void) { return &stock; } \nvoid hs_bindgen_test_names_1aef1bdf2d1ba544 (void) { unsafe(); }\n/* get_unsafe_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_b758151b55abc14a (void)) (void) { return &unsafe; } \nvoid hs_bindgen_test_names_d428a3f008917f79 (void) { via(); }\n/* get_via_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_426a163ac069db6c (void)) (void) { return &via; } \n")

{-| __C declaration:__ @by@

    __defined at:__ @names.h:3:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_a08e966b63669ba8" by'
  :: IO ()

{-| __C declaration:__ @by@

    __defined at:__ @names.h:3:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_bb58aea59417aa76" hs_bindgen_test_names_bb58aea59417aa76
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE by'_ptr #-}

by'_ptr :: F.FunPtr (IO ())
by'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_bb58aea59417aa76

{-| __C declaration:__ @forall@

    __defined at:__ @names.h:4:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_946148697abf4575" forall'
  :: IO ()

{-| __C declaration:__ @forall@

    __defined at:__ @names.h:4:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_4a4cda3da32bed2b" hs_bindgen_test_names_4a4cda3da32bed2b
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE forall'_ptr #-}

forall'_ptr :: F.FunPtr (IO ())
forall'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_4a4cda3da32bed2b

{-| __C declaration:__ @mdo@

    __defined at:__ @names.h:5:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_a1ece6a6fa8e9763" mdo'
  :: IO ()

{-| __C declaration:__ @mdo@

    __defined at:__ @names.h:5:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_f3f3b62a0c9a709c" hs_bindgen_test_names_f3f3b62a0c9a709c
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE mdo'_ptr #-}

mdo'_ptr :: F.FunPtr (IO ())
mdo'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_f3f3b62a0c9a709c

{-| __C declaration:__ @pattern@

    __defined at:__ @names.h:6:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_1e48788938305e48" pattern'
  :: IO ()

{-| __C declaration:__ @pattern@

    __defined at:__ @names.h:6:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_d6de06049441d0d8" hs_bindgen_test_names_d6de06049441d0d8
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE pattern'_ptr #-}

pattern'_ptr :: F.FunPtr (IO ())
pattern'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_d6de06049441d0d8

{-| __C declaration:__ @proc@

    __defined at:__ @names.h:7:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_9bb16600c50998ed" proc'
  :: IO ()

{-| __C declaration:__ @proc@

    __defined at:__ @names.h:7:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_e5d464f87b7fe950" hs_bindgen_test_names_e5d464f87b7fe950
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE proc'_ptr #-}

proc'_ptr :: F.FunPtr (IO ())
proc'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_e5d464f87b7fe950

{-| __C declaration:__ @rec@

    __defined at:__ @names.h:8:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_8dfd7fa26e360d8e" rec'
  :: IO ()

{-| __C declaration:__ @rec@

    __defined at:__ @names.h:8:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_ab3827c2d532dee0" hs_bindgen_test_names_ab3827c2d532dee0
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE rec'_ptr #-}

rec'_ptr :: F.FunPtr (IO ())
rec'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_ab3827c2d532dee0

{-| __C declaration:__ @using@

    __defined at:__ @names.h:9:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_57c5446244bcece1" using'
  :: IO ()

{-| __C declaration:__ @using@

    __defined at:__ @names.h:9:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_c00c761386bb1622" hs_bindgen_test_names_c00c761386bb1622
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE using'_ptr #-}

using'_ptr :: F.FunPtr (IO ())
using'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_c00c761386bb1622

{-| __C declaration:__ @anyclass@

    __defined at:__ @names.h:12:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_a2a878b7e49c1a71" anyclass
  :: IO ()

{-| __C declaration:__ @anyclass@

    __defined at:__ @names.h:12:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_4a396f85749a2639" hs_bindgen_test_names_4a396f85749a2639
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE anyclass_ptr #-}

anyclass_ptr :: F.FunPtr (IO ())
anyclass_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_4a396f85749a2639

{-| __C declaration:__ @capi@

    __defined at:__ @names.h:13:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_2f73a4347f70a468" capi
  :: IO ()

{-| __C declaration:__ @capi@

    __defined at:__ @names.h:13:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_c343892e825ac083" hs_bindgen_test_names_c343892e825ac083
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE capi_ptr #-}

capi_ptr :: F.FunPtr (IO ())
capi_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_c343892e825ac083

{-| __C declaration:__ @cases@

    __defined at:__ @names.h:14:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_cb2993ce15743783" cases
  :: IO ()

{-| __C declaration:__ @cases@

    __defined at:__ @names.h:14:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_142ad3bb39343793" hs_bindgen_test_names_142ad3bb39343793
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE cases_ptr #-}

cases_ptr :: F.FunPtr (IO ())
cases_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_142ad3bb39343793

{-| __C declaration:__ @ccall@

    __defined at:__ @names.h:15:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_954c564a2bd129a5" ccall
  :: IO ()

{-| __C declaration:__ @ccall@

    __defined at:__ @names.h:15:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_c601c7e6cf3e8093" hs_bindgen_test_names_c601c7e6cf3e8093
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE ccall_ptr #-}

ccall_ptr :: F.FunPtr (IO ())
ccall_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_c601c7e6cf3e8093

{-| __C declaration:__ @dynamic@

    __defined at:__ @names.h:16:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_3a96dcbee4c024e0" dynamic
  :: IO ()

{-| __C declaration:__ @dynamic@

    __defined at:__ @names.h:16:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_041d2882439ff8ed" hs_bindgen_test_names_041d2882439ff8ed
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE dynamic_ptr #-}

dynamic_ptr :: F.FunPtr (IO ())
dynamic_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_041d2882439ff8ed

{-| __C declaration:__ @export@

    __defined at:__ @names.h:17:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_2e78d006337ea551" export
  :: IO ()

{-| __C declaration:__ @export@

    __defined at:__ @names.h:17:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_8ab0ab2a4f7b30af" hs_bindgen_test_names_8ab0ab2a4f7b30af
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE export_ptr #-}

export_ptr :: F.FunPtr (IO ())
export_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_8ab0ab2a4f7b30af

{-| __C declaration:__ @family@

    __defined at:__ @names.h:18:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_10c8e433951b50a8" family
  :: IO ()

{-| __C declaration:__ @family@

    __defined at:__ @names.h:18:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_9c717aeb3b2a21a7" hs_bindgen_test_names_9c717aeb3b2a21a7
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE family_ptr #-}

family_ptr :: F.FunPtr (IO ())
family_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_9c717aeb3b2a21a7

{-| __C declaration:__ @group@

    __defined at:__ @names.h:19:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_6217d85466fed7df" group
  :: IO ()

{-| __C declaration:__ @group@

    __defined at:__ @names.h:19:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_21121f108b34a76f" hs_bindgen_test_names_21121f108b34a76f
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE group_ptr #-}

group_ptr :: F.FunPtr (IO ())
group_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_21121f108b34a76f

{-| __C declaration:__ @interruptible@

    __defined at:__ @names.h:20:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_00164685ff44cb75" interruptible
  :: IO ()

{-| __C declaration:__ @interruptible@

    __defined at:__ @names.h:20:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_99f8d337ba1ebac6" hs_bindgen_test_names_99f8d337ba1ebac6
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE interruptible_ptr #-}

interruptible_ptr :: F.FunPtr (IO ())
interruptible_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_99f8d337ba1ebac6

{-| __C declaration:__ @javascript@

    __defined at:__ @names.h:21:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_c810b13a75ea93bc" javascript
  :: IO ()

{-| __C declaration:__ @javascript@

    __defined at:__ @names.h:21:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_4603bd39e6211039" hs_bindgen_test_names_4603bd39e6211039
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE javascript_ptr #-}

javascript_ptr :: F.FunPtr (IO ())
javascript_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_4603bd39e6211039

{-| __C declaration:__ @label@

    __defined at:__ @names.h:22:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_41d6d93a2614dff5" label
  :: IO ()

{-| __C declaration:__ @label@

    __defined at:__ @names.h:22:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_a0fbab6bd4868b8a" hs_bindgen_test_names_a0fbab6bd4868b8a
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE label_ptr #-}

label_ptr :: F.FunPtr (IO ())
label_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_a0fbab6bd4868b8a

{-| __C declaration:__ @prim@

    __defined at:__ @names.h:23:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_bdfec7f41298a418" prim
  :: IO ()

{-| __C declaration:__ @prim@

    __defined at:__ @names.h:23:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_a21123e386a2c566" hs_bindgen_test_names_a21123e386a2c566
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE prim_ptr #-}

prim_ptr :: F.FunPtr (IO ())
prim_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_a21123e386a2c566

{-| __C declaration:__ @role@

    __defined at:__ @names.h:24:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_0d10aaa9ecbf0f2f" role
  :: IO ()

{-| __C declaration:__ @role@

    __defined at:__ @names.h:24:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_da0964ddbc3ebb7a" hs_bindgen_test_names_da0964ddbc3ebb7a
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE role_ptr #-}

role_ptr :: F.FunPtr (IO ())
role_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_da0964ddbc3ebb7a

{-| __C declaration:__ @safe@

    __defined at:__ @names.h:25:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_0ebc368c3818b18b" safe
  :: IO ()

{-| __C declaration:__ @safe@

    __defined at:__ @names.h:25:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_b7ed55355edeb6b7" hs_bindgen_test_names_b7ed55355edeb6b7
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE safe_ptr #-}

safe_ptr :: F.FunPtr (IO ())
safe_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_b7ed55355edeb6b7

{-| __C declaration:__ @stdcall@

    __defined at:__ @names.h:26:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_764bfbe5fbb4fac9" stdcall
  :: IO ()

{-| __C declaration:__ @stdcall@

    __defined at:__ @names.h:26:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_71d621f5185b4a35" hs_bindgen_test_names_71d621f5185b4a35
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE stdcall_ptr #-}

stdcall_ptr :: F.FunPtr (IO ())
stdcall_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_71d621f5185b4a35

{-| __C declaration:__ @stock@

    __defined at:__ @names.h:27:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_6597a6eedc28f577" stock
  :: IO ()

{-| __C declaration:__ @stock@

    __defined at:__ @names.h:27:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_1956b7d5c507eed1" hs_bindgen_test_names_1956b7d5c507eed1
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE stock_ptr #-}

stock_ptr :: F.FunPtr (IO ())
stock_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_1956b7d5c507eed1

{-| __C declaration:__ @unsafe@

    __defined at:__ @names.h:28:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_1aef1bdf2d1ba544" unsafe
  :: IO ()

{-| __C declaration:__ @unsafe@

    __defined at:__ @names.h:28:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_b758151b55abc14a" hs_bindgen_test_names_b758151b55abc14a
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE unsafe_ptr #-}

unsafe_ptr :: F.FunPtr (IO ())
unsafe_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_b758151b55abc14a

{-| __C declaration:__ @via@

    __defined at:__ @names.h:29:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_d428a3f008917f79" via
  :: IO ()

{-| __C declaration:__ @via@

    __defined at:__ @names.h:29:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_426a163ac069db6c" hs_bindgen_test_names_426a163ac069db6c
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE via_ptr #-}

via_ptr :: F.FunPtr (IO ())
via_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_426a163ac069db6c
