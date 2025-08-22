{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-foreign-imports #-}

module Example where

import qualified Foreign as F
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <names.h>\nvoid hs_bindgen_test_names_a08e966b63669ba8 (void) { by(); }\n/* get_by_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_bb58aea59417aa76 (void)) (void) { return &by; } \nvoid hs_bindgen_test_names_946148697abf4575 (void) { forall(); }\n/* get_forall_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_4a4cda3da32bed2b (void)) (void) { return &forall; } \nvoid hs_bindgen_test_names_a1ece6a6fa8e9763 (void) { mdo(); }\n/* get_mdo_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_f3f3b62a0c9a709c (void)) (void) { return &mdo; } \nvoid hs_bindgen_test_names_1e48788938305e48 (void) { pattern(); }\n/* get_pattern_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_d6de06049441d0d8 (void)) (void) { return &pattern; } \nvoid hs_bindgen_test_names_9bb16600c50998ed (void) { proc(); }\n/* get_proc_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_e5d464f87b7fe950 (void)) (void) { return &proc; } \nvoid hs_bindgen_test_names_8dfd7fa26e360d8e (void) { rec(); }\n/* get_rec_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_ab3827c2d532dee0 (void)) (void) { return &rec; } \nvoid hs_bindgen_test_names_57c5446244bcece1 (void) { using(); }\n/* get_using_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_c00c761386bb1622 (void)) (void) { return &using; } \nvoid hs_bindgen_test_names_a2a878b7e49c1a71 (void) { anyclass(); }\n/* get_anyclass_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_4a396f85749a2639 (void)) (void) { return &anyclass; } \nvoid hs_bindgen_test_names_2f73a4347f70a468 (void) { capi(); }\n/* get_capi_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_c343892e825ac083 (void)) (void) { return &capi; } \nvoid hs_bindgen_test_names_cb2993ce15743783 (void) { cases(); }\n/* get_cases_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_142ad3bb39343793 (void)) (void) { return &cases; } \nvoid hs_bindgen_test_names_954c564a2bd129a5 (void) { ccall(); }\n/* get_ccall_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_c601c7e6cf3e8093 (void)) (void) { return &ccall; } \nvoid hs_bindgen_test_names_3a96dcbee4c024e0 (void) { dynamic(); }\n/* get_dynamic_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_041d2882439ff8ed (void)) (void) { return &dynamic; } \nvoid hs_bindgen_test_names_2e78d006337ea551 (void) { export(); }\n/* get_export_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_8ab0ab2a4f7b30af (void)) (void) { return &export; } \nvoid hs_bindgen_test_names_10c8e433951b50a8 (void) { family(); }\n/* get_family_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_9c717aeb3b2a21a7 (void)) (void) { return &family; } \nvoid hs_bindgen_test_names_6217d85466fed7df (void) { group(); }\n/* get_group_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_21121f108b34a76f (void)) (void) { return &group; } \nvoid hs_bindgen_test_names_00164685ff44cb75 (void) { interruptible(); }\n/* get_interruptible_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_99f8d337ba1ebac6 (void)) (void) { return &interruptible; } \nvoid hs_bindgen_test_names_c810b13a75ea93bc (void) { javascript(); }\n/* get_javascript_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_4603bd39e6211039 (void)) (void) { return &javascript; } \nvoid hs_bindgen_test_names_41d6d93a2614dff5 (void) { label(); }\n/* get_label_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_a0fbab6bd4868b8a (void)) (void) { return &label; } \nvoid hs_bindgen_test_names_bdfec7f41298a418 (void) { prim(); }\n/* get_prim_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_a21123e386a2c566 (void)) (void) { return &prim; } \nvoid hs_bindgen_test_names_0d10aaa9ecbf0f2f (void) { role(); }\n/* get_role_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_da0964ddbc3ebb7a (void)) (void) { return &role; } \nvoid hs_bindgen_test_names_0ebc368c3818b18b (void) { safe(); }\n/* get_safe_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_b7ed55355edeb6b7 (void)) (void) { return &safe; } \nvoid hs_bindgen_test_names_764bfbe5fbb4fac9 (void) { stdcall(); }\n/* get_stdcall_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_71d621f5185b4a35 (void)) (void) { return &stdcall; } \nvoid hs_bindgen_test_names_6597a6eedc28f577 (void) { stock(); }\n/* get_stock_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_1956b7d5c507eed1 (void)) (void) { return &stock; } \nvoid hs_bindgen_test_names_1aef1bdf2d1ba544 (void) { unsafe(); }\n/* get_unsafe_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_b758151b55abc14a (void)) (void) { return &unsafe; } \nvoid hs_bindgen_test_names_d428a3f008917f79 (void) { via(); }\n/* get_via_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_426a163ac069db6c (void)) (void) { return &via; } \n")

foreign import ccall safe "hs_bindgen_test_names_a08e966b63669ba8" by'
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_bb58aea59417aa76" by'_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_946148697abf4575" forall'
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_4a4cda3da32bed2b" forall'_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_a1ece6a6fa8e9763" mdo'
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_f3f3b62a0c9a709c" mdo'_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_1e48788938305e48" pattern'
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_d6de06049441d0d8" pattern'_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_9bb16600c50998ed" proc'
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_e5d464f87b7fe950" proc'_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_8dfd7fa26e360d8e" rec'
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_ab3827c2d532dee0" rec'_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_57c5446244bcece1" using'
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_c00c761386bb1622" using'_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_a2a878b7e49c1a71" anyclass
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_4a396f85749a2639" anyclass_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_2f73a4347f70a468" capi
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_c343892e825ac083" capi_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_cb2993ce15743783" cases
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_142ad3bb39343793" cases_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_954c564a2bd129a5" ccall
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_c601c7e6cf3e8093" ccall_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_3a96dcbee4c024e0" dynamic
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_041d2882439ff8ed" dynamic_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_2e78d006337ea551" export
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_8ab0ab2a4f7b30af" export_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_10c8e433951b50a8" family
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_9c717aeb3b2a21a7" family_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_6217d85466fed7df" group
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_21121f108b34a76f" group_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_00164685ff44cb75" interruptible
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_99f8d337ba1ebac6" interruptible_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_c810b13a75ea93bc" javascript
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_4603bd39e6211039" javascript_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_41d6d93a2614dff5" label
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_a0fbab6bd4868b8a" label_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_bdfec7f41298a418" prim
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_a21123e386a2c566" prim_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_0d10aaa9ecbf0f2f" role
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_da0964ddbc3ebb7a" role_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_0ebc368c3818b18b" safe
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_b7ed55355edeb6b7" safe_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_764bfbe5fbb4fac9" stdcall
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_71d621f5185b4a35" stdcall_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_6597a6eedc28f577" stock
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_1956b7d5c507eed1" stock_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_1aef1bdf2d1ba544" unsafe
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_b758151b55abc14a" unsafe_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_names_d428a3f008917f79" via
  :: IO ()

foreign import ccall safe "hs_bindgen_test_names_426a163ac069db6c" via_ptr
  :: F.FunPtr (IO ())
