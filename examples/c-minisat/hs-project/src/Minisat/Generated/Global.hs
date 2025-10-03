{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Minisat.Generated.Global where

import Prelude (IO)

import Foreign qualified as F
import GHC.IO.Unsafe qualified
import GHC.Ptr qualified as Ptr

import HsBindgen.Runtime.Prelude qualified

import Minisat.Generated

$(HsBindgen.Runtime.Prelude.addCSource "#include </home/bolt/Desktop/Bolt/UMinho/Profissional/Well-Typed/Projects/hs-bindgen/examples/c-minisat/minisat-c-bindings/minisat.h>\n/* get_minisat_l_True_ptr */ __attribute__ ((const)) minisat_lbool const *hs_bindgen_bfdb0a8babf7f847 (void) { return &minisat_l_True; } \n/* get_minisat_l_False_ptr */ __attribute__ ((const)) minisat_lbool const *hs_bindgen_3215dd3e514471bb (void) { return &minisat_l_False; } \n/* get_minisat_l_Undef_ptr */ __attribute__ ((const)) minisat_lbool const *hs_bindgen_93d5cc79c028e170 (void) { return &minisat_l_Undef; } \n")

foreign import ccall unsafe "hs_bindgen_bfdb0a8babf7f847" hs_bindgen_bfdb0a8babf7f847
  :: IO (Ptr.Ptr Minisat_lbool)

{-# NOINLINE minisat_l_True_ptr #-}

{-| __C declaration:__ @minisat_l_True@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:41:28@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_l_True_ptr :: Ptr.Ptr Minisat_lbool
minisat_l_True_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_bfdb0a8babf7f847

{-# NOINLINE minisat_l_True #-}

minisat_l_True :: Minisat_lbool
minisat_l_True =
  GHC.IO.Unsafe.unsafePerformIO (F.peek minisat_l_True_ptr)

foreign import ccall unsafe "hs_bindgen_3215dd3e514471bb" hs_bindgen_3215dd3e514471bb
  :: IO (Ptr.Ptr Minisat_lbool)

{-# NOINLINE minisat_l_False_ptr #-}

{-| __C declaration:__ @minisat_l_False@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:42:28@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_l_False_ptr :: Ptr.Ptr Minisat_lbool
minisat_l_False_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3215dd3e514471bb

{-# NOINLINE minisat_l_False #-}

minisat_l_False :: Minisat_lbool
minisat_l_False =
  GHC.IO.Unsafe.unsafePerformIO (F.peek minisat_l_False_ptr)

foreign import ccall unsafe "hs_bindgen_93d5cc79c028e170" hs_bindgen_93d5cc79c028e170
  :: IO (Ptr.Ptr Minisat_lbool)

{-# NOINLINE minisat_l_Undef_ptr #-}

{-| __C declaration:__ @minisat_l_Undef@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:43:28@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_l_Undef_ptr :: Ptr.Ptr Minisat_lbool
minisat_l_Undef_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_93d5cc79c028e170

{-# NOINLINE minisat_l_Undef #-}

minisat_l_Undef :: Minisat_lbool
minisat_l_Undef =
  GHC.IO.Unsafe.unsafePerformIO (F.peek minisat_l_Undef_ptr)
