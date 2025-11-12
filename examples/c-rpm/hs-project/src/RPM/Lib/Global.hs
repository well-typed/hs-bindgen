{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Lib.Global where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)
import RPM.Lib

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmlib.h>"
  , "/* get_rpmGlobalMacroContext_ptr */"
  , "__attribute__ ((const))"
  , "struct rpmMacroContext_s **hs_bindgen_b68b4195b597a76e (void)"
  , "{"
  , "  return &rpmGlobalMacroContext;"
  , "}"
  , "/* get_rpmCLIMacroContext_ptr */"
  , "__attribute__ ((const))"
  , "struct rpmMacroContext_s **hs_bindgen_c226717c49113ee7 (void)"
  , "{"
  , "  return &rpmCLIMacroContext;"
  , "}"
  , "/* get_RPMVERSION_ptr */"
  , "__attribute__ ((const))"
  , "char const *const *hs_bindgen_afcb954eac7c43ac (void)"
  , "{"
  , "  return &RPMVERSION;"
  , "}"
  , "/* get_rpmNAME_ptr */"
  , "__attribute__ ((const))"
  , "char const *const *hs_bindgen_0b40107705188369 (void)"
  , "{"
  , "  return &rpmNAME;"
  , "}"
  , "/* get_rpmEVR_ptr */"
  , "__attribute__ ((const))"
  , "char const *const *hs_bindgen_036fd9a91e7086f7 (void)"
  , "{"
  , "  return &rpmEVR;"
  , "}"
  , "/* get_rpmFLAGS_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_c64c59781664906b (void)"
  , "{"
  , "  return &rpmFLAGS;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_b68b4195b597a76e" hs_bindgen_b68b4195b597a76e ::
     IO (Ptr.Ptr (Ptr.Ptr RpmMacroContext_s))

{-# NOINLINE rpmGlobalMacroContext_ptr #-}

{-| __C declaration:__ @rpmGlobalMacroContext@

    __defined at:__ @rpm\/rpmlib.h:25:35@

    __exported by:__ @rpm\/rpmlib.h@
-}
rpmGlobalMacroContext_ptr :: Ptr.Ptr (Ptr.Ptr RpmMacroContext_s)
rpmGlobalMacroContext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b68b4195b597a76e

foreign import ccall unsafe "hs_bindgen_c226717c49113ee7" hs_bindgen_c226717c49113ee7 ::
     IO (Ptr.Ptr (Ptr.Ptr RpmMacroContext_s))

{-# NOINLINE rpmCLIMacroContext_ptr #-}

{-| __C declaration:__ @rpmCLIMacroContext@

    __defined at:__ @rpm\/rpmlib.h:27:35@

    __exported by:__ @rpm\/rpmlib.h@
-}
rpmCLIMacroContext_ptr :: Ptr.Ptr (Ptr.Ptr RpmMacroContext_s)
rpmCLIMacroContext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c226717c49113ee7

foreign import ccall unsafe "hs_bindgen_afcb954eac7c43ac" hs_bindgen_afcb954eac7c43ac ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CChar))

{-# NOINLINE rPMVERSION_ptr #-}

{-| __C declaration:__ @RPMVERSION@

    __defined at:__ @rpm\/rpmlib.h:29:27@

    __exported by:__ @rpm\/rpmlib.h@
-}
rPMVERSION_ptr :: Ptr.Ptr (Ptr.Ptr FC.CChar)
rPMVERSION_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_afcb954eac7c43ac

{-# NOINLINE rPMVERSION #-}

rPMVERSION :: Ptr.Ptr FC.CChar
rPMVERSION =
  GHC.IO.Unsafe.unsafePerformIO (F.peek rPMVERSION_ptr)

foreign import ccall unsafe "hs_bindgen_0b40107705188369" hs_bindgen_0b40107705188369 ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CChar))

{-# NOINLINE rpmNAME_ptr #-}

{-| __C declaration:__ @rpmNAME@

    __defined at:__ @rpm\/rpmlib.h:31:27@

    __exported by:__ @rpm\/rpmlib.h@
-}
rpmNAME_ptr :: Ptr.Ptr (Ptr.Ptr FC.CChar)
rpmNAME_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0b40107705188369

{-# NOINLINE rpmNAME #-}

rpmNAME :: Ptr.Ptr FC.CChar
rpmNAME =
  GHC.IO.Unsafe.unsafePerformIO (F.peek rpmNAME_ptr)

foreign import ccall unsafe "hs_bindgen_036fd9a91e7086f7" hs_bindgen_036fd9a91e7086f7 ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CChar))

{-# NOINLINE rpmEVR_ptr #-}

{-| __C declaration:__ @rpmEVR@

    __defined at:__ @rpm\/rpmlib.h:33:27@

    __exported by:__ @rpm\/rpmlib.h@
-}
rpmEVR_ptr :: Ptr.Ptr (Ptr.Ptr FC.CChar)
rpmEVR_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_036fd9a91e7086f7

{-# NOINLINE rpmEVR #-}

rpmEVR :: Ptr.Ptr FC.CChar
rpmEVR =
  GHC.IO.Unsafe.unsafePerformIO (F.peek rpmEVR_ptr)

foreign import ccall unsafe "hs_bindgen_c64c59781664906b" hs_bindgen_c64c59781664906b ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE rpmFLAGS_ptr #-}

{-| __C declaration:__ @rpmFLAGS@

    __defined at:__ @rpm\/rpmlib.h:35:18@

    __exported by:__ @rpm\/rpmlib.h@
-}
rpmFLAGS_ptr :: Ptr.Ptr FC.CInt
rpmFLAGS_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c64c59781664906b

{-# NOINLINE rpmFLAGS #-}

rpmFLAGS :: FC.CInt
rpmFLAGS =
  GHC.IO.Unsafe.unsafePerformIO (F.peek rpmFLAGS_ptr)
