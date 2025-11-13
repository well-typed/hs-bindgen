{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Header.Global where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/header.h>"
  , "/* get_rpm_header_magic_ptr */"
  , "__attribute__ ((const))"
  , "unsigned char const (*hs_bindgen_454e35e15c0df8bd (void))[8]"
  , "{"
  , "  return &rpm_header_magic;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_454e35e15c0df8bd" hs_bindgen_454e35e15c0df8bd ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 8) FC.CUChar))

{-# NOINLINE rpm_header_magic_ptr #-}

{-|

  > header

  Header magic value

__C declaration:__ @rpm_header_magic@

__defined at:__ @rpm\/header.h:27:28@

__exported by:__ @rpm\/header.h@
-}
rpm_header_magic_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 8) FC.CUChar)
rpm_header_magic_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_454e35e15c0df8bd

{-# NOINLINE rpm_header_magic #-}

rpm_header_magic :: (HsBindgen.Runtime.ConstantArray.ConstantArray 8) FC.CUChar
rpm_header_magic =
  GHC.IO.Unsafe.unsafePerformIO (F.peek rpm_header_magic_ptr)
