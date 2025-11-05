{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Data.Bits (FiniteBits)
import Prelude (Bounded, Enum, Eq, IO, Integral, Num, Ord, Read, Real, Show)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <declarations_required_for_scoping.h>"
  , "void hs_bindgen_test_declarations_required_for_scop_fcce70013c76ce8b ("
  , "  A arg1"
  , ")"
  , "{"
  , "  f(arg1);"
  , "}"
  , "/* get_f_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_declarations_required_for_scop_c34fd33eedc1490d (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &f;"
  , "}"
  ]))

{-| __C declaration:__ @A@

    __defined at:__ @declarations_required_for_scoping.h:5:9@

    __exported by:__ @declarations_required_for_scoping.h@
-}
newtype A = A
  { un_A :: HsBindgen.Runtime.Prelude.CSize
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @f@

    __defined at:__ @declarations_required_for_scoping.h:7:6@

    __exported by:__ @declarations_required_for_scoping.h@
-}
foreign import ccall safe "hs_bindgen_test_declarations_required_for_scop_fcce70013c76ce8b" f ::
     A
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

foreign import ccall unsafe "hs_bindgen_test_declarations_required_for_scop_c34fd33eedc1490d" hs_bindgen_test_declarations_required_for_scop_c34fd33eedc1490d ::
     IO (Ptr.FunPtr (A -> IO ()))

{-# NOINLINE f_ptr #-}

{-| __C declaration:__ @f@

    __defined at:__ @declarations_required_for_scoping.h:7:6@

    __exported by:__ @declarations_required_for_scoping.h@
-}
f_ptr :: Ptr.FunPtr (A -> IO ())
f_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_declarations_required_for_scop_c34fd33eedc1490d
