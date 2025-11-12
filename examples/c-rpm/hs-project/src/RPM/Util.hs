{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RPM.Util where

import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (Eq, IO, Ord, Show)

{-| Auxiliary type used by 'RpmMemFailFunc'

__defined at:__ @rpm\/rpmutil.h:152:18@

__exported by:__ @rpm\/rpmutil.h@
-}
newtype RpmMemFailFunc_Deref = RpmMemFailFunc_Deref
  { un_RpmMemFailFunc_Deref :: HsBindgen.Runtime.Prelude.CSize -> (Ptr.Ptr Void) -> IO (Ptr.Ptr Void)
  }

foreign import ccall safe "wrapper" toRpmMemFailFunc_Deref ::
     RpmMemFailFunc_Deref
  -> IO (Ptr.FunPtr RpmMemFailFunc_Deref)

foreign import ccall safe "dynamic" fromRpmMemFailFunc_Deref ::
     Ptr.FunPtr RpmMemFailFunc_Deref
  -> RpmMemFailFunc_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr RpmMemFailFunc_Deref where

  toFunPtr = toRpmMemFailFunc_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr RpmMemFailFunc_Deref where

  fromFunPtr = fromRpmMemFailFunc_Deref

{-|

  > rpmutil

  Memory allocation failure callback prototype. When registered through rpmSetMemFail(), this gets called if memory allocation through rmalloc() and friends fails. If the application can somehow recover memory here, it can return a newly allocated memory block of requested size, otherwise it must return NULL after performing it's own shutdown deeds or terminate itself.

  [__@size@ /(input)/__]: Size of allocation request in bytes

  [__@data@ /(input)/__]: User data (or NULL)

  __returns:__ Allocated memory block of requested size or NULL

__C declaration:__ @rpmMemFailFunc@

__defined at:__ @rpm\/rpmutil.h:152:18@

__exported by:__ @rpm\/rpmutil.h@
-}
newtype RpmMemFailFunc = RpmMemFailFunc
  { un_RpmMemFailFunc :: Ptr.FunPtr RpmMemFailFunc_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)
