{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.IncompleteArray
import Prelude (Eq, Show)

{-| __C declaration:__ @triplet@

    __defined at:__ @manual\/arrays.h:32:13@

    __exported by:__ @manual\/arrays.h@
-}
newtype Triplet = Triplet
  { un_Triplet :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @matrix@

    __defined at:__ @manual\/arrays.h:34:17@

    __exported by:__ @manual\/arrays.h@
-}
newtype Matrix = Matrix
  { un_Matrix :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) Triplet
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| A typedef representing a an array of unknown size, where each element is a pointer to an array of known size 3, where each element is an int.

__C declaration:__ @triplet_ptrs@

__defined at:__ @manual\/arrays.h:44:15@

__exported by:__ @manual\/arrays.h@
-}
newtype Triplet_ptrs = Triplet_ptrs
  { un_Triplet_ptrs :: HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
  }
  deriving stock (Eq, Show)
