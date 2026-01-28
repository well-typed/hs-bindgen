{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.IncompleteArray
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (Eq, Show)

{-| __C declaration:__ @matrix@

    __defined at:__ @arrays\/multi_dim.h 12:13@

    __exported by:__ @arrays\/multi_dim.h@
-}
newtype Matrix = Matrix
  { un_Matrix :: (HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Matrix) "un_Matrix")
         ) => GHC.Records.HasField "un_Matrix" (Ptr.Ptr Matrix) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Matrix")

instance HsBindgen.Runtime.HasCField.HasCField Matrix "un_Matrix" where

  type CFieldType Matrix "un_Matrix" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @triplets@

    __defined at:__ @arrays\/multi_dim.h 17:13@

    __exported by:__ @arrays\/multi_dim.h@
-}
newtype Triplets = Triplets
  { un_Triplets :: HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  }
  deriving stock (Eq, Show)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Triplets) "un_Triplets")
         ) => GHC.Records.HasField "un_Triplets" (Ptr.Ptr Triplets) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Triplets")

instance HsBindgen.Runtime.HasCField.HasCField Triplets "un_Triplets" where

  type CFieldType Triplets "un_Triplets" =
    HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)

  offset# = \_ -> \_ -> 0
