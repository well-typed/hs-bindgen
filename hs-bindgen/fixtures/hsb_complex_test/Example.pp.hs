{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Complex
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __defined at:__ @hsb_complex_test.h:24:9@

    __exported by:__ @hsb_complex_test.h@
-}
data Complex_object_t = Complex_object_t
  { complex_object_t_velocity :: Data.Complex.Complex FC.CFloat
    {- ^ __C declaration:__ @velocity@

         __defined at:__ @hsb_complex_test.h:25:20@

         __exported by:__ @hsb_complex_test.h@
    -}
  , complex_object_t_position :: Data.Complex.Complex FC.CDouble
    {- ^ __C declaration:__ @position@

         __defined at:__ @hsb_complex_test.h:26:20@

         __exported by:__ @hsb_complex_test.h@
    -}
  , complex_object_t_id :: FC.CInt
    {- ^ __C declaration:__ @id@

         __defined at:__ @hsb_complex_test.h:27:9@

         __exported by:__ @hsb_complex_test.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Complex_object_t where

  sizeOf = \_ -> (32 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Complex_object_t
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"complex_object_t_velocity") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"complex_object_t_position") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"complex_object_t_id") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Complex_object_t
            complex_object_t_velocity2
            complex_object_t_position3
            complex_object_t_id4 ->
                 HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"complex_object_t_velocity") ptr0 complex_object_t_velocity2
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"complex_object_t_position") ptr0 complex_object_t_position3
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"complex_object_t_id") ptr0 complex_object_t_id4

instance HsBindgen.Runtime.HasCField.HasCField Complex_object_t "complex_object_t_velocity" where

  type CFieldType Complex_object_t "complex_object_t_velocity" =
    Data.Complex.Complex FC.CFloat

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Complex_object_t) "complex_object_t_velocity")
         ) => GHC.Records.HasField "complex_object_t_velocity" (Ptr.Ptr Complex_object_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"complex_object_t_velocity")

instance HsBindgen.Runtime.HasCField.HasCField Complex_object_t "complex_object_t_position" where

  type CFieldType Complex_object_t "complex_object_t_position" =
    Data.Complex.Complex FC.CDouble

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Complex_object_t) "complex_object_t_position")
         ) => GHC.Records.HasField "complex_object_t_position" (Ptr.Ptr Complex_object_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"complex_object_t_position")

instance HsBindgen.Runtime.HasCField.HasCField Complex_object_t "complex_object_t_id" where

  type CFieldType Complex_object_t "complex_object_t_id" =
    FC.CInt

  offset# = \_ -> \_ -> 24

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Complex_object_t) "complex_object_t_id")
         ) => GHC.Records.HasField "complex_object_t_id" (Ptr.Ptr Complex_object_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"complex_object_t_id")
