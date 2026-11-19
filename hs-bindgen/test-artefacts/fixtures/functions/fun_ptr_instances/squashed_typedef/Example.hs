{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.T(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Eq, IO, Int, Show, fmap, pure, type (~))

{-| __C declaration:__ @struct S@

    __defined at:__ @functions\/fun_ptr_instances\/squashed_typedef.h 3:8@

    __exported by:__ @functions\/fun_ptr_instances\/squashed_typedef.h@
-}
data T = T
  { t_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @functions\/fun_ptr_instances\/squashed_typedef.h 3:16@

         __exported by:__ @functions\/fun_ptr_instances\/squashed_typedef.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize T where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw T where

  readRaw =
    \ptr0 ->
          pure T
      <*> HasCField.readRaw (BG.Proxy @"t_x") ptr0

instance Marshal.WriteRaw T where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T t_x2 ->
            HasCField.writeRaw (BG.Proxy @"t_x") ptr0 t_x2

deriving via Marshal.EquivStorable T instance BG.Storable T

deriving via Struct.IsStructViaReadRaw T instance Struct.IsStruct T

{-| __C declaration:__ @x@

    __defined at:__ @functions\/fun_ptr_instances\/squashed_typedef.h 3:16@

    __exported by:__ @functions\/fun_ptr_instances\/squashed_typedef.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "t_x" T ty where

  hasField =
    \x0 -> (\y1 -> T {t_x = y1}, BG.getField @"t_x" x0)

instance (ty ~ BG.CInt) => BG.HasField "t_x" (BG.Ptr T) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t_x")

instance HasCField.HasCField T "t_x" where

  type CFieldType T "t_x" = BG.CInt

  offset# = \_ -> \_ -> 0

-- __unique:__ @instance ToFunPtr (BG.Ptr T -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_cd9938955d8254da_base ::
     (BG.Ptr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr T -> IO ())@
hs_bindgen_cd9938955d8254da ::
     (BG.Ptr T -> IO ())
  -> IO (BG.FunPtr (BG.Ptr T -> IO ()))
hs_bindgen_cd9938955d8254da =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_cd9938955d8254da_base (\x1 ->
                                                            fun0 (BG.fromFFIType x1)))

-- __unique:__ @instance FromFunPtr (BG.Ptr T -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_a30df0ed4fa7cc2a_base ::
     BG.FunPtr (BG.Ptr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr T -> IO ())@
hs_bindgen_a30df0ed4fa7cc2a ::
     BG.FunPtr (BG.Ptr T -> IO ())
  -> BG.Ptr T -> IO ()
hs_bindgen_a30df0ed4fa7cc2a =
  \funPtr0 ->
    \x1 ->
      hs_bindgen_a30df0ed4fa7cc2a_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)

instance BG.ToFunPtr (BG.Ptr T -> IO ()) where

  toFunPtr = hs_bindgen_cd9938955d8254da

instance BG.FromFunPtr (BG.Ptr T -> IO ()) where

  fromFunPtr = hs_bindgen_a30df0ed4fa7cc2a
