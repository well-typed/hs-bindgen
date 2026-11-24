{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module AllOutputKinds.Acyclic_callback
    ( AllOutputKinds.Acyclic_callback.Callback_code(..)
    , pattern AllOutputKinds.Acyclic_callback.CALLBACK_CODE_A
    , pattern AllOutputKinds.Acyclic_callback.CALLBACK_CODE_B
    , AllOutputKinds.Acyclic_callback.Acyclic_callback_Aux(..)
    , AllOutputKinds.Acyclic_callback.Acyclic_callback(..)
    )
  where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Eq, IO, Int, Ord, Read, Show, fmap, pure, type (~))

{-| __C declaration:__ @enum callback_code@

    __defined at:__ @all_output_kinds\/acyclic_callback.h 7:6@

    __exported by:__ @all_output_kinds.h@
-}
newtype Callback_code = Callback_code
  { unwrapCallback_code :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize Callback_code where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Callback_code where

  readRaw =
    \ptr0 ->
          pure Callback_code
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Callback_code where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Callback_code unwrapCallback_code2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapCallback_code2

deriving via Marshal.EquivStorable Callback_code instance BG.Storable Callback_code

deriving via BG.CUInt instance BG.Prim Callback_code

instance CEnum.CEnum Callback_code where

  type CEnumZ Callback_code = BG.CUInt

  toCEnum = Callback_code

  fromCEnum = BG.getField @"unwrapCallback_code"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "CALLBACK_CODE_A"), (1, BG.singleton "CALLBACK_CODE_B")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "Callback_code"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Callback_code"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum Callback_code where

  minDeclaredValue = CALLBACK_CODE_A

  maxDeclaredValue = CALLBACK_CODE_B

instance Show Callback_code where

  showsPrec = CEnum.shows

instance Read Callback_code where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "unwrapCallback_code" Callback_code ty where

  hasField =
    \x0 ->
      ( \y1 -> Callback_code {unwrapCallback_code = y1}
      , BG.getField @"unwrapCallback_code" x0
      )

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapCallback_code" (BG.Ptr Callback_code) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapCallback_code")

instance HasCField.HasCField Callback_code "unwrapCallback_code" where

  type CFieldType Callback_code "unwrapCallback_code" =
    BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @CALLBACK_CODE_A@

    __defined at:__ @all_output_kinds\/acyclic_callback.h 7:22@

    __exported by:__ @all_output_kinds.h@
-}
pattern CALLBACK_CODE_A :: Callback_code
pattern CALLBACK_CODE_A = Callback_code 0

{-| __C declaration:__ @CALLBACK_CODE_B@

    __defined at:__ @all_output_kinds\/acyclic_callback.h 7:39@

    __exported by:__ @all_output_kinds.h@
-}
pattern CALLBACK_CODE_B :: Callback_code
pattern CALLBACK_CODE_B = Callback_code 1

{-| Auxiliary type used by 'Acyclic_callback'

    __C declaration:__ @acyclic_callback@

    __defined at:__ @all_output_kinds\/acyclic_callback.h 9:16@

    __exported by:__ @all_output_kinds.h@
-}
newtype Acyclic_callback_Aux = Acyclic_callback_Aux
  { unwrapAcyclic_callback_Aux :: Callback_code -> BG.CInt -> IO ()
  }
  deriving stock (BG.Generic)

instance ( ty ~ (Callback_code -> BG.CInt -> IO ())
         ) => BG.CompatHasField.HasField "unwrapAcyclic_callback_Aux" Acyclic_callback_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Acyclic_callback_Aux {unwrapAcyclic_callback_Aux = y1}
      , BG.getField @"unwrapAcyclic_callback_Aux" x0
      )

instance ( ty ~ (Callback_code -> BG.CInt -> IO ())
         ) => BG.HasField "unwrapAcyclic_callback_Aux" (BG.Ptr Acyclic_callback_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapAcyclic_callback_Aux")

instance HasCField.HasCField Acyclic_callback_Aux "unwrapAcyclic_callback_Aux" where

  type CFieldType Acyclic_callback_Aux "unwrapAcyclic_callback_Aux" =
    Callback_code -> BG.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @acyclic_callback@

    __defined at:__ @all_output_kinds\/acyclic_callback.h 9:16@

    __exported by:__ @all_output_kinds.h@
-}
newtype Acyclic_callback = Acyclic_callback
  { unwrapAcyclic_callback :: BG.FunPtr Acyclic_callback_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr Acyclic_callback_Aux
         ) => BG.CompatHasField.HasField "unwrapAcyclic_callback" Acyclic_callback ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Acyclic_callback {unwrapAcyclic_callback = y1}
      , BG.getField @"unwrapAcyclic_callback" x0
      )

instance ( ty ~ BG.FunPtr Acyclic_callback_Aux
         ) => BG.HasField "unwrapAcyclic_callback" (BG.Ptr Acyclic_callback) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapAcyclic_callback")

instance HasCField.HasCField Acyclic_callback "unwrapAcyclic_callback" where

  type CFieldType Acyclic_callback "unwrapAcyclic_callback" =
    BG.FunPtr Acyclic_callback_Aux

  offset# = \_ -> \_ -> 0

-- __unique:__ @toAcyclic_callback_Aux@
foreign import ccall safe "wrapper" hs_bindgen_857e504970476dc4_base ::
     (BG.CUInt -> BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.CUInt -> BG.CInt -> IO ()))

-- __unique:__ @toAcyclic_callback_Aux@
hs_bindgen_857e504970476dc4 ::
     Acyclic_callback_Aux
  -> IO (BG.FunPtr Acyclic_callback_Aux)
hs_bindgen_857e504970476dc4 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_857e504970476dc4_base (\x1 ->
                                                            \x2 ->
                                                              BG.getField @"unwrapAcyclic_callback_Aux" fun0 (BG.fromFFIType x1) (BG.fromFFIType x2)))

-- __unique:__ @fromAcyclic_callback_Aux@
foreign import ccall safe "dynamic" hs_bindgen_01102796b37891a5_base ::
     BG.FunPtr (BG.CUInt -> BG.CInt -> IO ())
  -> BG.CUInt -> BG.CInt -> IO ()

-- __unique:__ @fromAcyclic_callback_Aux@
hs_bindgen_01102796b37891a5 ::
     BG.FunPtr Acyclic_callback_Aux
  -> Acyclic_callback_Aux
hs_bindgen_01102796b37891a5 =
  \funPtr0 ->
    Acyclic_callback_Aux (\x1 ->
                            \x2 ->
                              hs_bindgen_01102796b37891a5_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2))

instance BG.ToFunPtr Acyclic_callback_Aux where

  toFunPtr = hs_bindgen_857e504970476dc4

instance BG.FromFunPtr Acyclic_callback_Aux where

  fromFunPtr = hs_bindgen_01102796b37891a5
