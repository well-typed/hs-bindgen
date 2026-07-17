{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Toggle_Aux(..)
    , Example.Toggle(..)
    , Example.Counter_Aux(..)
    , Example.Counter(..)
    , Example.VarCounter_Aux(..)
    , Example.VarCounter(..)
    )
  where

import qualified HsBindgen.Runtime.Block as Block
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| Auxiliary type used by 'Toggle'

    __C declaration:__ @Toggle@

    __defined at:__ @edge-cases\/iterator.h 3:16@

    __exported by:__ @edge-cases\/iterator.h@
-}
newtype Toggle_Aux = Toggle_Aux
  { unwrapToggle_Aux :: IO BG.CBool
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toToggle_Aux@
foreign import ccall safe "wrapper" hs_bindgen_eca2bca8e63194be_base ::
     IO BG.Word8
  -> IO (BG.FunPtr (IO BG.Word8))

-- __unique:__ @toToggle_Aux@
hs_bindgen_eca2bca8e63194be ::
     Toggle_Aux
  -> IO (BG.FunPtr Toggle_Aux)
hs_bindgen_eca2bca8e63194be =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_eca2bca8e63194be_base (BG.toFFIType fun0))

-- __unique:__ @fromToggle_Aux@
foreign import ccall safe "dynamic" hs_bindgen_703fc4bdc168721d_base ::
     BG.FunPtr (IO BG.Word8)
  -> IO BG.Word8

-- __unique:__ @fromToggle_Aux@
hs_bindgen_703fc4bdc168721d ::
     BG.FunPtr Toggle_Aux
  -> Toggle_Aux
hs_bindgen_703fc4bdc168721d =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_703fc4bdc168721d_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr Toggle_Aux where

  toFunPtr = hs_bindgen_eca2bca8e63194be

instance BG.FromFunPtr Toggle_Aux where

  fromFunPtr = hs_bindgen_703fc4bdc168721d

instance ( ty ~ IO BG.CBool
         ) => BG.CompatHasField.HasField "unwrapToggle_Aux" Toggle_Aux ty where

  hasField =
    \x0 ->
      ( \y1 -> Toggle_Aux {unwrapToggle_Aux = y1}
      , BG.getField @"unwrapToggle_Aux" x0
      )

instance ( ty ~ IO BG.CBool
         ) => BG.HasField "unwrapToggle_Aux" (BG.Ptr Toggle_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapToggle_Aux")

instance HasCField.HasCField Toggle_Aux "unwrapToggle_Aux" where

  type CFieldType Toggle_Aux "unwrapToggle_Aux" =
    IO BG.CBool

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @Toggle@

    __defined at:__ @edge-cases\/iterator.h 3:16@

    __exported by:__ @edge-cases\/iterator.h@
-}
newtype Toggle = Toggle
  { unwrapToggle :: Block.Block Toggle_Aux
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

instance ( ty ~ Block.Block Toggle_Aux
         ) => BG.CompatHasField.HasField "unwrapToggle" Toggle ty where

  hasField =
    \x0 ->
      (\y1 ->
         Toggle {unwrapToggle = y1}, BG.getField @"unwrapToggle" x0)

instance ( ty ~ Block.Block Toggle_Aux
         ) => BG.HasField "unwrapToggle" (BG.Ptr Toggle) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapToggle")

instance HasCField.HasCField Toggle "unwrapToggle" where

  type CFieldType Toggle "unwrapToggle" =
    Block.Block Toggle_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Counter'

    __C declaration:__ @Counter@

    __defined at:__ @edge-cases\/iterator.h 10:14@

    __exported by:__ @edge-cases\/iterator.h@
-}
newtype Counter_Aux = Counter_Aux
  { unwrapCounter_Aux :: IO BG.CInt
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toCounter_Aux@
foreign import ccall safe "wrapper" hs_bindgen_2202848aad97fe0a_base ::
     IO BG.Int32
  -> IO (BG.FunPtr (IO BG.Int32))

-- __unique:__ @toCounter_Aux@
hs_bindgen_2202848aad97fe0a ::
     Counter_Aux
  -> IO (BG.FunPtr Counter_Aux)
hs_bindgen_2202848aad97fe0a =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_2202848aad97fe0a_base (BG.toFFIType fun0))

-- __unique:__ @fromCounter_Aux@
foreign import ccall safe "dynamic" hs_bindgen_73304cb84e9a2f8f_base ::
     BG.FunPtr (IO BG.Int32)
  -> IO BG.Int32

-- __unique:__ @fromCounter_Aux@
hs_bindgen_73304cb84e9a2f8f ::
     BG.FunPtr Counter_Aux
  -> Counter_Aux
hs_bindgen_73304cb84e9a2f8f =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_73304cb84e9a2f8f_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr Counter_Aux where

  toFunPtr = hs_bindgen_2202848aad97fe0a

instance BG.FromFunPtr Counter_Aux where

  fromFunPtr = hs_bindgen_73304cb84e9a2f8f

instance ( ty ~ IO BG.CInt
         ) => BG.CompatHasField.HasField "unwrapCounter_Aux" Counter_Aux ty where

  hasField =
    \x0 ->
      ( \y1 -> Counter_Aux {unwrapCounter_Aux = y1}
      , BG.getField @"unwrapCounter_Aux" x0
      )

instance ( ty ~ IO BG.CInt
         ) => BG.HasField "unwrapCounter_Aux" (BG.Ptr Counter_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapCounter_Aux")

instance HasCField.HasCField Counter_Aux "unwrapCounter_Aux" where

  type CFieldType Counter_Aux "unwrapCounter_Aux" =
    IO BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @Counter@

    __defined at:__ @edge-cases\/iterator.h 10:14@

    __exported by:__ @edge-cases\/iterator.h@
-}
newtype Counter = Counter
  { unwrapCounter :: Block.Block Counter_Aux
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

instance ( ty ~ Block.Block Counter_Aux
         ) => BG.CompatHasField.HasField "unwrapCounter" Counter ty where

  hasField =
    \x0 ->
      (\y1 ->
         Counter {unwrapCounter = y1}, BG.getField @"unwrapCounter" x0)

instance ( ty ~ Block.Block Counter_Aux
         ) => BG.HasField "unwrapCounter" (BG.Ptr Counter) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapCounter")

instance HasCField.HasCField Counter "unwrapCounter" where

  type CFieldType Counter "unwrapCounter" =
    Block.Block Counter_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'VarCounter'

    __C declaration:__ @VarCounter@

    __defined at:__ @edge-cases\/iterator.h 17:14@

    __exported by:__ @edge-cases\/iterator.h@
-}
newtype VarCounter_Aux = VarCounter_Aux
  { unwrapVarCounter_Aux :: BG.CInt -> IO BG.CInt
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toVarCounter_Aux@
foreign import ccall safe "wrapper" hs_bindgen_42a7337570f8b0d0_base ::
     (BG.Int32 -> IO BG.Int32)
  -> IO (BG.FunPtr (BG.Int32 -> IO BG.Int32))

-- __unique:__ @toVarCounter_Aux@
hs_bindgen_42a7337570f8b0d0 ::
     VarCounter_Aux
  -> IO (BG.FunPtr VarCounter_Aux)
hs_bindgen_42a7337570f8b0d0 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_42a7337570f8b0d0_base (BG.toFFIType fun0))

-- __unique:__ @fromVarCounter_Aux@
foreign import ccall safe "dynamic" hs_bindgen_43d902480175fccf_base ::
     BG.FunPtr (BG.Int32 -> IO BG.Int32)
  -> BG.Int32 -> IO BG.Int32

-- __unique:__ @fromVarCounter_Aux@
hs_bindgen_43d902480175fccf ::
     BG.FunPtr VarCounter_Aux
  -> VarCounter_Aux
hs_bindgen_43d902480175fccf =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_43d902480175fccf_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr VarCounter_Aux where

  toFunPtr = hs_bindgen_42a7337570f8b0d0

instance BG.FromFunPtr VarCounter_Aux where

  fromFunPtr = hs_bindgen_43d902480175fccf

instance ( ty ~ (BG.CInt -> IO BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapVarCounter_Aux" VarCounter_Aux ty where

  hasField =
    \x0 ->
      ( \y1 -> VarCounter_Aux {unwrapVarCounter_Aux = y1}
      , BG.getField @"unwrapVarCounter_Aux" x0
      )

instance ( ty ~ (BG.CInt -> IO BG.CInt)
         ) => BG.HasField "unwrapVarCounter_Aux" (BG.Ptr VarCounter_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapVarCounter_Aux")

instance HasCField.HasCField VarCounter_Aux "unwrapVarCounter_Aux" where

  type CFieldType VarCounter_Aux "unwrapVarCounter_Aux" =
    BG.CInt -> IO BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @VarCounter@

    __defined at:__ @edge-cases\/iterator.h 17:14@

    __exported by:__ @edge-cases\/iterator.h@
-}
newtype VarCounter = VarCounter
  { unwrapVarCounter :: Block.Block VarCounter_Aux
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

instance ( ty ~ Block.Block VarCounter_Aux
         ) => BG.CompatHasField.HasField "unwrapVarCounter" VarCounter ty where

  hasField =
    \x0 ->
      ( \y1 -> VarCounter {unwrapVarCounter = y1}
      , BG.getField @"unwrapVarCounter" x0
      )

instance ( ty ~ Block.Block VarCounter_Aux
         ) => BG.HasField "unwrapVarCounter" (BG.Ptr VarCounter) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapVarCounter")

instance HasCField.HasCField VarCounter "unwrapVarCounter" where

  type CFieldType VarCounter "unwrapVarCounter" =
    Block.Block VarCounter_Aux

  offset# = \_ -> \_ -> 0
