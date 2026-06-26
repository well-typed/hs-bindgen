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
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Internal.Prelude.CompatHasField as RIP.CompatHasField

{-| Auxiliary type used by 'Toggle'

    __C declaration:__ @Toggle@

    __defined at:__ @edge-cases\/iterator.h 3:16@

    __exported by:__ @edge-cases\/iterator.h@
-}
newtype Toggle_Aux = Toggle_Aux
  { unwrapToggle_Aux :: IO RIP.CBool
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

-- __unique:__ @toToggle_Aux@
foreign import ccall safe "wrapper" hs_bindgen_eca2bca8e63194be_base ::
     IO RIP.Word8
  -> IO (RIP.FunPtr (IO RIP.Word8))

-- __unique:__ @toToggle_Aux@
hs_bindgen_eca2bca8e63194be ::
     Toggle_Aux
  -> IO (RIP.FunPtr Toggle_Aux)
hs_bindgen_eca2bca8e63194be =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_eca2bca8e63194be_base (RIP.toFFIType fun0))

-- __unique:__ @fromToggle_Aux@
foreign import ccall safe "dynamic" hs_bindgen_703fc4bdc168721d_base ::
     RIP.FunPtr (IO RIP.Word8)
  -> IO RIP.Word8

-- __unique:__ @fromToggle_Aux@
hs_bindgen_703fc4bdc168721d ::
     RIP.FunPtr Toggle_Aux
  -> Toggle_Aux
hs_bindgen_703fc4bdc168721d =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_703fc4bdc168721d_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr Toggle_Aux where

  toFunPtr = hs_bindgen_eca2bca8e63194be

instance RIP.FromFunPtr Toggle_Aux where

  fromFunPtr = hs_bindgen_703fc4bdc168721d

instance ( ty ~ IO RIP.CBool
         ) => RIP.CompatHasField.HasField "unwrapToggle_Aux" Toggle_Aux ty where

  hasField =
    \x0 ->
      ( \y1 -> Toggle_Aux {unwrapToggle_Aux = y1}
      , RIP.getField @"unwrapToggle_Aux" x0
      )

instance ( ty ~ IO RIP.CBool
         ) => RIP.HasField "unwrapToggle_Aux" (RIP.Ptr Toggle_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapToggle_Aux")

instance HasCField.HasCField Toggle_Aux "unwrapToggle_Aux" where

  type CFieldType Toggle_Aux "unwrapToggle_Aux" =
    IO RIP.CBool

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @Toggle@

    __defined at:__ @edge-cases\/iterator.h 3:16@

    __exported by:__ @edge-cases\/iterator.h@
-}
newtype Toggle = Toggle
  { unwrapToggle :: Block.Block Toggle_Aux
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

instance ( ty ~ Block.Block Toggle_Aux
         ) => RIP.CompatHasField.HasField "unwrapToggle" Toggle ty where

  hasField =
    \x0 ->
      (\y1 ->
         Toggle {unwrapToggle = y1}, RIP.getField @"unwrapToggle" x0)

instance ( ty ~ Block.Block Toggle_Aux
         ) => RIP.HasField "unwrapToggle" (RIP.Ptr Toggle) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapToggle")

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
  { unwrapCounter_Aux :: IO RIP.CInt
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

-- __unique:__ @toCounter_Aux@
foreign import ccall safe "wrapper" hs_bindgen_2202848aad97fe0a_base ::
     IO RIP.Int32
  -> IO (RIP.FunPtr (IO RIP.Int32))

-- __unique:__ @toCounter_Aux@
hs_bindgen_2202848aad97fe0a ::
     Counter_Aux
  -> IO (RIP.FunPtr Counter_Aux)
hs_bindgen_2202848aad97fe0a =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_2202848aad97fe0a_base (RIP.toFFIType fun0))

-- __unique:__ @fromCounter_Aux@
foreign import ccall safe "dynamic" hs_bindgen_73304cb84e9a2f8f_base ::
     RIP.FunPtr (IO RIP.Int32)
  -> IO RIP.Int32

-- __unique:__ @fromCounter_Aux@
hs_bindgen_73304cb84e9a2f8f ::
     RIP.FunPtr Counter_Aux
  -> Counter_Aux
hs_bindgen_73304cb84e9a2f8f =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_73304cb84e9a2f8f_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr Counter_Aux where

  toFunPtr = hs_bindgen_2202848aad97fe0a

instance RIP.FromFunPtr Counter_Aux where

  fromFunPtr = hs_bindgen_73304cb84e9a2f8f

instance ( ty ~ IO RIP.CInt
         ) => RIP.CompatHasField.HasField "unwrapCounter_Aux" Counter_Aux ty where

  hasField =
    \x0 ->
      ( \y1 -> Counter_Aux {unwrapCounter_Aux = y1}
      , RIP.getField @"unwrapCounter_Aux" x0
      )

instance ( ty ~ IO RIP.CInt
         ) => RIP.HasField "unwrapCounter_Aux" (RIP.Ptr Counter_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapCounter_Aux")

instance HasCField.HasCField Counter_Aux "unwrapCounter_Aux" where

  type CFieldType Counter_Aux "unwrapCounter_Aux" =
    IO RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @Counter@

    __defined at:__ @edge-cases\/iterator.h 10:14@

    __exported by:__ @edge-cases\/iterator.h@
-}
newtype Counter = Counter
  { unwrapCounter :: Block.Block Counter_Aux
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

instance ( ty ~ Block.Block Counter_Aux
         ) => RIP.CompatHasField.HasField "unwrapCounter" Counter ty where

  hasField =
    \x0 ->
      (\y1 ->
         Counter {unwrapCounter = y1}, RIP.getField @"unwrapCounter" x0)

instance ( ty ~ Block.Block Counter_Aux
         ) => RIP.HasField "unwrapCounter" (RIP.Ptr Counter) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapCounter")

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
  { unwrapVarCounter_Aux :: RIP.CInt -> IO RIP.CInt
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

-- __unique:__ @toVarCounter_Aux@
foreign import ccall safe "wrapper" hs_bindgen_42a7337570f8b0d0_base ::
     (RIP.Int32 -> IO RIP.Int32)
  -> IO (RIP.FunPtr (RIP.Int32 -> IO RIP.Int32))

-- __unique:__ @toVarCounter_Aux@
hs_bindgen_42a7337570f8b0d0 ::
     VarCounter_Aux
  -> IO (RIP.FunPtr VarCounter_Aux)
hs_bindgen_42a7337570f8b0d0 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_42a7337570f8b0d0_base (RIP.toFFIType fun0))

-- __unique:__ @fromVarCounter_Aux@
foreign import ccall safe "dynamic" hs_bindgen_43d902480175fccf_base ::
     RIP.FunPtr (RIP.Int32 -> IO RIP.Int32)
  -> RIP.Int32 -> IO RIP.Int32

-- __unique:__ @fromVarCounter_Aux@
hs_bindgen_43d902480175fccf ::
     RIP.FunPtr VarCounter_Aux
  -> VarCounter_Aux
hs_bindgen_43d902480175fccf =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_43d902480175fccf_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr VarCounter_Aux where

  toFunPtr = hs_bindgen_42a7337570f8b0d0

instance RIP.FromFunPtr VarCounter_Aux where

  fromFunPtr = hs_bindgen_43d902480175fccf

instance ( ty ~ (RIP.CInt -> IO RIP.CInt)
         ) => RIP.CompatHasField.HasField "unwrapVarCounter_Aux" VarCounter_Aux ty where

  hasField =
    \x0 ->
      ( \y1 -> VarCounter_Aux {unwrapVarCounter_Aux = y1}
      , RIP.getField @"unwrapVarCounter_Aux" x0
      )

instance ( ty ~ (RIP.CInt -> IO RIP.CInt)
         ) => RIP.HasField "unwrapVarCounter_Aux" (RIP.Ptr VarCounter_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapVarCounter_Aux")

instance HasCField.HasCField VarCounter_Aux "unwrapVarCounter_Aux" where

  type CFieldType VarCounter_Aux "unwrapVarCounter_Aux" =
    RIP.CInt -> IO RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @VarCounter@

    __defined at:__ @edge-cases\/iterator.h 17:14@

    __exported by:__ @edge-cases\/iterator.h@
-}
newtype VarCounter = VarCounter
  { unwrapVarCounter :: Block.Block VarCounter_Aux
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

instance ( ty ~ Block.Block VarCounter_Aux
         ) => RIP.CompatHasField.HasField "unwrapVarCounter" VarCounter ty where

  hasField =
    \x0 ->
      ( \y1 -> VarCounter {unwrapVarCounter = y1}
      , RIP.getField @"unwrapVarCounter" x0
      )

instance ( ty ~ Block.Block VarCounter_Aux
         ) => RIP.HasField "unwrapVarCounter" (RIP.Ptr VarCounter) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapVarCounter")

instance HasCField.HasCField VarCounter "unwrapVarCounter" where

  type CFieldType VarCounter "unwrapVarCounter" =
    Block.Block VarCounter_Aux

  offset# = \_ -> \_ -> 0
