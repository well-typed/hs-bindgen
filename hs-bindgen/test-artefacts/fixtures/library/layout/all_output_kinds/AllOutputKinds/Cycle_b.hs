{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module AllOutputKinds.Cycle_b
    ( AllOutputKinds.Cycle_b.cYCLE_B_LIMIT
    , AllOutputKinds.Cycle_b.Cycle_b_callback_Aux(..)
    , AllOutputKinds.Cycle_b.Cycle_b_callback(..)
    , AllOutputKinds.Cycle_b.Cycle_b(..)
    )
  where

import qualified AllOutputKinds.Nested.By_value
import qualified C.Expr.HostPlatform
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import {-# SOURCE #-} qualified AllOutputKinds.Cycle_a
import Prelude ((<*>), (>>), Eq, IO, Int, Ord, Show, pure, type (~))

{-| __C declaration:__ @macro CYCLE_B_LIMIT@

    __defined at:__ @all_output_kinds\/cycle_b.h 8:9@

    __exported by:__ @all_output_kinds.h@
-}
cYCLE_B_LIMIT :: BG.CInt
cYCLE_B_LIMIT =
  (C.Expr.HostPlatform.*) AllOutputKinds.Cycle_a.cYCLE_A_LIMIT (2 :: BG.CInt)

{-| Auxiliary type used by 'Cycle_b_callback'

    __C declaration:__ @cycle_b_callback@

    __defined at:__ @all_output_kinds\/cycle_b.h 12:16@

    __exported by:__ @all_output_kinds.h@
-}
newtype Cycle_b_callback_Aux = Cycle_b_callback_Aux
  { unwrapCycle_b_callback_Aux :: BG.Ptr Cycle_b -> BG.CFloat -> IO ()
  }
  deriving stock (BG.Generic)

instance ( ty ~ (BG.Ptr Cycle_b -> BG.CFloat -> IO ())
         ) => BG.CompatHasField.HasField "unwrapCycle_b_callback_Aux" Cycle_b_callback_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Cycle_b_callback_Aux {unwrapCycle_b_callback_Aux = y1}
      , BG.getField @"unwrapCycle_b_callback_Aux" x0
      )

instance ( ty ~ (BG.Ptr Cycle_b -> BG.CFloat -> IO ())
         ) => BG.HasField "unwrapCycle_b_callback_Aux" (BG.Ptr Cycle_b_callback_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapCycle_b_callback_Aux")

instance HasCField.HasCField Cycle_b_callback_Aux "unwrapCycle_b_callback_Aux" where

  type CFieldType Cycle_b_callback_Aux "unwrapCycle_b_callback_Aux" =
    BG.Ptr Cycle_b -> BG.CFloat -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @cycle_b_callback@

    __defined at:__ @all_output_kinds\/cycle_b.h 12:16@

    __exported by:__ @all_output_kinds.h@
-}
newtype Cycle_b_callback = Cycle_b_callback
  { unwrapCycle_b_callback :: BG.FunPtr Cycle_b_callback_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr Cycle_b_callback_Aux
         ) => BG.CompatHasField.HasField "unwrapCycle_b_callback" Cycle_b_callback ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Cycle_b_callback {unwrapCycle_b_callback = y1}
      , BG.getField @"unwrapCycle_b_callback" x0
      )

instance ( ty ~ BG.FunPtr Cycle_b_callback_Aux
         ) => BG.HasField "unwrapCycle_b_callback" (BG.Ptr Cycle_b_callback) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapCycle_b_callback")

instance HasCField.HasCField Cycle_b_callback "unwrapCycle_b_callback" where

  type CFieldType Cycle_b_callback "unwrapCycle_b_callback" =
    BG.FunPtr Cycle_b_callback_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct cycle_b@

    __defined at:__ @all_output_kinds\/cycle_b.h 14:8@

    __exported by:__ @all_output_kinds.h@
-}
data Cycle_b = Cycle_b
  { cycle_b_owner :: BG.Ptr AllOutputKinds.Cycle_a.Cycle_a
    {- ^ __C declaration:__ @owner@

         __defined at:__ @all_output_kinds\/cycle_b.h 15:19@

         __exported by:__ @all_output_kinds.h@
    -}
  , cycle_b_next :: BG.Ptr Cycle_b
    {- ^ __C declaration:__ @next@

         __defined at:__ @all_output_kinds\/cycle_b.h 16:19@

         __exported by:__ @all_output_kinds.h@
    -}
  , cycle_b_position :: AllOutputKinds.Nested.By_value.By_value
    {- ^ __C declaration:__ @position@

         __defined at:__ @all_output_kinds\/cycle_b.h 17:19@

         __exported by:__ @all_output_kinds.h@
    -}
  , cycle_b_callback :: Cycle_b_callback
    {- ^ __C declaration:__ @callback@

         __defined at:__ @all_output_kinds\/cycle_b.h 18:20@

         __exported by:__ @all_output_kinds.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Cycle_b where

  staticSizeOf = \_ -> (32 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Cycle_b where

  readRaw =
    \ptr0 ->
          pure Cycle_b
      <*> HasCField.readRaw (BG.Proxy @"cycle_b_owner") ptr0
      <*> HasCField.readRaw (BG.Proxy @"cycle_b_next") ptr0
      <*> HasCField.readRaw (BG.Proxy @"cycle_b_position") ptr0
      <*> HasCField.readRaw (BG.Proxy @"cycle_b_callback") ptr0

instance Marshal.WriteRaw Cycle_b where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Cycle_b cycle_b_owner2 cycle_b_next3 cycle_b_position4 cycle_b_callback5 ->
               HasCField.writeRaw (BG.Proxy @"cycle_b_owner") ptr0 cycle_b_owner2
            >> HasCField.writeRaw (BG.Proxy @"cycle_b_next") ptr0 cycle_b_next3
            >> HasCField.writeRaw (BG.Proxy @"cycle_b_position") ptr0 cycle_b_position4
            >> HasCField.writeRaw (BG.Proxy @"cycle_b_callback") ptr0 cycle_b_callback5

deriving via Marshal.EquivStorable Cycle_b instance BG.Storable Cycle_b

deriving via Struct.IsStructViaReadRaw Cycle_b instance Struct.IsStruct Cycle_b

{-| __C declaration:__ @owner@

    __defined at:__ @all_output_kinds\/cycle_b.h 15:19@

    __exported by:__ @all_output_kinds.h@
-}
instance ( ty ~ BG.Ptr AllOutputKinds.Cycle_a.Cycle_a
         ) => BG.CompatHasField.HasField "cycle_b_owner" Cycle_b ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Cycle_b { cycle_b_owner = y1
                  , cycle_b_next = BG.getField @"cycle_b_next" x0
                  , cycle_b_position = BG.getField @"cycle_b_position" x0
                  , cycle_b_callback = BG.getField @"cycle_b_callback" x0
                  }
      , BG.getField @"cycle_b_owner" x0
      )

instance ( ty ~ BG.Ptr AllOutputKinds.Cycle_a.Cycle_a
         ) => BG.HasField "cycle_b_owner" (BG.Ptr Cycle_b) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"cycle_b_owner")

instance HasCField.HasCField Cycle_b "cycle_b_owner" where

  type CFieldType Cycle_b "cycle_b_owner" =
    BG.Ptr AllOutputKinds.Cycle_a.Cycle_a

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @next@

    __defined at:__ @all_output_kinds\/cycle_b.h 16:19@

    __exported by:__ @all_output_kinds.h@
-}
instance ( ty ~ BG.Ptr Cycle_b
         ) => BG.CompatHasField.HasField "cycle_b_next" Cycle_b ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Cycle_b { cycle_b_next = y1
                  , cycle_b_owner = BG.getField @"cycle_b_owner" x0
                  , cycle_b_position = BG.getField @"cycle_b_position" x0
                  , cycle_b_callback = BG.getField @"cycle_b_callback" x0
                  }
      , BG.getField @"cycle_b_next" x0
      )

instance ( ty ~ BG.Ptr Cycle_b
         ) => BG.HasField "cycle_b_next" (BG.Ptr Cycle_b) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"cycle_b_next")

instance HasCField.HasCField Cycle_b "cycle_b_next" where

  type CFieldType Cycle_b "cycle_b_next" =
    BG.Ptr Cycle_b

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @position@

    __defined at:__ @all_output_kinds\/cycle_b.h 17:19@

    __exported by:__ @all_output_kinds.h@
-}
instance ( ty ~ AllOutputKinds.Nested.By_value.By_value
         ) => BG.CompatHasField.HasField "cycle_b_position" Cycle_b ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Cycle_b { cycle_b_position = y1
                  , cycle_b_owner = BG.getField @"cycle_b_owner" x0
                  , cycle_b_next = BG.getField @"cycle_b_next" x0
                  , cycle_b_callback = BG.getField @"cycle_b_callback" x0
                  }
      , BG.getField @"cycle_b_position" x0
      )

instance ( ty ~ AllOutputKinds.Nested.By_value.By_value
         ) => BG.HasField "cycle_b_position" (BG.Ptr Cycle_b) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"cycle_b_position")

instance HasCField.HasCField Cycle_b "cycle_b_position" where

  type CFieldType Cycle_b "cycle_b_position" =
    AllOutputKinds.Nested.By_value.By_value

  offset# = \_ -> \_ -> 16

{-| __C declaration:__ @callback@

    __defined at:__ @all_output_kinds\/cycle_b.h 18:20@

    __exported by:__ @all_output_kinds.h@
-}
instance ( ty ~ Cycle_b_callback
         ) => BG.CompatHasField.HasField "cycle_b_callback" Cycle_b ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Cycle_b { cycle_b_callback = y1
                  , cycle_b_owner = BG.getField @"cycle_b_owner" x0
                  , cycle_b_next = BG.getField @"cycle_b_next" x0
                  , cycle_b_position = BG.getField @"cycle_b_position" x0
                  }
      , BG.getField @"cycle_b_callback" x0
      )

instance ( ty ~ Cycle_b_callback
         ) => BG.HasField "cycle_b_callback" (BG.Ptr Cycle_b) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"cycle_b_callback")

instance HasCField.HasCField Cycle_b "cycle_b_callback" where

  type CFieldType Cycle_b "cycle_b_callback" =
    Cycle_b_callback

  offset# = \_ -> \_ -> 24
