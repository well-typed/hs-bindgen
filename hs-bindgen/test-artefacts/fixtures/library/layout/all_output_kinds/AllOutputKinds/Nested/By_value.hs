{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module AllOutputKinds.Nested.By_value
    ( AllOutputKinds.Nested.By_value.bY_VALUE_FIELDS
    , AllOutputKinds.Nested.By_value.By_value(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @macro BY_VALUE_FIELDS@

    __defined at:__ @all_output_kinds\/nested\/by_value.h 6:9@

    __exported by:__ @all_output_kinds.h@
-}
bY_VALUE_FIELDS :: BG.CInt
bY_VALUE_FIELDS = (2 :: BG.CInt)

{-| __C declaration:__ @struct by_value@

    __defined at:__ @all_output_kinds\/nested\/by_value.h 8:8@

    __exported by:__ @all_output_kinds.h@
-}
data By_value = By_value
  { by_value_x :: BG.CFloat
    {- ^ __C declaration:__ @x@

         __defined at:__ @all_output_kinds\/nested\/by_value.h 9:9@

         __exported by:__ @all_output_kinds.h@
    -}
  , by_value_y :: BG.CFloat
    {- ^ __C declaration:__ @y@

         __defined at:__ @all_output_kinds\/nested\/by_value.h 10:9@

         __exported by:__ @all_output_kinds.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize By_value where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw By_value where

  readRaw =
    \ptr0 ->
          pure By_value
      <*> HasCField.readRaw (BG.Proxy @"by_value_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"by_value_y") ptr0

instance Marshal.WriteRaw By_value where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          By_value by_value_x2 by_value_y3 ->
               HasCField.writeRaw (BG.Proxy @"by_value_x") ptr0 by_value_x2
            >> HasCField.writeRaw (BG.Proxy @"by_value_y") ptr0 by_value_y3

deriving via Marshal.EquivStorable By_value instance BG.Storable By_value

deriving via Struct.IsStructViaReadRaw By_value instance Struct.IsStruct By_value

{-| __C declaration:__ @x@

    __defined at:__ @all_output_kinds\/nested\/by_value.h 9:9@

    __exported by:__ @all_output_kinds.h@
-}
instance ( ty ~ BG.CFloat
         ) => BG.CompatHasField.HasField "by_value_x" By_value ty where

  hasField =
    \x0 ->
      ( \y1 ->
          By_value {by_value_x = y1, by_value_y = BG.getField @"by_value_y" x0}
      , BG.getField @"by_value_x" x0
      )

instance ( ty ~ BG.CFloat
         ) => BG.HasField "by_value_x" (BG.Ptr By_value) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"by_value_x")

instance HasCField.HasCField By_value "by_value_x" where

  type CFieldType By_value "by_value_x" = BG.CFloat

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @all_output_kinds\/nested\/by_value.h 10:9@

    __exported by:__ @all_output_kinds.h@
-}
instance ( ty ~ BG.CFloat
         ) => BG.CompatHasField.HasField "by_value_y" By_value ty where

  hasField =
    \x0 ->
      ( \y1 ->
          By_value {by_value_y = y1, by_value_x = BG.getField @"by_value_x" x0}
      , BG.getField @"by_value_y" x0
      )

instance ( ty ~ BG.CFloat
         ) => BG.HasField "by_value_y" (BG.Ptr By_value) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"by_value_y")

instance HasCField.HasCField By_value "by_value_y" where

  type CFieldType By_value "by_value_y" = BG.CFloat

  offset# = \_ -> \_ -> 4
