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

module AllOutputKinds.Cycle_a
    ( AllOutputKinds.Cycle_a.cYCLE_A_LIMIT
    , AllOutputKinds.Cycle_a.Cycle_a(..)
    )
  where

import qualified AllOutputKinds.Nested.By_value
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import {-# SOURCE #-} qualified AllOutputKinds.Cycle_b
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @macro CYCLE_A_LIMIT@

    __defined at:__ @all_output_kinds\/cycle_a.h 9:9@

    __exported by:__ @all_output_kinds.h@
-}
cYCLE_A_LIMIT :: BG.CInt
cYCLE_A_LIMIT = (64 :: BG.CInt)

{-| __C declaration:__ @struct cycle_a@

    __defined at:__ @all_output_kinds\/cycle_a.h 13:8@

    __exported by:__ @all_output_kinds.h@
-}
data Cycle_a = Cycle_a
  { cycle_a_position :: AllOutputKinds.Nested.By_value.By_value
    {- ^ __C declaration:__ @position@

         __defined at:__ @all_output_kinds\/cycle_a.h 14:19@

         __exported by:__ @all_output_kinds.h@
    -}
  , cycle_a_first :: BG.Ptr AllOutputKinds.Cycle_b.Cycle_b
    {- ^ __C declaration:__ @first@

         __defined at:__ @all_output_kinds\/cycle_a.h 15:19@

         __exported by:__ @all_output_kinds.h@
    -}
  , cycle_a_count :: BG.CInt
    {- ^ __C declaration:__ @count@

         __defined at:__ @all_output_kinds\/cycle_a.h 16:7@

         __exported by:__ @all_output_kinds.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Cycle_a where

  staticSizeOf = \_ -> (24 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Cycle_a where

  readRaw =
    \ptr0 ->
          pure Cycle_a
      <*> HasCField.readRaw (BG.Proxy @"cycle_a_position") ptr0
      <*> HasCField.readRaw (BG.Proxy @"cycle_a_first") ptr0
      <*> HasCField.readRaw (BG.Proxy @"cycle_a_count") ptr0

instance Marshal.WriteRaw Cycle_a where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Cycle_a cycle_a_position2 cycle_a_first3 cycle_a_count4 ->
               HasCField.writeRaw (BG.Proxy @"cycle_a_position") ptr0 cycle_a_position2
            >> HasCField.writeRaw (BG.Proxy @"cycle_a_first") ptr0 cycle_a_first3
            >> HasCField.writeRaw (BG.Proxy @"cycle_a_count") ptr0 cycle_a_count4

deriving via Marshal.EquivStorable Cycle_a instance BG.Storable Cycle_a

deriving via Struct.IsStructViaReadRaw Cycle_a instance Struct.IsStruct Cycle_a

{-| __C declaration:__ @position@

    __defined at:__ @all_output_kinds\/cycle_a.h 14:19@

    __exported by:__ @all_output_kinds.h@
-}
instance ( ty ~ AllOutputKinds.Nested.By_value.By_value
         ) => BG.CompatHasField.HasField "cycle_a_position" Cycle_a ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Cycle_a { cycle_a_position = y1
                  , cycle_a_first = BG.getField @"cycle_a_first" x0
                  , cycle_a_count = BG.getField @"cycle_a_count" x0
                  }
      , BG.getField @"cycle_a_position" x0
      )

instance ( ty ~ AllOutputKinds.Nested.By_value.By_value
         ) => BG.HasField "cycle_a_position" (BG.Ptr Cycle_a) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"cycle_a_position")

instance HasCField.HasCField Cycle_a "cycle_a_position" where

  type CFieldType Cycle_a "cycle_a_position" =
    AllOutputKinds.Nested.By_value.By_value

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @first@

    __defined at:__ @all_output_kinds\/cycle_a.h 15:19@

    __exported by:__ @all_output_kinds.h@
-}
instance ( ty ~ BG.Ptr AllOutputKinds.Cycle_b.Cycle_b
         ) => BG.CompatHasField.HasField "cycle_a_first" Cycle_a ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Cycle_a { cycle_a_first = y1
                  , cycle_a_position = BG.getField @"cycle_a_position" x0
                  , cycle_a_count = BG.getField @"cycle_a_count" x0
                  }
      , BG.getField @"cycle_a_first" x0
      )

instance ( ty ~ BG.Ptr AllOutputKinds.Cycle_b.Cycle_b
         ) => BG.HasField "cycle_a_first" (BG.Ptr Cycle_a) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"cycle_a_first")

instance HasCField.HasCField Cycle_a "cycle_a_first" where

  type CFieldType Cycle_a "cycle_a_first" =
    BG.Ptr AllOutputKinds.Cycle_b.Cycle_b

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @count@

    __defined at:__ @all_output_kinds\/cycle_a.h 16:7@

    __exported by:__ @all_output_kinds.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "cycle_a_count" Cycle_a ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Cycle_a { cycle_a_count = y1
                  , cycle_a_position = BG.getField @"cycle_a_position" x0
                  , cycle_a_first = BG.getField @"cycle_a_first" x0
                  }
      , BG.getField @"cycle_a_count" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "cycle_a_count" (BG.Ptr Cycle_a) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"cycle_a_count")

instance HasCField.HasCField Cycle_a "cycle_a_count" where

  type CFieldType Cycle_a "cycle_a_count" = BG.CInt

  offset# = \_ -> \_ -> 16
