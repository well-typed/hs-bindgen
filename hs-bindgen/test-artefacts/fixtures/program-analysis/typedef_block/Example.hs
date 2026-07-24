{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Blk_struct(..)
    , Example.Blk_Aux(..)
    , Example.Blk(..)
    , Example.Blkarg_struct(..)
    , Example.Blkarg_Aux(..)
    , Example.Blkarg(..)
    )
  where

import qualified HsBindgen.Runtime.Block as Block
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct blk@

    __defined at:__ @program-analysis\/typedef_block.h 13:8@

    __exported by:__ @program-analysis\/typedef_block.h@
-}
data Blk_struct = Blk_struct
  { blk_struct_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @program-analysis\/typedef_block.h 13:18@

         __exported by:__ @program-analysis\/typedef_block.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Blk_struct where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Blk_struct where

  readRaw =
    \ptr0 ->
          pure Blk_struct
      <*> HasCField.readRaw (BG.Proxy @"blk_struct_x") ptr0

instance Marshal.WriteRaw Blk_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Blk_struct blk_struct_x2 ->
            HasCField.writeRaw (BG.Proxy @"blk_struct_x") ptr0 blk_struct_x2

deriving via Marshal.EquivStorable Blk_struct instance BG.Storable Blk_struct

deriving via Struct.IsStructViaStorable Blk_struct instance Struct.IsStruct Blk_struct

{-| __C declaration:__ @x@

    __defined at:__ @program-analysis\/typedef_block.h 13:18@

    __exported by:__ @program-analysis\/typedef_block.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "blk_struct_x" Blk_struct ty where

  hasField =
    \x0 ->
      (\y1 ->
         Blk_struct {blk_struct_x = y1}, BG.getField @"blk_struct_x" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "blk_struct_x" (BG.Ptr Blk_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"blk_struct_x")

instance HasCField.HasCField Blk_struct "blk_struct_x" where

  type CFieldType Blk_struct "blk_struct_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Blk'

    __C declaration:__ @blk@

    __defined at:__ @program-analysis\/typedef_block.h 14:22@

    __exported by:__ @program-analysis\/typedef_block.h@
-}
newtype Blk_Aux = Blk_Aux
  { unwrapBlk_Aux :: IO Blk_struct
  }
  deriving stock (BG.Generic)

instance ( ty ~ IO Blk_struct
         ) => BG.CompatHasField.HasField "unwrapBlk_Aux" Blk_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         Blk_Aux {unwrapBlk_Aux = y1}, BG.getField @"unwrapBlk_Aux" x0)

instance ( ty ~ IO Blk_struct
         ) => BG.HasField "unwrapBlk_Aux" (BG.Ptr Blk_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapBlk_Aux")

instance HasCField.HasCField Blk_Aux "unwrapBlk_Aux" where

  type CFieldType Blk_Aux "unwrapBlk_Aux" =
    IO Blk_struct

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @blk@

    __defined at:__ @program-analysis\/typedef_block.h 14:22@

    __exported by:__ @program-analysis\/typedef_block.h@
-}
newtype Blk = Blk
  { unwrapBlk :: Block.Block Blk_Aux
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

instance ( ty ~ Block.Block Blk_Aux
         ) => BG.CompatHasField.HasField "unwrapBlk" Blk ty where

  hasField =
    \x0 ->
      (\y1 ->
         Blk {unwrapBlk = y1}, BG.getField @"unwrapBlk" x0)

instance ( ty ~ Block.Block Blk_Aux
         ) => BG.HasField "unwrapBlk" (BG.Ptr Blk) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapBlk")

instance HasCField.HasCField Blk "unwrapBlk" where

  type CFieldType Blk "unwrapBlk" = Block.Block Blk_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct blkarg@

    __defined at:__ @program-analysis\/typedef_block.h 17:8@

    __exported by:__ @program-analysis\/typedef_block.h@
-}
data Blkarg_struct = Blkarg_struct
  { blkarg_struct_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @program-analysis\/typedef_block.h 17:21@

         __exported by:__ @program-analysis\/typedef_block.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Blkarg_struct where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Blkarg_struct where

  readRaw =
    \ptr0 ->
          pure Blkarg_struct
      <*> HasCField.readRaw (BG.Proxy @"blkarg_struct_y") ptr0

instance Marshal.WriteRaw Blkarg_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Blkarg_struct blkarg_struct_y2 ->
            HasCField.writeRaw (BG.Proxy @"blkarg_struct_y") ptr0 blkarg_struct_y2

deriving via Marshal.EquivStorable Blkarg_struct instance BG.Storable Blkarg_struct

deriving via Struct.IsStructViaStorable Blkarg_struct instance Struct.IsStruct Blkarg_struct

{-| __C declaration:__ @y@

    __defined at:__ @program-analysis\/typedef_block.h 17:21@

    __exported by:__ @program-analysis\/typedef_block.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "blkarg_struct_y" Blkarg_struct ty where

  hasField =
    \x0 ->
      ( \y1 -> Blkarg_struct {blkarg_struct_y = y1}
      , BG.getField @"blkarg_struct_y" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "blkarg_struct_y" (BG.Ptr Blkarg_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"blkarg_struct_y")

instance HasCField.HasCField Blkarg_struct "blkarg_struct_y" where

  type CFieldType Blkarg_struct "blkarg_struct_y" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Blkarg'

    __C declaration:__ @blkarg@

    __defined at:__ @program-analysis\/typedef_block.h 18:15@

    __exported by:__ @program-analysis\/typedef_block.h@
-}
newtype Blkarg_Aux = Blkarg_Aux
  { unwrapBlkarg_Aux :: Blkarg_struct -> IO BG.CInt
  }
  deriving stock (BG.Generic)

instance ( ty ~ (Blkarg_struct -> IO BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapBlkarg_Aux" Blkarg_Aux ty where

  hasField =
    \x0 ->
      ( \y1 -> Blkarg_Aux {unwrapBlkarg_Aux = y1}
      , BG.getField @"unwrapBlkarg_Aux" x0
      )

instance ( ty ~ (Blkarg_struct -> IO BG.CInt)
         ) => BG.HasField "unwrapBlkarg_Aux" (BG.Ptr Blkarg_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapBlkarg_Aux")

instance HasCField.HasCField Blkarg_Aux "unwrapBlkarg_Aux" where

  type CFieldType Blkarg_Aux "unwrapBlkarg_Aux" =
    Blkarg_struct -> IO BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @blkarg@

    __defined at:__ @program-analysis\/typedef_block.h 18:15@

    __exported by:__ @program-analysis\/typedef_block.h@
-}
newtype Blkarg = Blkarg
  { unwrapBlkarg :: Block.Block Blkarg_Aux
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

instance ( ty ~ Block.Block Blkarg_Aux
         ) => BG.CompatHasField.HasField "unwrapBlkarg" Blkarg ty where

  hasField =
    \x0 ->
      (\y1 ->
         Blkarg {unwrapBlkarg = y1}, BG.getField @"unwrapBlkarg" x0)

instance ( ty ~ Block.Block Blkarg_Aux
         ) => BG.HasField "unwrapBlkarg" (BG.Ptr Blkarg) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapBlkarg")

instance HasCField.HasCField Blkarg "unwrapBlkarg" where

  type CFieldType Blkarg "unwrapBlkarg" =
    Block.Block Blkarg_Aux

  offset# = \_ -> \_ -> 0
