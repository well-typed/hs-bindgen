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
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Internal.Prelude.CompatHasField as RIP.CompatHasField
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct blk@

    __defined at:__ @program-analysis\/typedef_block.h 13:8@

    __exported by:__ @program-analysis\/typedef_block.h@
-}
data Blk_struct = Blk_struct
  { blk_struct_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @program-analysis\/typedef_block.h 13:18@

         __exported by:__ @program-analysis\/typedef_block.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Blk_struct where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Blk_struct where

  readRaw =
    \ptr0 ->
          pure Blk_struct
      <*> HasCField.readRaw (RIP.Proxy @"blk_struct_x") ptr0

instance Marshal.WriteRaw Blk_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Blk_struct blk_struct_x2 ->
            HasCField.writeRaw (RIP.Proxy @"blk_struct_x") ptr0 blk_struct_x2

deriving via Marshal.EquivStorable Blk_struct instance RIP.Storable Blk_struct

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "blk_struct_x" Blk_struct ty where

  hasField =
    \x0 ->
      (\y1 ->
         Blk_struct {blk_struct_x = y1}, RIP.getField @"blk_struct_x" x0)

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "blk_struct_x" (RIP.Ptr Blk_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"blk_struct_x")

instance HasCField.HasCField Blk_struct "blk_struct_x" where

  type CFieldType Blk_struct "blk_struct_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Blk'

    __C declaration:__ @blk@

    __defined at:__ @program-analysis\/typedef_block.h 14:22@

    __exported by:__ @program-analysis\/typedef_block.h@
-}
newtype Blk_Aux = Blk_Aux
  { unwrapBlk_Aux :: IO Blk_struct
  }
  deriving stock (RIP.Generic)

instance ( ty ~ IO Blk_struct
         ) => RIP.CompatHasField.HasField "unwrapBlk_Aux" Blk_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         Blk_Aux {unwrapBlk_Aux = y1}, RIP.getField @"unwrapBlk_Aux" x0)

instance ( ty ~ IO Blk_struct
         ) => RIP.HasField "unwrapBlk_Aux" (RIP.Ptr Blk_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapBlk_Aux")

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
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

instance ( ty ~ Block.Block Blk_Aux
         ) => RIP.CompatHasField.HasField "unwrapBlk" Blk ty where

  hasField =
    \x0 ->
      (\y1 ->
         Blk {unwrapBlk = y1}, RIP.getField @"unwrapBlk" x0)

instance ( ty ~ Block.Block Blk_Aux
         ) => RIP.HasField "unwrapBlk" (RIP.Ptr Blk) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapBlk")

instance HasCField.HasCField Blk "unwrapBlk" where

  type CFieldType Blk "unwrapBlk" = Block.Block Blk_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct blkarg@

    __defined at:__ @program-analysis\/typedef_block.h 17:8@

    __exported by:__ @program-analysis\/typedef_block.h@
-}
data Blkarg_struct = Blkarg_struct
  { blkarg_struct_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @program-analysis\/typedef_block.h 17:21@

         __exported by:__ @program-analysis\/typedef_block.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Blkarg_struct where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Blkarg_struct where

  readRaw =
    \ptr0 ->
          pure Blkarg_struct
      <*> HasCField.readRaw (RIP.Proxy @"blkarg_struct_y") ptr0

instance Marshal.WriteRaw Blkarg_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Blkarg_struct blkarg_struct_y2 ->
            HasCField.writeRaw (RIP.Proxy @"blkarg_struct_y") ptr0 blkarg_struct_y2

deriving via Marshal.EquivStorable Blkarg_struct instance RIP.Storable Blkarg_struct

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "blkarg_struct_y" Blkarg_struct ty where

  hasField =
    \x0 ->
      ( \y1 -> Blkarg_struct {blkarg_struct_y = y1}
      , RIP.getField @"blkarg_struct_y" x0
      )

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "blkarg_struct_y" (RIP.Ptr Blkarg_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"blkarg_struct_y")

instance HasCField.HasCField Blkarg_struct "blkarg_struct_y" where

  type CFieldType Blkarg_struct "blkarg_struct_y" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Blkarg'

    __C declaration:__ @blkarg@

    __defined at:__ @program-analysis\/typedef_block.h 18:15@

    __exported by:__ @program-analysis\/typedef_block.h@
-}
newtype Blkarg_Aux = Blkarg_Aux
  { unwrapBlkarg_Aux :: Blkarg_struct -> IO RIP.CInt
  }
  deriving stock (RIP.Generic)

instance ( ty ~ (Blkarg_struct -> IO RIP.CInt)
         ) => RIP.CompatHasField.HasField "unwrapBlkarg_Aux" Blkarg_Aux ty where

  hasField =
    \x0 ->
      ( \y1 -> Blkarg_Aux {unwrapBlkarg_Aux = y1}
      , RIP.getField @"unwrapBlkarg_Aux" x0
      )

instance ( ty ~ (Blkarg_struct -> IO RIP.CInt)
         ) => RIP.HasField "unwrapBlkarg_Aux" (RIP.Ptr Blkarg_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapBlkarg_Aux")

instance HasCField.HasCField Blkarg_Aux "unwrapBlkarg_Aux" where

  type CFieldType Blkarg_Aux "unwrapBlkarg_Aux" =
    Blkarg_struct -> IO RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @blkarg@

    __defined at:__ @program-analysis\/typedef_block.h 18:15@

    __exported by:__ @program-analysis\/typedef_block.h@
-}
newtype Blkarg = Blkarg
  { unwrapBlkarg :: Block.Block Blkarg_Aux
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

instance ( ty ~ Block.Block Blkarg_Aux
         ) => RIP.CompatHasField.HasField "unwrapBlkarg" Blkarg ty where

  hasField =
    \x0 ->
      (\y1 ->
         Blkarg {unwrapBlkarg = y1}, RIP.getField @"unwrapBlkarg" x0)

instance ( ty ~ Block.Block Blkarg_Aux
         ) => RIP.HasField "unwrapBlkarg" (RIP.Ptr Blkarg) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapBlkarg")

instance HasCField.HasCField Blkarg "unwrapBlkarg" where

  type CFieldType Blkarg "unwrapBlkarg" =
    Block.Block Blkarg_Aux

  offset# = \_ -> \_ -> 0
