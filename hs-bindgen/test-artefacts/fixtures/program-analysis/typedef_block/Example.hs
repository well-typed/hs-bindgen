{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Blk_struct(..)
    , Example.Blk(..)
    , Example.Blkarg_struct(..)
    , Example.Blkarg(..)
    )
  where

import qualified HsBindgen.Runtime.Block as Block
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
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

instance HasCField.HasCField Blk_struct "blk_struct_x" where

  type CFieldType Blk_struct "blk_struct_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "blk_struct_x" (RIP.Ptr Blk_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"blk_struct_x")

{-| __C declaration:__ @blk@

    __defined at:__ @program-analysis\/typedef_block.h 14:22@

    __exported by:__ @program-analysis\/typedef_block.h@
-}
newtype Blk = Blk
  { unwrapBlk :: Block.Block (IO Blk_struct)
  }
  deriving stock (RIP.Generic)

instance ( ty ~ Block.Block (IO Blk_struct)
         ) => RIP.HasField "unwrapBlk" (RIP.Ptr Blk) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapBlk")

instance HasCField.HasCField Blk "unwrapBlk" where

  type CFieldType Blk "unwrapBlk" =
    Block.Block (IO Blk_struct)

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

instance HasCField.HasCField Blkarg_struct "blkarg_struct_y" where

  type CFieldType Blkarg_struct "blkarg_struct_y" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "blkarg_struct_y" (RIP.Ptr Blkarg_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"blkarg_struct_y")

{-| __C declaration:__ @blkarg@

    __defined at:__ @program-analysis\/typedef_block.h 18:15@

    __exported by:__ @program-analysis\/typedef_block.h@
-}
newtype Blkarg = Blkarg
  { unwrapBlkarg :: Block.Block (Blkarg_struct -> IO RIP.CInt)
  }
  deriving stock (RIP.Generic)

instance ( ty ~ Block.Block (Blkarg_struct -> IO RIP.CInt)
         ) => RIP.HasField "unwrapBlkarg" (RIP.Ptr Blkarg) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapBlkarg")

instance HasCField.HasCField Blkarg "unwrapBlkarg" where

  type CFieldType Blkarg "unwrapBlkarg" =
    Block.Block (Blkarg_struct -> IO RIP.CInt)

  offset# = \_ -> \_ -> 0
