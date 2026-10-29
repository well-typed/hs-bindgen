{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
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

module M.List
    ( M.List.List(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import {-# SOURCE #-} qualified M.Node
import Prelude ((<*>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct list@

    __defined at:__ @list.h 5:8@

    __exported by:__ @node.h@
-}
data List = List
  { list_head :: BG.Ptr M.Node.Node
    {- ^ __C declaration:__ @head@

         __defined at:__ @list.h 5:28@

         __exported by:__ @node.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize List where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw List where

  readRaw =
    \ptr0 ->
          pure List
      <*> HasCField.readRaw (BG.Proxy @"list_head") ptr0

instance Marshal.WriteRaw List where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          List list_head2 ->
            HasCField.writeRaw (BG.Proxy @"list_head") ptr0 list_head2

deriving via Marshal.EquivStorable List instance BG.Storable List

deriving via Struct.IsStructViaReadRaw List instance Struct.IsStruct List

{-| __C declaration:__ @head@

    __defined at:__ @list.h 5:28@

    __exported by:__ @node.h@
-}
instance ( ty ~ BG.Ptr M.Node.Node
         ) => BG.CompatHasField.HasField "list_head" List ty where

  hasField =
    \x0 ->
      (\y1 ->
         List {list_head = y1}, BG.getField @"list_head" x0)

instance ( ty ~ BG.Ptr M.Node.Node
         ) => BG.HasField "list_head" (BG.Ptr List) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"list_head")

instance HasCField.HasCField List "list_head" where

  type CFieldType List "list_head" = BG.Ptr M.Node.Node

  offset# = \_ -> \_ -> 0
