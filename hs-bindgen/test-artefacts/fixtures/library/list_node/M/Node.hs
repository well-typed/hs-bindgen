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

module M.Node
    ( M.Node.Node(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import {-# SOURCE #-} qualified M.List
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct node@

    __defined at:__ @node.h 5:8@

    __exported by:__ @node.h@
-}
data Node = Node
  { node_owner :: BG.Ptr M.List.List
    {- ^ __C declaration:__ @owner@

         __defined at:__ @node.h 5:28@

         __exported by:__ @node.h@
    -}
  , node_next :: BG.Ptr Node
    {- ^ __C declaration:__ @next@

         __defined at:__ @node.h 5:48@

         __exported by:__ @node.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Node where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Node where

  readRaw =
    \ptr0 ->
          pure Node
      <*> HasCField.readRaw (BG.Proxy @"node_owner") ptr0
      <*> HasCField.readRaw (BG.Proxy @"node_next") ptr0

instance Marshal.WriteRaw Node where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Node node_owner2 node_next3 ->
               HasCField.writeRaw (BG.Proxy @"node_owner") ptr0 node_owner2
            >> HasCField.writeRaw (BG.Proxy @"node_next") ptr0 node_next3

deriving via Marshal.EquivStorable Node instance BG.Storable Node

deriving via Struct.IsStructViaReadRaw Node instance Struct.IsStruct Node

{-| __C declaration:__ @owner@

    __defined at:__ @node.h 5:28@

    __exported by:__ @node.h@
-}
instance ( ty ~ BG.Ptr M.List.List
         ) => BG.CompatHasField.HasField "node_owner" Node ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Node {node_owner = y1, node_next = BG.getField @"node_next" x0}
      , BG.getField @"node_owner" x0
      )

instance ( ty ~ BG.Ptr M.List.List
         ) => BG.HasField "node_owner" (BG.Ptr Node) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"node_owner")

instance HasCField.HasCField Node "node_owner" where

  type CFieldType Node "node_owner" =
    BG.Ptr M.List.List

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @next@

    __defined at:__ @node.h 5:48@

    __exported by:__ @node.h@
-}
instance ( ty ~ BG.Ptr Node
         ) => BG.CompatHasField.HasField "node_next" Node ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Node {node_next = y1, node_owner = BG.getField @"node_owner" x0}
      , BG.getField @"node_next" x0
      )

instance ( ty ~ BG.Ptr Node
         ) => BG.HasField "node_next" (BG.Ptr Node) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"node_next")

instance HasCField.HasCField Node "node_next" where

  type CFieldType Node "node_next" = BG.Ptr Node

  offset# = \_ -> \_ -> 8
