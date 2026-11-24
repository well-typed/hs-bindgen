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

module DoxygenInLibraryHeaders.A
    ( DoxygenInLibraryHeaders.A.A_node(..)
    , DoxygenInLibraryHeaders.A.a_MAX
    )
  where

import qualified DoxygenInLibraryHeaders.B
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| A node of a list.

    __See:__ 'B_list'

    __C declaration:__ @struct a_node@

    __defined at:__ @doxygen_in_library_headers\/a.h 11:8@

    __exported by:__ @doxygen_in_library_headers.h@
-}
data A_node = A_node
  { a_node_value :: BG.CInt
    {- ^ The stored value

         __C declaration:__ @value@

         __defined at:__ @doxygen_in_library_headers\/a.h 13:7@

         __exported by:__ @doxygen_in_library_headers.h@
    -}
  , a_node_owner :: BG.Ptr DoxygenInLibraryHeaders.B.B_list
    {- ^ The list that owns this node

         __C declaration:__ @owner@

         __defined at:__ @doxygen_in_library_headers\/a.h 15:18@

         __exported by:__ @doxygen_in_library_headers.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize A_node where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw A_node where

  readRaw =
    \ptr0 ->
          pure A_node
      <*> HasCField.readRaw (BG.Proxy @"a_node_value") ptr0
      <*> HasCField.readRaw (BG.Proxy @"a_node_owner") ptr0

instance Marshal.WriteRaw A_node where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          A_node a_node_value2 a_node_owner3 ->
               HasCField.writeRaw (BG.Proxy @"a_node_value") ptr0 a_node_value2
            >> HasCField.writeRaw (BG.Proxy @"a_node_owner") ptr0 a_node_owner3

deriving via Marshal.EquivStorable A_node instance BG.Storable A_node

deriving via Struct.IsStructViaReadRaw A_node instance Struct.IsStruct A_node

{-| The stored value

    __C declaration:__ @value@

    __defined at:__ @doxygen_in_library_headers\/a.h 13:7@

    __exported by:__ @doxygen_in_library_headers.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "a_node_value" A_node ty where

  hasField =
    \x0 ->
      ( \y1 ->
          A_node {a_node_value = y1, a_node_owner = BG.getField @"a_node_owner" x0}
      , BG.getField @"a_node_value" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "a_node_value" (BG.Ptr A_node) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"a_node_value")

instance HasCField.HasCField A_node "a_node_value" where

  type CFieldType A_node "a_node_value" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| The list that owns this node

    __C declaration:__ @owner@

    __defined at:__ @doxygen_in_library_headers\/a.h 15:18@

    __exported by:__ @doxygen_in_library_headers.h@
-}
instance ( ty ~ BG.Ptr DoxygenInLibraryHeaders.B.B_list
         ) => BG.CompatHasField.HasField "a_node_owner" A_node ty where

  hasField =
    \x0 ->
      ( \y1 ->
          A_node {a_node_owner = y1, a_node_value = BG.getField @"a_node_value" x0}
      , BG.getField @"a_node_owner" x0
      )

instance ( ty ~ BG.Ptr DoxygenInLibraryHeaders.B.B_list
         ) => BG.HasField "a_node_owner" (BG.Ptr A_node) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"a_node_owner")

instance HasCField.HasCField A_node "a_node_owner" where

  type CFieldType A_node "a_node_owner" =
    BG.Ptr DoxygenInLibraryHeaders.B.B_list

  offset# = \_ -> \_ -> 8

{-| Number of nodes, at most

    __C declaration:__ @macro A_MAX@

    __defined at:__ @doxygen_in_library_headers\/a.h 19:9@

    __exported by:__ @doxygen_in_library_headers.h@
-}
a_MAX :: BG.CInt
a_MAX = (16 :: BG.CInt)
