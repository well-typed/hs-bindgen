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

module DoxygenInLibraryHeaders.B
    ( DoxygenInLibraryHeaders.B.B_list(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Eq, Int, Show, pure, type (~))

{-| A list of nodes

    __C declaration:__ @struct b_list@

    __defined at:__ @b.h 5:8@

    __exported by:__ @doxygen_in_library_headers.h@
-}
data B_list = B_list
  { b_list_length :: BG.CInt
    {- ^ Number of nodes in the list

         __C declaration:__ @length@

         __defined at:__ @b.h 6:7@

         __exported by:__ @doxygen_in_library_headers.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize B_list where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw B_list where

  readRaw =
    \ptr0 ->
          pure B_list
      <*> HasCField.readRaw (BG.Proxy @"b_list_length") ptr0

instance Marshal.WriteRaw B_list where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          B_list b_list_length2 ->
            HasCField.writeRaw (BG.Proxy @"b_list_length") ptr0 b_list_length2

deriving via Marshal.EquivStorable B_list instance BG.Storable B_list

deriving via Struct.IsStructViaReadRaw B_list instance Struct.IsStruct B_list

{-| Number of nodes in the list

    __C declaration:__ @length@

    __defined at:__ @b.h 6:7@

    __exported by:__ @doxygen_in_library_headers.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "b_list_length" B_list ty where

  hasField =
    \x0 ->
      (\y1 ->
         B_list {b_list_length = y1}, BG.getField @"b_list_length" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "b_list_length" (BG.Ptr B_list) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"b_list_length")

instance HasCField.HasCField B_list "b_list_length" where

  type CFieldType B_list "b_list_length" = BG.CInt

  offset# = \_ -> \_ -> 0
