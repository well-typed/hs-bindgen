{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
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
    ( Example.Opaque
    , Example.Outside(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct opaque@

    __defined at:__ @functions\/decls_in_signature.h 2:8@

    __exported by:__ @functions\/decls_in_signature.h@
-}
data Opaque

{-| __C declaration:__ @struct outside@

    __defined at:__ @functions\/decls_in_signature.h 3:8@

    __exported by:__ @functions\/decls_in_signature.h@
-}
data Outside = Outside
  { outside_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @functions\/decls_in_signature.h 4:7@

         __exported by:__ @functions\/decls_in_signature.h@
    -}
  , outside_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @functions\/decls_in_signature.h 5:7@

         __exported by:__ @functions\/decls_in_signature.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Outside where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Outside where

  readRaw =
    \ptr0 ->
          pure Outside
      <*> HasCField.readRaw (BG.Proxy @"outside_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"outside_y") ptr0

instance Marshal.WriteRaw Outside where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Outside outside_x2 outside_y3 ->
               HasCField.writeRaw (BG.Proxy @"outside_x") ptr0 outside_x2
            >> HasCField.writeRaw (BG.Proxy @"outside_y") ptr0 outside_y3

deriving via Marshal.EquivStorable Outside instance BG.Storable Outside

deriving via Struct.IsStructViaStorable Outside instance Struct.IsStruct Outside

{-| __C declaration:__ @x@

    __defined at:__ @functions\/decls_in_signature.h 4:7@

    __exported by:__ @functions\/decls_in_signature.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "outside_x" Outside ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Outside {outside_x = y1, outside_y = BG.getField @"outside_y" x0}
      , BG.getField @"outside_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "outside_x" (BG.Ptr Outside) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"outside_x")

instance HasCField.HasCField Outside "outside_x" where

  type CFieldType Outside "outside_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @functions\/decls_in_signature.h 5:7@

    __exported by:__ @functions\/decls_in_signature.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "outside_y" Outside ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Outside {outside_y = y1, outside_x = BG.getField @"outside_x" x0}
      , BG.getField @"outside_y" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "outside_y" (BG.Ptr Outside) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"outside_y")

instance HasCField.HasCField Outside "outside_y" where

  type CFieldType Outside "outside_y" = BG.CInt

  offset# = \_ -> \_ -> 4
