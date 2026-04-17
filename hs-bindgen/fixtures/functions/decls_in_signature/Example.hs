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
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

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
  { outside_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @functions\/decls_in_signature.h 4:7@

         __exported by:__ @functions\/decls_in_signature.h@
    -}
  , outside_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @functions\/decls_in_signature.h 5:7@

         __exported by:__ @functions\/decls_in_signature.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Outside where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Outside where

  readRaw =
    \ptr0 ->
          pure Outside
      <*> HasCField.readRaw (RIP.Proxy @"outside_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"outside_y") ptr0

instance Marshal.WriteRaw Outside where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Outside outside_x2 outside_y3 ->
               HasCField.writeRaw (RIP.Proxy @"outside_x") ptr0 outside_x2
            >> HasCField.writeRaw (RIP.Proxy @"outside_y") ptr0 outside_y3

deriving via Marshal.EquivStorable Outside instance RIP.Storable Outside

instance HasCField.HasCField Outside "outside_x" where

  type CFieldType Outside "outside_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "outside_x" (RIP.Ptr Outside) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"outside_x")

instance HasCField.HasCField Outside "outside_y" where

  type CFieldType Outside "outside_y" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "outside_y" (RIP.Ptr Outside) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"outside_y")
