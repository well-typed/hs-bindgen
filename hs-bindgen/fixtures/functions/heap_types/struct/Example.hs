{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct S@

    __defined at:__ @functions\/heap_types\/struct.h 3:8@

    __exported by:__ @functions\/heap_types\/struct.h@
-}
data T = T
  { t_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @functions\/heap_types\/struct.h 4:7@

         __exported by:__ @functions\/heap_types\/struct.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize T where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw T where

  readRaw =
    \ptr0 ->
          pure T
      <*> HasCField.readRaw (RIP.Proxy @"t_x") ptr0

instance Marshal.WriteRaw T where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T t_x2 ->
            HasCField.writeRaw (RIP.Proxy @"t_x") ptr0 t_x2

deriving via Marshal.EquivStorable T instance RIP.Storable T

instance HasCField.HasCField T "t_x" where

  type CFieldType T "t_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "t_x" (RIP.Ptr T) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"t_x")
