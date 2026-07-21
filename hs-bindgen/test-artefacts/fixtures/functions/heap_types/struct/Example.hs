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
    ( Example.T(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct S@

    __defined at:__ @functions\/heap_types\/struct.h 3:8@

    __exported by:__ @functions\/heap_types\/struct.h@
-}
data T = T
  { t_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @functions\/heap_types\/struct.h 4:7@

         __exported by:__ @functions\/heap_types\/struct.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize T where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw T where

  readRaw =
    \ptr0 ->
          pure T
      <*> HasCField.readRaw (BG.Proxy @"t_x") ptr0

instance Marshal.WriteRaw T where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T t_x2 ->
            HasCField.writeRaw (BG.Proxy @"t_x") ptr0 t_x2

deriving via Marshal.EquivStorable T instance BG.Storable T

{-| __C declaration:__ @x@

    __defined at:__ @functions\/heap_types\/struct.h 4:7@

    __exported by:__ @functions\/heap_types\/struct.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "t_x" T ty where

  hasField =
    \x0 -> (\y1 -> T {t_x = y1}, BG.getField @"t_x" x0)

instance (ty ~ BG.CInt) => BG.HasField "t_x" (BG.Ptr T) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t_x")

instance HasCField.HasCField T "t_x" where

  type CFieldType T "t_x" = BG.CInt

  offset# = \_ -> \_ -> 0
