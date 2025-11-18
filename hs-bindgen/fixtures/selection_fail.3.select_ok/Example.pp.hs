{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @OkBefore@

    __defined at:__ @selection_fail.h:1:8@

    __exported by:__ @selection_fail.h@
-}
data OkBefore = OkBefore
  { okBefore_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @selection_fail.h:2:7@

         __exported by:__ @selection_fail.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable OkBefore where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure OkBefore
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"okBefore_x") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          OkBefore okBefore_x2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"okBefore_x") ptr0 okBefore_x2

instance HsBindgen.Runtime.HasCField.HasCField OkBefore "okBefore_x" where

  type CFieldType OkBefore "okBefore_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType OkBefore) "okBefore_x")
         ) => GHC.Records.HasField "okBefore_x" (Ptr.Ptr OkBefore) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"okBefore_x")
