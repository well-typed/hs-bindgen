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
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.IncompleteArray
import qualified M
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (Eq, Show)

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array.h 6:13@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array.h@
-}
newtype A = A
  { unwrapA :: HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt
  }
  deriving stock (Eq, Show)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType A) "unwrapA")
         ) => GHC.Records.HasField "unwrapA" (Ptr.Ptr A) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapA")

instance HsBindgen.Runtime.HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" =
    HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array.h 7:11@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array.h@
-}
newtype B = B
  { unwrapB :: A
  }
  deriving stock (Eq, Show)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType B) "unwrapB")
         ) => GHC.Records.HasField "unwrapB" (Ptr.Ptr B) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapB")

instance HsBindgen.Runtime.HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @E@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array.h 19:11@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array.h@
-}
newtype E = E
  { unwrapE :: M.C
  }

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType E) "unwrapE")
         ) => GHC.Records.HasField "unwrapE" (Ptr.Ptr E) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapE")

instance HsBindgen.Runtime.HasCField.HasCField E "unwrapE" where

  type CFieldType E "unwrapE" = M.C

  offset# = \_ -> \_ -> 0
