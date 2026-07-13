{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.MyArray(..)
    , Example.A(..)
    , Example.B(..)
    , Example.E(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified M

{-| __C declaration:__ @MyArray@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 5:13@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
newtype MyArray = MyArray
  { unwrapMyArray :: IA.IncompleteArray BG.CInt
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype (IsA.IsArray)

instance ( ty ~ IA.IncompleteArray BG.CInt
         ) => BG.CompatHasField.HasField "unwrapMyArray" MyArray ty where

  hasField =
    \x0 ->
      (\y1 ->
         MyArray {unwrapMyArray = y1}, BG.getField @"unwrapMyArray" x0)

instance ( ty ~ IA.IncompleteArray BG.CInt
         ) => BG.HasField "unwrapMyArray" (BG.Ptr MyArray) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMyArray")

instance HasCField.HasCField MyArray "unwrapMyArray" where

  type CFieldType MyArray "unwrapMyArray" =
    IA.IncompleteArray BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro A@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 9:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
newtype A = A
  { unwrapA :: MyArray
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype (IsA.IsArray)

instance (ty ~ MyArray) => BG.CompatHasField.HasField "unwrapA" A ty where

  hasField =
    \x0 ->
      (\y1 -> A {unwrapA = y1}, BG.getField @"unwrapA" x0)

instance (ty ~ MyArray) => BG.HasField "unwrapA" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = MyArray

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro B@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 10:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
newtype B = B
  { unwrapB :: A
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype (IsA.IsArray)

instance (ty ~ A) => BG.CompatHasField.HasField "unwrapB" B ty where

  hasField =
    \x0 ->
      (\y1 -> B {unwrapB = y1}, BG.getField @"unwrapB" x0)

instance (ty ~ A) => BG.HasField "unwrapB" (BG.Ptr B) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapB")

instance HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro E@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 31:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
newtype E = E
  { unwrapE :: M.C
  }
  deriving stock (BG.Generic)

instance (ty ~ M.C) => BG.CompatHasField.HasField "unwrapE" E ty where

  hasField =
    \x0 ->
      (\y1 -> E {unwrapE = y1}, BG.getField @"unwrapE" x0)

instance (ty ~ M.C) => BG.HasField "unwrapE" (BG.Ptr E) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapE")

instance HasCField.HasCField E "unwrapE" where

  type CFieldType E "unwrapE" = M.C

  offset# = \_ -> \_ -> 0
