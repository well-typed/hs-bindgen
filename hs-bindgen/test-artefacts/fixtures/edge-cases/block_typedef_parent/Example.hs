{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.B_Aux(..)
    , Example.B(..)
    , Example.S(..)
    )
  where

import qualified HsBindgen.Runtime.Block as Block
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude (IO, type (~))

{-| Auxiliary type used by 'B'

    __C declaration:__ @B@

    __defined at:__ @edge-cases\/block_typedef_parent.h 6:16@

    __exported by:__ @edge-cases\/block_typedef_parent.h@
-}
newtype B_Aux = B_Aux
  { unwrapB_Aux :: S -> IO ()
  }
  deriving stock (BG.Generic)

instance ( ty ~ (S -> IO ())
         ) => BG.CompatHasField.HasField "unwrapB_Aux" B_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         B_Aux {unwrapB_Aux = y1}, BG.getField @"unwrapB_Aux" x0)

instance ( ty ~ (S -> IO ())
         ) => BG.HasField "unwrapB_Aux" (BG.Ptr B_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapB_Aux")

instance HasCField.HasCField B_Aux "unwrapB_Aux" where

  type CFieldType B_Aux "unwrapB_Aux" = S -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B@

    __defined at:__ @edge-cases\/block_typedef_parent.h 6:16@

    __exported by:__ @edge-cases\/block_typedef_parent.h@
-}
newtype B = B
  { unwrapB :: Block.Block B_Aux
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

instance ( ty ~ Block.Block B_Aux
         ) => BG.CompatHasField.HasField "unwrapB" B ty where

  hasField =
    \x0 ->
      (\y1 -> B {unwrapB = y1}, BG.getField @"unwrapB" x0)

instance ( ty ~ Block.Block B_Aux
         ) => BG.HasField "unwrapB" (BG.Ptr B) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapB")

instance HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = Block.Block B_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct S@

    __defined at:__ @block_typedef_child.h 1:8@

    __exported by:__ @edge-cases\/block_typedef_parent.h@
-}
data S = S
  { s_b :: B
    {- ^ __C declaration:__ @b@

         __defined at:__ @block_typedef_child.h 1:14@

         __exported by:__ @edge-cases\/block_typedef_parent.h@
    -}
  }
  deriving stock (BG.Generic)

{-| __C declaration:__ @b@

    __defined at:__ @block_typedef_child.h 1:14@

    __exported by:__ @edge-cases\/block_typedef_parent.h@
-}
instance (ty ~ B) => BG.CompatHasField.HasField "s_b" S ty where

  hasField =
    \x0 -> (\y1 -> S {s_b = y1}, BG.getField @"s_b" x0)

instance (ty ~ B) => BG.HasField "s_b" (BG.Ptr S) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s_b")

instance HasCField.HasCField S "s_b" where

  type CFieldType S "s_b" = B

  offset# = \_ -> \_ -> 0
