{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

module M.Foo
    ( M.Foo.Foo
    )
  where

import qualified HsBindgen.Runtime.Marshal as Marshal
import Prelude (Int)

{-| __C declaration:__ @struct foo@

    __defined at:__ @foo.h 9:8@

    __exported by:__ @forward_typedef\/root.h@
-}
data Foo

instance Marshal.StaticSize Foo where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)
