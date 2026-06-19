{-# LANGUAGE EmptyDataDecls #-}

module Example
    ( Example.Point2d
    , Example.Value
    , Example.Color
    , Example.TdStruct
    , Example.TdChain
    , Example.TdUnion
    , Example.Counter
    , Example.OpaqueFwd
    , Example.MacroPtr
    )
  where

import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct point2d@

    __defined at:__ @binding-specs\/rep\/emptydata\/staticsize.h 9:8@

    __exported by:__ @binding-specs\/rep\/emptydata\/staticsize.h@
-}
data Point2d

instance Marshal.StaticSize Point2d where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

{-| __C declaration:__ @union value@

    __defined at:__ @binding-specs\/rep\/emptydata\/staticsize.h 11:7@

    __exported by:__ @binding-specs\/rep\/emptydata\/staticsize.h@
-}
data Value

instance Marshal.StaticSize Value where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

{-| __C declaration:__ @enum color@

    __defined at:__ @binding-specs\/rep\/emptydata\/staticsize.h 13:6@

    __exported by:__ @binding-specs\/rep\/emptydata\/staticsize.h@
-}
data Color

instance Marshal.StaticSize Color where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

{-| __C declaration:__ @td_struct@

    __defined at:__ @binding-specs\/rep\/emptydata\/staticsize.h 17:21@

    __exported by:__ @binding-specs\/rep\/emptydata\/staticsize.h@
-}
data TdStruct

instance Marshal.StaticSize TdStruct where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

{-| __C declaration:__ @td_chain@

    __defined at:__ @binding-specs\/rep\/emptydata\/staticsize.h 18:21@

    __exported by:__ @binding-specs\/rep\/emptydata\/staticsize.h@
-}
data TdChain

instance Marshal.StaticSize TdChain where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

{-| __C declaration:__ @td_union@

    __defined at:__ @binding-specs\/rep\/emptydata\/staticsize.h 19:21@

    __exported by:__ @binding-specs\/rep\/emptydata\/staticsize.h@
-}
data TdUnion

instance Marshal.StaticSize TdUnion where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

{-| __C declaration:__ @counter@

    __defined at:__ @binding-specs\/rep\/emptydata\/staticsize.h 23:13@

    __exported by:__ @binding-specs\/rep\/emptydata\/staticsize.h@
-}
data Counter

{-| __C declaration:__ @struct opaque_fwd@

    __defined at:__ @binding-specs\/rep\/emptydata\/staticsize.h 25:8@

    __exported by:__ @binding-specs\/rep\/emptydata\/staticsize.h@
-}
data OpaqueFwd

{-| __C declaration:__ @macro macro_ptr@

    __defined at:__ @binding-specs\/rep\/emptydata\/staticsize.h 27:9@

    __exported by:__ @binding-specs\/rep\/emptydata\/staticsize.h@
-}
data MacroPtr
