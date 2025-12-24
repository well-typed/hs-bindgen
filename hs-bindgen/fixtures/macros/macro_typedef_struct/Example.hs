{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.HasCField
import Data.Bits (FiniteBits)
import GHC.Prim ((*#), (+#))
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

{-| __C declaration:__ @MY_TYPE@

    __defined at:__ @macros\/macro_typedef_struct.h:1:9@

    __exported by:__ @macros\/macro_typedef_struct.h@
-}
newtype MY_TYPE = MY_TYPE
  { un_MY_TYPE :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @struct bar@

    __defined at:__ @macros\/macro_typedef_struct.h:3:9@

    __exported by:__ @macros\/macro_typedef_struct.h@
-}
data Bar = Bar
  { bar_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/macro_typedef_struct.h:4:7@

         __exported by:__ @macros\/macro_typedef_struct.h@
    -}
  , bar_y :: MY_TYPE
    {- ^ __C declaration:__ @y@

         __defined at:__ @macros\/macro_typedef_struct.h:5:11@

         __exported by:__ @macros\/macro_typedef_struct.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Bar where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Bar
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"bar_x") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"bar_y") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_x2 bar_y3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"bar_x") ptr0 bar_x2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"bar_y") ptr0 bar_y3

instance Data.Primitive.Types.Prim Bar where

  sizeOf# = \_ -> (8#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Bar (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Bar v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Bar bar_x4 bar_y5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) bar_x4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) bar_y5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Bar (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Bar v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Bar bar_x4 bar_y5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) bar_x4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) bar_y5 s6

instance HsBindgen.Runtime.HasCField.HasCField Bar "bar_x" where

  type CFieldType Bar "bar_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Bar) "bar_x")
         ) => GHC.Records.HasField "bar_x" (Ptr.Ptr Bar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"bar_x")

instance HsBindgen.Runtime.HasCField.HasCField Bar "bar_y" where

  type CFieldType Bar "bar_y" = MY_TYPE

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Bar) "bar_y")
         ) => GHC.Records.HasField "bar_y" (Ptr.Ptr Bar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"bar_y")
