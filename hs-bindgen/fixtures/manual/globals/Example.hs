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
import qualified HsBindgen.Runtime.Bitfield
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.IncompleteArray
import Data.Bits (FiniteBits)
import GHC.Exts ((*#), (+#))
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

{-| __C declaration:__ @struct globalConfig@

    __defined at:__ @manual\/globals.h 7:9@

    __exported by:__ @manual\/globals.h@
-}
data GlobalConfig = GlobalConfig
  { globalConfig_numThreads :: FC.CInt
    {- ^ __C declaration:__ @numThreads@

         __defined at:__ @manual\/globals.h 8:7@

         __exported by:__ @manual\/globals.h@
    -}
  , globalConfig_numWorkers :: FC.CInt
    {- ^ __C declaration:__ @numWorkers@

         __defined at:__ @manual\/globals.h 9:7@

         __exported by:__ @manual\/globals.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable GlobalConfig where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure GlobalConfig
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"globalConfig_numThreads") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"globalConfig_numWorkers") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          GlobalConfig globalConfig_numThreads2 globalConfig_numWorkers3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"globalConfig_numThreads") ptr0 globalConfig_numThreads2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"globalConfig_numWorkers") ptr0 globalConfig_numWorkers3

instance Data.Primitive.Types.Prim GlobalConfig where

  sizeOf# = \_ -> (8#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        GlobalConfig (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, GlobalConfig v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              GlobalConfig globalConfig_numThreads4 globalConfig_numWorkers5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) globalConfig_numThreads4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) globalConfig_numWorkers5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        GlobalConfig (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, GlobalConfig v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              GlobalConfig globalConfig_numThreads4 globalConfig_numWorkers5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) globalConfig_numThreads4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) globalConfig_numWorkers5 s6

instance HsBindgen.Runtime.HasCField.HasCField GlobalConfig "globalConfig_numThreads" where

  type CFieldType GlobalConfig "globalConfig_numThreads" =
    FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType GlobalConfig) "globalConfig_numThreads")
         ) => GHC.Records.HasField "globalConfig_numThreads" (Ptr.Ptr GlobalConfig) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"globalConfig_numThreads")

instance HsBindgen.Runtime.HasCField.HasCField GlobalConfig "globalConfig_numWorkers" where

  type CFieldType GlobalConfig "globalConfig_numWorkers" =
    FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType GlobalConfig) "globalConfig_numWorkers")
         ) => GHC.Records.HasField "globalConfig_numWorkers" (Ptr.Ptr GlobalConfig) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"globalConfig_numWorkers")

{-| __C declaration:__ @ConstInt@

    __defined at:__ @manual\/globals.h 40:19@

    __exported by:__ @manual\/globals.h@
-}
newtype ConstInt = ConstInt
  { un_ConstInt :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType, Data.Primitive.Types.Prim, HsBindgen.Runtime.Bitfield.Bitfield, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType ConstInt) "un_ConstInt")
         ) => GHC.Records.HasField "un_ConstInt" (Ptr.Ptr ConstInt) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_ConstInt")

instance HsBindgen.Runtime.HasCField.HasCField ConstInt "un_ConstInt" where

  type CFieldType ConstInt "un_ConstInt" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct tuple@

    __defined at:__ @manual\/globals.h 52:8@

    __exported by:__ @manual\/globals.h@
-}
data Tuple = Tuple
  { tuple_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @manual\/globals.h 52:20@

         __exported by:__ @manual\/globals.h@
    -}
  , tuple_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @manual\/globals.h 52:33@

         __exported by:__ @manual\/globals.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Tuple where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Tuple
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"tuple_x") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"tuple_y") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Tuple tuple_x2 tuple_y3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"tuple_x") ptr0 tuple_x2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"tuple_y") ptr0 tuple_y3

instance Data.Primitive.Types.Prim Tuple where

  sizeOf# = \_ -> (8#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Tuple (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Tuple v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Tuple tuple_x4 tuple_y5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) tuple_x4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) tuple_y5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Tuple (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Tuple v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Tuple tuple_x4 tuple_y5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) tuple_x4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) tuple_y5 s6

instance HsBindgen.Runtime.HasCField.HasCField Tuple "tuple_x" where

  type CFieldType Tuple "tuple_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Tuple) "tuple_x")
         ) => GHC.Records.HasField "tuple_x" (Ptr.Ptr Tuple) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"tuple_x")

instance HsBindgen.Runtime.HasCField.HasCField Tuple "tuple_y" where

  type CFieldType Tuple "tuple_y" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Tuple) "tuple_y")
         ) => GHC.Records.HasField "tuple_y" (Ptr.Ptr Tuple) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"tuple_y")

{-| __C declaration:__ @triplet@

    __defined at:__ @manual\/globals.h 81:13@

    __exported by:__ @manual\/globals.h@
-}
newtype Triplet = Triplet
  { un_Triplet :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Triplet) "un_Triplet")
         ) => GHC.Records.HasField "un_Triplet" (Ptr.Ptr Triplet) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Triplet")

instance HsBindgen.Runtime.HasCField.HasCField Triplet "un_Triplet" where

  type CFieldType Triplet "un_Triplet" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @list@

    __defined at:__ @manual\/globals.h 85:13@

    __exported by:__ @manual\/globals.h@
-}
newtype List = List
  { un_List :: HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt
  }
  deriving stock (Eq, Show)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType List) "un_List")
         ) => GHC.Records.HasField "un_List" (Ptr.Ptr List) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_List")

instance HsBindgen.Runtime.HasCField.HasCField List "un_List" where

  type CFieldType List "un_List" =
    HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt

  offset# = \_ -> \_ -> 0
