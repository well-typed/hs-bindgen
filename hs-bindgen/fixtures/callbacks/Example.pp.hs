{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Example where

import qualified Data.Array.Byte
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.SizedByteArray
import qualified Text.Read
import Data.Bits (FiniteBits)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, showsPrec)

{-| Auxiliary type used by 'FileOpenedNotification'

__defined at:__ @callbacks.h:10:16@

__exported by:__ @callbacks.h@
-}
newtype FileOpenedNotification_Deref = FileOpenedNotification_Deref
  { un_FileOpenedNotification_Deref :: IO ()
  }

foreign import ccall safe "wrapper" toFileOpenedNotification_Deref ::
     FileOpenedNotification_Deref
  -> IO (Ptr.FunPtr FileOpenedNotification_Deref)

foreign import ccall safe "dynamic" fromFileOpenedNotification_Deref ::
     Ptr.FunPtr FileOpenedNotification_Deref
  -> FileOpenedNotification_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr FileOpenedNotification_Deref where

  toFunPtr = toFileOpenedNotification_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr FileOpenedNotification_Deref where

  fromFunPtr = fromFileOpenedNotification_Deref

{-| __C declaration:__ @FileOpenedNotification@

    __defined at:__ @callbacks.h:10:16@

    __exported by:__ @callbacks.h@
-}
newtype FileOpenedNotification = FileOpenedNotification
  { un_FileOpenedNotification :: Ptr.FunPtr FileOpenedNotification_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'ProgressUpdate'

__defined at:__ @callbacks.h:11:16@

__exported by:__ @callbacks.h@
-}
newtype ProgressUpdate_Deref = ProgressUpdate_Deref
  { un_ProgressUpdate_Deref :: FC.CInt -> IO ()
  }

foreign import ccall safe "wrapper" toProgressUpdate_Deref ::
     ProgressUpdate_Deref
  -> IO (Ptr.FunPtr ProgressUpdate_Deref)

foreign import ccall safe "dynamic" fromProgressUpdate_Deref ::
     Ptr.FunPtr ProgressUpdate_Deref
  -> ProgressUpdate_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr ProgressUpdate_Deref where

  toFunPtr = toProgressUpdate_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr ProgressUpdate_Deref where

  fromFunPtr = fromProgressUpdate_Deref

{-| __C declaration:__ @ProgressUpdate@

    __defined at:__ @callbacks.h:11:16@

    __exported by:__ @callbacks.h@
-}
newtype ProgressUpdate = ProgressUpdate
  { un_ProgressUpdate :: Ptr.FunPtr ProgressUpdate_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'DataValidator'

__defined at:__ @callbacks.h:12:15@

__exported by:__ @callbacks.h@
-}
newtype DataValidator_Deref = DataValidator_Deref
  { un_DataValidator_Deref :: FC.CInt -> IO FC.CInt
  }

foreign import ccall safe "wrapper" toDataValidator_Deref ::
     DataValidator_Deref
  -> IO (Ptr.FunPtr DataValidator_Deref)

foreign import ccall safe "dynamic" fromDataValidator_Deref ::
     Ptr.FunPtr DataValidator_Deref
  -> DataValidator_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr DataValidator_Deref where

  toFunPtr = toDataValidator_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr DataValidator_Deref where

  fromFunPtr = fromDataValidator_Deref

{-| __C declaration:__ @DataValidator@

    __defined at:__ @callbacks.h:12:15@

    __exported by:__ @callbacks.h@
-}
newtype DataValidator = DataValidator
  { un_DataValidator :: Ptr.FunPtr DataValidator_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @Measurement@

    __defined at:__ @callbacks.h:21:8@

    __exported by:__ @callbacks.h@
-}
data Measurement = Measurement
  { measurement_value :: FC.CDouble
    {- ^ __C declaration:__ @value@

         __defined at:__ @callbacks.h:22:10@

         __exported by:__ @callbacks.h@
    -}
  , measurement_timestamp :: FC.CDouble
    {- ^ __C declaration:__ @timestamp@

         __defined at:__ @callbacks.h:23:10@

         __exported by:__ @callbacks.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Measurement where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Measurement
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Measurement measurement_value2 measurement_timestamp3 ->
               F.pokeByteOff ptr0 (0 :: Int) measurement_value2
            >> F.pokeByteOff ptr0 (8 :: Int) measurement_timestamp3

{-| Auxiliary type used by 'MeasurementReceived'

__defined at:__ @callbacks.h:26:16@

__exported by:__ @callbacks.h@
-}
newtype MeasurementReceived_Deref = MeasurementReceived_Deref
  { un_MeasurementReceived_Deref :: (Ptr.Ptr Measurement) -> IO ()
  }

foreign import ccall safe "wrapper" toMeasurementReceived_Deref ::
     MeasurementReceived_Deref
  -> IO (Ptr.FunPtr MeasurementReceived_Deref)

foreign import ccall safe "dynamic" fromMeasurementReceived_Deref ::
     Ptr.FunPtr MeasurementReceived_Deref
  -> MeasurementReceived_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr MeasurementReceived_Deref where

  toFunPtr = toMeasurementReceived_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr MeasurementReceived_Deref where

  fromFunPtr = fromMeasurementReceived_Deref

{-| __C declaration:__ @MeasurementReceived@

    __defined at:__ @callbacks.h:26:16@

    __exported by:__ @callbacks.h@
-}
newtype MeasurementReceived = MeasurementReceived
  { un_MeasurementReceived :: Ptr.FunPtr MeasurementReceived_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'MeasurementReceived2'

__defined at:__ @callbacks.h:29:16@

__exported by:__ @callbacks.h@
-}
newtype MeasurementReceived2_Deref = MeasurementReceived2_Deref
  { un_MeasurementReceived2_Deref :: Measurement -> IO ()
  }

{-| __C declaration:__ @MeasurementReceived2@

    __defined at:__ @callbacks.h:29:16@

    __exported by:__ @callbacks.h@
-}
newtype MeasurementReceived2 = MeasurementReceived2
  { un_MeasurementReceived2 :: Ptr.FunPtr MeasurementReceived2_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'SampleBufferFull'

__defined at:__ @callbacks.h:32:16@

__exported by:__ @callbacks.h@
-}
newtype SampleBufferFull_Deref = SampleBufferFull_Deref
  { un_SampleBufferFull_Deref :: ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) FC.CInt) -> IO ()
  }

{-| __C declaration:__ @SampleBufferFull@

    __defined at:__ @callbacks.h:32:16@

    __exported by:__ @callbacks.h@
-}
newtype SampleBufferFull = SampleBufferFull
  { un_SampleBufferFull :: Ptr.FunPtr SampleBufferFull_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @MeasurementHandler@

    __defined at:__ @callbacks.h:50:8@

    __exported by:__ @callbacks.h@
-}
data MeasurementHandler = MeasurementHandler
  { measurementHandler_onReceived :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ())
    {- ^ __C declaration:__ @onReceived@

         __defined at:__ @callbacks.h:51:10@

         __exported by:__ @callbacks.h@
    -}
  , measurementHandler_validate :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt)
    {- ^ __C declaration:__ @validate@

         __defined at:__ @callbacks.h:52:9@

         __exported by:__ @callbacks.h@
    -}
  , measurementHandler_onError :: Ptr.FunPtr (FC.CInt -> IO ())
    {- ^ __C declaration:__ @onError@

         __defined at:__ @callbacks.h:53:10@

         __exported by:__ @callbacks.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable MeasurementHandler where

  sizeOf = \_ -> (24 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure MeasurementHandler
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MeasurementHandler
            measurementHandler_onReceived2
            measurementHandler_validate3
            measurementHandler_onError4 ->
                 F.pokeByteOff ptr0 (0 :: Int) measurementHandler_onReceived2
              >> F.pokeByteOff ptr0 (8 :: Int) measurementHandler_validate3
              >> F.pokeByteOff ptr0 (16 :: Int) measurementHandler_onError4

{-| __C declaration:__ @DataPipeline@

    __defined at:__ @callbacks.h:58:8@

    __exported by:__ @callbacks.h@
-}
data DataPipeline = DataPipeline
  { dataPipeline_preProcess :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ())
    {- ^ __C declaration:__ @preProcess@

         __defined at:__ @callbacks.h:59:10@

         __exported by:__ @callbacks.h@
    -}
  , dataPipeline_process :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ())
    {- ^ __C declaration:__ @process@

         __defined at:__ @callbacks.h:60:10@

         __exported by:__ @callbacks.h@
    -}
  , dataPipeline_postProcess :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())
    {- ^ __C declaration:__ @postProcess@

         __defined at:__ @callbacks.h:61:10@

         __exported by:__ @callbacks.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable DataPipeline where

  sizeOf = \_ -> (24 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure DataPipeline
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          DataPipeline
            dataPipeline_preProcess2
            dataPipeline_process3
            dataPipeline_postProcess4 ->
                 F.pokeByteOff ptr0 (0 :: Int) dataPipeline_preProcess2
              >> F.pokeByteOff ptr0 (8 :: Int) dataPipeline_process3
              >> F.pokeByteOff ptr0 (16 :: Int) dataPipeline_postProcess4

{-| __C declaration:__ @ProcessorCallback@

    __defined at:__ @callbacks.h:69:7@

    __exported by:__ @callbacks.h@
-}
newtype ProcessorCallback = ProcessorCallback
  { un_ProcessorCallback :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 8) 8 instance F.Storable ProcessorCallback

{-|

  __See:__ 'set_processorCallback_simple'

__C declaration:__ @simple@

__defined at:__ @callbacks.h:70:10@

__exported by:__ @callbacks.h@
-}
get_processorCallback_simple ::
     ProcessorCallback
  -> Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ())
get_processorCallback_simple =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_processorCallback_simple'

-}
set_processorCallback_simple ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ())
  -> ProcessorCallback
set_processorCallback_simple =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  __See:__ 'set_processorCallback_withValidator'

__C declaration:__ @withValidator@

__defined at:__ @callbacks.h:71:10@

__exported by:__ @callbacks.h@
-}
get_processorCallback_withValidator ::
     ProcessorCallback
  -> Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ())
get_processorCallback_withValidator =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_processorCallback_withValidator'

-}
set_processorCallback_withValidator ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ())
  -> ProcessorCallback
set_processorCallback_withValidator =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  __See:__ 'set_processorCallback_withProgress'

__C declaration:__ @withProgress@

__defined at:__ @callbacks.h:72:10@

__exported by:__ @callbacks.h@
-}
get_processorCallback_withProgress ::
     ProcessorCallback
  -> Ptr.FunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())
get_processorCallback_withProgress =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_processorCallback_withProgress'

-}
set_processorCallback_withProgress ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())
  -> ProcessorCallback
set_processorCallback_withProgress =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-| __defined at:__ @callbacks.h:76:3@

    __exported by:__ @callbacks.h@
-}
newtype Processor_mode = Processor_mode
  { un_Processor_mode :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable Processor_mode where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Processor_mode
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Processor_mode un_Processor_mode2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Processor_mode2

instance HsBindgen.Runtime.CEnum.CEnum Processor_mode where

  type CEnumZ Processor_mode = FC.CUInt

  toCEnum = Processor_mode

  fromCEnum = un_Processor_mode

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "MODE_SIMPLE")
                                                     , (1, Data.List.NonEmpty.singleton "MODE_VALIDATED")
                                                     , (2, Data.List.NonEmpty.singleton "MODE_PROGRESS")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Processor_mode"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Processor_mode"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Processor_mode where

  minDeclaredValue = MODE_SIMPLE

  maxDeclaredValue = MODE_PROGRESS

instance Show Processor_mode where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Processor_mode where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @MODE_SIMPLE@

    __defined at:__ @callbacks.h:76:10@

    __exported by:__ @callbacks.h@
-}
pattern MODE_SIMPLE :: Processor_mode
pattern MODE_SIMPLE = Processor_mode 0

{-| __C declaration:__ @MODE_VALIDATED@

    __defined at:__ @callbacks.h:76:23@

    __exported by:__ @callbacks.h@
-}
pattern MODE_VALIDATED :: Processor_mode
pattern MODE_VALIDATED = Processor_mode 1

{-| __C declaration:__ @MODE_PROGRESS@

    __defined at:__ @callbacks.h:76:39@

    __exported by:__ @callbacks.h@
-}
pattern MODE_PROGRESS :: Processor_mode
pattern MODE_PROGRESS = Processor_mode 2

{-| __C declaration:__ @Processor@

    __defined at:__ @callbacks.h:75:8@

    __exported by:__ @callbacks.h@
-}
data Processor = Processor
  { processor_mode :: Processor_mode
    {- ^ __C declaration:__ @mode@

         __defined at:__ @callbacks.h:76:55@

         __exported by:__ @callbacks.h@
    -}
  , processor_callback :: ProcessorCallback
    {- ^ __C declaration:__ @callback@

         __defined at:__ @callbacks.h:77:27@

         __exported by:__ @callbacks.h@
    -}
  }

instance F.Storable Processor where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Processor
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Processor processor_mode2 processor_callback3 ->
               F.pokeByteOff ptr0 (0 :: Int) processor_mode2
            >> F.pokeByteOff ptr0 (8 :: Int) processor_callback3

{-| __C declaration:__ @foo@

    __defined at:__ @callbacks.h:94:13@

    __exported by:__ @callbacks.h@
-}
newtype Foo = Foo
  { un_Foo :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @foo2@

    __defined at:__ @callbacks.h:95:13@

    __exported by:__ @callbacks.h@
-}
newtype Foo2 = Foo2
  { un_Foo2 :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

foreign import ccall safe "wrapper" funPtr_7a7e54ab_to ::
     (Foo -> IO ())
  -> IO (Ptr.FunPtr (Foo -> IO ()))

foreign import ccall safe "dynamic" funPtr_7a7e54ab_from ::
     Ptr.FunPtr (Foo -> IO ())
  -> Foo -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr (Foo -> IO ()) where

  toFunPtr = funPtr_7a7e54ab_to

instance HsBindgen.Runtime.FunPtr.FromFunPtr (Foo -> IO ()) where

  fromFunPtr = funPtr_7a7e54ab_from

foreign import ccall safe "wrapper" funPtr_527712e4_to ::
     ((Ptr.Ptr Measurement) -> IO FC.CInt)
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt))

foreign import ccall safe "dynamic" funPtr_527712e4_from ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt)
  -> (Ptr.Ptr Measurement) -> IO FC.CInt

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt) where

  toFunPtr = funPtr_527712e4_to

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt) where

  fromFunPtr = funPtr_527712e4_from

foreign import ccall safe "wrapper" funPtr_2cc53fd3_to ::
     ((Ptr.Ptr Measurement) -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ()))

foreign import ccall safe "dynamic" funPtr_2cc53fd3_from ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ())
  -> (Ptr.Ptr Measurement) -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> IO ()) where

  toFunPtr = funPtr_2cc53fd3_to

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> IO ()) where

  fromFunPtr = funPtr_2cc53fd3_from

foreign import ccall safe "wrapper" funPtr_6f6352e1_to ::
     ((Ptr.Ptr Measurement) -> DataValidator -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ()))

foreign import ccall safe "dynamic" funPtr_6f6352e1_from ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ())
  -> (Ptr.Ptr Measurement) -> DataValidator -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ()) where

  toFunPtr = funPtr_6f6352e1_to

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ()) where

  fromFunPtr = funPtr_6f6352e1_from

foreign import ccall safe "wrapper" funPtr_065333fd_to ::
     ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ()))

foreign import ccall safe "dynamic" funPtr_065333fd_from ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())
  -> (Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ()) where

  toFunPtr = funPtr_065333fd_to

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ()) where

  fromFunPtr = funPtr_065333fd_from

foreign import ccall safe "wrapper" funPtr_ac6a854e_to ::
     ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ()))

foreign import ccall safe "dynamic" funPtr_ac6a854e_from ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())
  -> (Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ()) where

  toFunPtr = funPtr_ac6a854e_to

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ()) where

  fromFunPtr = funPtr_ac6a854e_from

foreign import ccall safe "wrapper" funPtr_5e6e3f6b_to ::
     ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ()))

foreign import ccall safe "dynamic" funPtr_5e6e3f6b_from ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())
  -> (Ptr.Ptr Measurement) -> ProgressUpdate -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ()) where

  toFunPtr = funPtr_5e6e3f6b_to

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ()) where

  fromFunPtr = funPtr_5e6e3f6b_from

foreign import ccall safe "wrapper" funPtr_8b272628_to ::
     ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ()))

foreign import ccall safe "dynamic" funPtr_8b272628_from ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())
  -> (Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ()) where

  toFunPtr = funPtr_8b272628_to

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ()) where

  fromFunPtr = funPtr_8b272628_from

foreign import ccall safe "wrapper" funPtr_30a248a4_to ::
     ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ()))

foreign import ccall safe "dynamic" funPtr_30a248a4_from ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())
  -> (Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ()) where

  toFunPtr = funPtr_30a248a4_to

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ()) where

  fromFunPtr = funPtr_30a248a4_from

foreign import ccall safe "wrapper" funPtr_a3803a24_to ::
     (Foo2 -> IO ())
  -> IO (Ptr.FunPtr (Foo2 -> IO ()))

foreign import ccall safe "dynamic" funPtr_a3803a24_from ::
     Ptr.FunPtr (Foo2 -> IO ())
  -> Foo2 -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr (Foo2 -> IO ()) where

  toFunPtr = funPtr_a3803a24_to

instance HsBindgen.Runtime.FunPtr.FromFunPtr (Foo2 -> IO ()) where

  fromFunPtr = funPtr_a3803a24_from
