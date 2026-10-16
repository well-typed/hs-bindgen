{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data FileOpenedNotification_Aux

instance BG.Generic FileOpenedNotification_Aux

instance BG.ToFunPtr FileOpenedNotification_Aux

instance BG.FromFunPtr FileOpenedNotification_Aux

data FileOpenedNotification

instance Eq FileOpenedNotification

instance BG.Generic FileOpenedNotification

instance Ord FileOpenedNotification

instance Show FileOpenedNotification

instance BG.HasFFIType FileOpenedNotification

instance Marshal.ReadRaw FileOpenedNotification

instance Marshal.StaticSize FileOpenedNotification

instance BG.Storable FileOpenedNotification

instance Marshal.WriteRaw FileOpenedNotification

data ProgressUpdate_Aux

instance BG.Generic ProgressUpdate_Aux

instance BG.ToFunPtr ProgressUpdate_Aux

instance BG.FromFunPtr ProgressUpdate_Aux

data ProgressUpdate

instance Eq ProgressUpdate

instance BG.Generic ProgressUpdate

instance Ord ProgressUpdate

instance Show ProgressUpdate

instance BG.HasFFIType ProgressUpdate

instance Marshal.ReadRaw ProgressUpdate

instance Marshal.StaticSize ProgressUpdate

instance BG.Storable ProgressUpdate

instance Marshal.WriteRaw ProgressUpdate

data DataValidator_Aux

instance BG.Generic DataValidator_Aux

instance BG.ToFunPtr DataValidator_Aux

instance BG.FromFunPtr DataValidator_Aux

data DataValidator

instance Eq DataValidator

instance BG.Generic DataValidator

instance Ord DataValidator

instance Show DataValidator

instance BG.HasFFIType DataValidator

instance Marshal.ReadRaw DataValidator

instance Marshal.StaticSize DataValidator

instance BG.Storable DataValidator

instance Marshal.WriteRaw DataValidator

data Measurement

instance Eq Measurement

instance BG.Generic Measurement

instance Show Measurement

instance Marshal.StaticSize Measurement

instance Marshal.ReadRaw Measurement

instance Marshal.WriteRaw Measurement

instance BG.Storable Measurement

instance Struct.IsStruct Measurement

data MeasurementReceived_Aux

instance BG.Generic MeasurementReceived_Aux

instance BG.ToFunPtr MeasurementReceived_Aux

instance BG.FromFunPtr MeasurementReceived_Aux

data MeasurementReceived

instance Eq MeasurementReceived

instance BG.Generic MeasurementReceived

instance Ord MeasurementReceived

instance Show MeasurementReceived

instance BG.HasFFIType MeasurementReceived

instance Marshal.ReadRaw MeasurementReceived

instance Marshal.StaticSize MeasurementReceived

instance BG.Storable MeasurementReceived

instance Marshal.WriteRaw MeasurementReceived

data MeasurementReceived2_Aux

instance BG.Generic MeasurementReceived2_Aux

data MeasurementReceived2

instance Eq MeasurementReceived2

instance BG.Generic MeasurementReceived2

instance Ord MeasurementReceived2

instance Show MeasurementReceived2

instance BG.HasFFIType MeasurementReceived2

instance Marshal.ReadRaw MeasurementReceived2

instance Marshal.StaticSize MeasurementReceived2

instance BG.Storable MeasurementReceived2

instance Marshal.WriteRaw MeasurementReceived2

data SampleBufferFull_Aux

instance BG.Generic SampleBufferFull_Aux

instance BG.ToFunPtr SampleBufferFull_Aux

instance BG.FromFunPtr SampleBufferFull_Aux

data SampleBufferFull

instance Eq SampleBufferFull

instance BG.Generic SampleBufferFull

instance Ord SampleBufferFull

instance Show SampleBufferFull

instance BG.HasFFIType SampleBufferFull

instance Marshal.ReadRaw SampleBufferFull

instance Marshal.StaticSize SampleBufferFull

instance BG.Storable SampleBufferFull

instance Marshal.WriteRaw SampleBufferFull

data MeasurementHandler

instance Eq MeasurementHandler

instance BG.Generic MeasurementHandler

instance Show MeasurementHandler

instance Marshal.StaticSize MeasurementHandler

instance Marshal.ReadRaw MeasurementHandler

instance Marshal.WriteRaw MeasurementHandler

instance BG.Storable MeasurementHandler

instance Struct.IsStruct MeasurementHandler

data DataPipeline

instance Eq DataPipeline

instance BG.Generic DataPipeline

instance Show DataPipeline

instance Marshal.StaticSize DataPipeline

instance Marshal.ReadRaw DataPipeline

instance Marshal.WriteRaw DataPipeline

instance BG.Storable DataPipeline

instance Struct.IsStruct DataPipeline

data ProcessorCallback

instance BG.Generic ProcessorCallback

instance Marshal.StaticSize ProcessorCallback

instance Marshal.ReadRaw ProcessorCallback

instance Marshal.WriteRaw ProcessorCallback

instance BG.Storable ProcessorCallback

instance Union.IsUnion ProcessorCallback

data Processor_mode

instance Eq Processor_mode

instance BG.Generic Processor_mode

instance Ord Processor_mode

instance BG.HasFFIType Processor_mode

instance Marshal.StaticSize Processor_mode

instance Marshal.ReadRaw Processor_mode

instance Marshal.WriteRaw Processor_mode

instance BG.Storable Processor_mode

instance BG.Prim Processor_mode

instance CEnum.CEnum Processor_mode

instance CEnum.SequentialCEnum Processor_mode

instance Show Processor_mode

instance Read Processor_mode

data Processor

instance BG.Generic Processor

instance Marshal.StaticSize Processor

instance Marshal.ReadRaw Processor

instance Marshal.WriteRaw Processor

instance BG.Storable Processor

instance Struct.IsStruct Processor

data Foo

instance Eq Foo

instance BG.Generic Foo

instance Ord Foo

instance Read Foo

instance Show Foo

instance BG.Bitfield Foo

instance BG.Bits Foo

instance Bounded Foo

instance Enum Foo

instance BG.FiniteBits Foo

instance BG.HasFFIType Foo

instance Integral Foo

instance BG.Ix Foo

instance Num Foo

instance BG.Prim Foo

instance Marshal.ReadRaw Foo

instance Real Foo

instance Marshal.StaticSize Foo

instance BG.Storable Foo

instance Marshal.WriteRaw Foo

data Foo2

instance Eq Foo2

instance BG.Generic Foo2

instance Ord Foo2

instance Read Foo2

instance Show Foo2

instance BG.Bitfield Foo2

instance BG.Bits Foo2

instance Bounded Foo2

instance Enum Foo2

instance BG.FiniteBits Foo2

instance BG.HasFFIType Foo2

instance Integral Foo2

instance BG.Ix Foo2

instance Num Foo2

instance BG.Prim Foo2

instance Marshal.ReadRaw Foo2

instance Real Foo2

instance Marshal.StaticSize Foo2

instance BG.Storable Foo2

instance Marshal.WriteRaw Foo2

data A

instance Eq A

instance BG.Generic A

instance Ord A

instance Read A

instance Show A

instance BG.Bitfield A

instance BG.Bits A

instance Bounded A

instance Enum A

instance BG.FiniteBits A

instance BG.HasFFIType A

instance Integral A

instance BG.Ix A

instance Num A

instance BG.Prim A

instance Marshal.ReadRaw A

instance Real A

instance Marshal.StaticSize A

instance BG.Storable A

instance Marshal.WriteRaw A

data B

instance Eq B

instance BG.Generic B

instance Ord B

instance Read B

instance Show B

instance BG.Bitfield B

instance BG.Bits B

instance Bounded B

instance Enum B

instance BG.FiniteBits B

instance BG.HasFFIType B

instance Integral B

instance BG.Ix B

instance Num B

instance BG.Prim B

instance Marshal.ReadRaw B

instance Real B

instance Marshal.StaticSize B

instance BG.Storable B

instance Marshal.WriteRaw B

data S

instance Eq S

instance BG.Generic S

instance Show S

instance Marshal.StaticSize S

instance Marshal.ReadRaw S

instance Marshal.WriteRaw S

instance BG.Storable S

instance Struct.IsStruct S

data C

instance Eq C

instance BG.Generic C

instance Ord C

instance Read C

instance Show C

instance BG.Bitfield C

instance BG.Bits C

instance Bounded C

instance Enum C

instance BG.FiniteBits C

instance BG.HasFFIType C

instance Integral C

instance BG.Ix C

instance Num C

instance BG.Prim C

instance Marshal.ReadRaw C

instance Real C

instance Marshal.StaticSize C

instance BG.Storable C

instance Marshal.WriteRaw C

data U

instance BG.Generic U

instance Marshal.StaticSize U

instance Marshal.ReadRaw U

instance Marshal.WriteRaw U

instance BG.Storable U

instance Union.IsUnion U

data D

instance Eq D

instance BG.Generic D

instance Ord D

instance Read D

instance Show D

instance BG.Bitfield D

instance BG.Bits D

instance Bounded D

instance Enum D

instance BG.FiniteBits D

instance BG.HasFFIType D

instance Integral D

instance BG.Ix D

instance Num D

instance BG.Prim D

instance Marshal.ReadRaw D

instance Real D

instance Marshal.StaticSize D

instance BG.Storable D

instance Marshal.WriteRaw D

data T

instance BG.Generic T

instance BG.ToFunPtr T

instance BG.FromFunPtr T
