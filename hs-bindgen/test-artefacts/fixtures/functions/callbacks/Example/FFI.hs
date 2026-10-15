{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Example
import Prelude (IO, fmap)

-- __unique:__ @toFileOpenedNotification_Aux@
foreign import ccall safe "wrapper" hs_bindgen_b3b8b1fad168671a_base ::
     IO ()
  -> IO (BG.FunPtr (IO ()))

-- __unique:__ @toFileOpenedNotification_Aux@
hs_bindgen_b3b8b1fad168671a ::
     FileOpenedNotification_Aux
  -> IO (BG.FunPtr FileOpenedNotification_Aux)
hs_bindgen_b3b8b1fad168671a =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_b3b8b1fad168671a_base (BG.getField @"unwrapFileOpenedNotification_Aux" fun0))

-- __unique:__ @fromFileOpenedNotification_Aux@
foreign import ccall safe "dynamic" hs_bindgen_f3ba5920f34c7f6a_base ::
     BG.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromFileOpenedNotification_Aux@
hs_bindgen_f3ba5920f34c7f6a ::
     BG.FunPtr FileOpenedNotification_Aux
  -> FileOpenedNotification_Aux
hs_bindgen_f3ba5920f34c7f6a =
  \funPtr0 ->
    FileOpenedNotification_Aux (hs_bindgen_f3ba5920f34c7f6a_base (BG.castFunPtr funPtr0))

-- __unique:__ @toProgressUpdate_Aux@
foreign import ccall safe "wrapper" hs_bindgen_d551f31556ffa727_base ::
     (BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.CInt -> IO ()))

-- __unique:__ @toProgressUpdate_Aux@
hs_bindgen_d551f31556ffa727 ::
     ProgressUpdate_Aux
  -> IO (BG.FunPtr ProgressUpdate_Aux)
hs_bindgen_d551f31556ffa727 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_d551f31556ffa727_base (\x1 ->
                                                            BG.getField @"unwrapProgressUpdate_Aux" fun0 (BG.fromFFIType x1)))

-- __unique:__ @fromProgressUpdate_Aux@
foreign import ccall safe "dynamic" hs_bindgen_ccf7f4b62a839a04_base ::
     BG.FunPtr (BG.CInt -> IO ())
  -> BG.CInt -> IO ()

-- __unique:__ @fromProgressUpdate_Aux@
hs_bindgen_ccf7f4b62a839a04 ::
     BG.FunPtr ProgressUpdate_Aux
  -> ProgressUpdate_Aux
hs_bindgen_ccf7f4b62a839a04 =
  \funPtr0 ->
    ProgressUpdate_Aux (\x1 ->
                          hs_bindgen_ccf7f4b62a839a04_base (BG.castFunPtr funPtr0) (BG.toFFIType x1))

-- __unique:__ @toDataValidator_Aux@
foreign import ccall safe "wrapper" hs_bindgen_c656ca21e63343d6_base ::
     (BG.CInt -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.CInt -> IO BG.CInt))

-- __unique:__ @toDataValidator_Aux@
hs_bindgen_c656ca21e63343d6 ::
     DataValidator_Aux
  -> IO (BG.FunPtr DataValidator_Aux)
hs_bindgen_c656ca21e63343d6 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_c656ca21e63343d6_base (\x1 ->
                                                            fmap BG.toFFIType (BG.getField @"unwrapDataValidator_Aux" fun0 (BG.fromFFIType x1))))

-- __unique:__ @fromDataValidator_Aux@
foreign import ccall safe "dynamic" hs_bindgen_c1e79a4c11ca4033_base ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> BG.CInt -> IO BG.CInt

-- __unique:__ @fromDataValidator_Aux@
hs_bindgen_c1e79a4c11ca4033 ::
     BG.FunPtr DataValidator_Aux
  -> DataValidator_Aux
hs_bindgen_c1e79a4c11ca4033 =
  \funPtr0 ->
    DataValidator_Aux (\x1 ->
                         fmap BG.fromFFIType (hs_bindgen_c1e79a4c11ca4033_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)))

-- __unique:__ @toMeasurementReceived_Aux@
foreign import ccall safe "wrapper" hs_bindgen_9259654df9d40f5b_base ::
     (BG.Ptr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> IO ()))

-- __unique:__ @toMeasurementReceived_Aux@
hs_bindgen_9259654df9d40f5b ::
     MeasurementReceived_Aux
  -> IO (BG.FunPtr MeasurementReceived_Aux)
hs_bindgen_9259654df9d40f5b =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_9259654df9d40f5b_base (\x1 ->
                                                            BG.getField @"unwrapMeasurementReceived_Aux" fun0 (BG.fromFFIType x1)))

-- __unique:__ @fromMeasurementReceived_Aux@
foreign import ccall safe "dynamic" hs_bindgen_383c36bb22947621_base ::
     BG.FunPtr (BG.Ptr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> IO ()

-- __unique:__ @fromMeasurementReceived_Aux@
hs_bindgen_383c36bb22947621 ::
     BG.FunPtr MeasurementReceived_Aux
  -> MeasurementReceived_Aux
hs_bindgen_383c36bb22947621 =
  \funPtr0 ->
    MeasurementReceived_Aux (\x1 ->
                               hs_bindgen_383c36bb22947621_base (BG.castFunPtr funPtr0) (BG.toFFIType x1))

-- __unique:__ @toSampleBufferFull_Aux@
foreign import ccall safe "wrapper" hs_bindgen_57d9e30494ae4453_base ::
     (BG.Ptr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> IO ()))

-- __unique:__ @toSampleBufferFull_Aux@
hs_bindgen_57d9e30494ae4453 ::
     SampleBufferFull_Aux
  -> IO (BG.FunPtr SampleBufferFull_Aux)
hs_bindgen_57d9e30494ae4453 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_57d9e30494ae4453_base (\x1 ->
                                                            BG.getField @"unwrapSampleBufferFull_Aux" fun0 (BG.fromFFIType x1)))

-- __unique:__ @fromSampleBufferFull_Aux@
foreign import ccall safe "dynamic" hs_bindgen_2ab7ac6bb756ba7e_base ::
     BG.FunPtr (BG.Ptr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> IO ()

-- __unique:__ @fromSampleBufferFull_Aux@
hs_bindgen_2ab7ac6bb756ba7e ::
     BG.FunPtr SampleBufferFull_Aux
  -> SampleBufferFull_Aux
hs_bindgen_2ab7ac6bb756ba7e =
  \funPtr0 ->
    SampleBufferFull_Aux (\x1 ->
                            hs_bindgen_2ab7ac6bb756ba7e_base (BG.castFunPtr funPtr0) (BG.toFFIType x1))

-- __unique:__ @toT@
foreign import ccall safe "wrapper" hs_bindgen_b8534912f6256492_base ::
     (BG.FunPtr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.FunPtr BG.Void -> IO ()))

-- __unique:__ @toT@
hs_bindgen_b8534912f6256492 ::
     T
  -> IO (BG.FunPtr T)
hs_bindgen_b8534912f6256492 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_b8534912f6256492_base (\x1 ->
                                                            BG.getField @"unwrapT" fun0 (BG.fromFFIType x1)))

-- __unique:__ @fromT@
foreign import ccall safe "dynamic" hs_bindgen_8f830eadb43a6fe4_base ::
     BG.FunPtr (BG.FunPtr BG.Void -> IO ())
  -> BG.FunPtr BG.Void -> IO ()

-- __unique:__ @fromT@
hs_bindgen_8f830eadb43a6fe4 ::
     BG.FunPtr T
  -> T
hs_bindgen_8f830eadb43a6fe4 =
  \funPtr0 ->
    T (\x1 ->
         hs_bindgen_8f830eadb43a6fe4_base (BG.castFunPtr funPtr0) (BG.toFFIType x1))

-- __unique:__ @instance ToFunPtr (A -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_a46c670f88b5e6d2_base ::
     (BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.CInt -> IO ()))

-- __unique:__ @instance ToFunPtr (A -> IO ())@
hs_bindgen_a46c670f88b5e6d2 ::
     (A -> IO ())
  -> IO (BG.FunPtr (A -> IO ()))
hs_bindgen_a46c670f88b5e6d2 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_a46c670f88b5e6d2_base (\x1 ->
                                                            fun0 (BG.fromFFIType x1)))

-- __unique:__ @instance FromFunPtr (A -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_542fc348d1df7dff_base ::
     BG.FunPtr (BG.CInt -> IO ())
  -> BG.CInt -> IO ()

-- __unique:__ @instance FromFunPtr (A -> IO ())@
hs_bindgen_542fc348d1df7dff ::
     BG.FunPtr (A -> IO ())
  -> A -> IO ()
hs_bindgen_542fc348d1df7dff =
  \funPtr0 ->
    \x1 ->
      hs_bindgen_542fc348d1df7dff_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)

-- __unique:__ @instance ToFunPtr (B -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_ca9230f035bf19b1_base ::
     (BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.CInt -> IO ()))

-- __unique:__ @instance ToFunPtr (B -> IO ())@
hs_bindgen_ca9230f035bf19b1 ::
     (B -> IO ())
  -> IO (BG.FunPtr (B -> IO ()))
hs_bindgen_ca9230f035bf19b1 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_ca9230f035bf19b1_base (\x1 ->
                                                            fun0 (BG.fromFFIType x1)))

-- __unique:__ @instance FromFunPtr (B -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_05635addcbfd7dd4_base ::
     BG.FunPtr (BG.CInt -> IO ())
  -> BG.CInt -> IO ()

-- __unique:__ @instance FromFunPtr (B -> IO ())@
hs_bindgen_05635addcbfd7dd4 ::
     BG.FunPtr (B -> IO ())
  -> B -> IO ()
hs_bindgen_05635addcbfd7dd4 =
  \funPtr0 ->
    \x1 ->
      hs_bindgen_05635addcbfd7dd4_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)

-- __unique:__ @instance ToFunPtr (C -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_151c4eb8866d683f_base ::
     (BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.CInt -> IO ()))

-- __unique:__ @instance ToFunPtr (C -> IO ())@
hs_bindgen_151c4eb8866d683f ::
     (C -> IO ())
  -> IO (BG.FunPtr (C -> IO ()))
hs_bindgen_151c4eb8866d683f =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_151c4eb8866d683f_base (\x1 ->
                                                            fun0 (BG.fromFFIType x1)))

-- __unique:__ @instance FromFunPtr (C -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_f9f9dcb4f8267450_base ::
     BG.FunPtr (BG.CInt -> IO ())
  -> BG.CInt -> IO ()

-- __unique:__ @instance FromFunPtr (C -> IO ())@
hs_bindgen_f9f9dcb4f8267450 ::
     BG.FunPtr (C -> IO ())
  -> C -> IO ()
hs_bindgen_f9f9dcb4f8267450 =
  \funPtr0 ->
    \x1 ->
      hs_bindgen_f9f9dcb4f8267450_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)

-- __unique:__ @instance ToFunPtr (D -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_bb8d99987b90516d_base ::
     (BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.CInt -> IO ()))

-- __unique:__ @instance ToFunPtr (D -> IO ())@
hs_bindgen_bb8d99987b90516d ::
     (D -> IO ())
  -> IO (BG.FunPtr (D -> IO ()))
hs_bindgen_bb8d99987b90516d =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_bb8d99987b90516d_base (\x1 ->
                                                            fun0 (BG.fromFFIType x1)))

-- __unique:__ @instance FromFunPtr (D -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_8eef50c371b3a153_base ::
     BG.FunPtr (BG.CInt -> IO ())
  -> BG.CInt -> IO ()

-- __unique:__ @instance FromFunPtr (D -> IO ())@
hs_bindgen_8eef50c371b3a153 ::
     BG.FunPtr (D -> IO ())
  -> D -> IO ()
hs_bindgen_8eef50c371b3a153 =
  \funPtr0 ->
    \x1 ->
      hs_bindgen_8eef50c371b3a153_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)

-- __unique:__ @instance ToFunPtr (Foo -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_d2a71f330b782e41_base ::
     (BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.CInt -> IO ()))

-- __unique:__ @instance ToFunPtr (Foo -> IO ())@
hs_bindgen_d2a71f330b782e41 ::
     (Foo -> IO ())
  -> IO (BG.FunPtr (Foo -> IO ()))
hs_bindgen_d2a71f330b782e41 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_d2a71f330b782e41_base (\x1 ->
                                                            fun0 (BG.fromFFIType x1)))

-- __unique:__ @instance FromFunPtr (Foo -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_c08440542d338bad_base ::
     BG.FunPtr (BG.CInt -> IO ())
  -> BG.CInt -> IO ()

-- __unique:__ @instance FromFunPtr (Foo -> IO ())@
hs_bindgen_c08440542d338bad ::
     BG.FunPtr (Foo -> IO ())
  -> Foo -> IO ()
hs_bindgen_c08440542d338bad =
  \funPtr0 ->
    \x1 ->
      hs_bindgen_c08440542d338bad_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> IO BG.CInt)@
foreign import ccall safe "wrapper" hs_bindgen_4064610c89a52f99_base ::
     (BG.Ptr BG.Void -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> IO BG.CInt))

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> IO BG.CInt)@
hs_bindgen_4064610c89a52f99 ::
     (BG.Ptr Measurement -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.Ptr Measurement -> IO BG.CInt))
hs_bindgen_4064610c89a52f99 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_4064610c89a52f99_base (\x1 ->
                                                            fmap BG.toFFIType (fun0 (BG.fromFFIType x1))))

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> IO BG.CInt)@
foreign import ccall safe "dynamic" hs_bindgen_46b04bdda1fdaafc_base ::
     BG.FunPtr (BG.Ptr BG.Void -> IO BG.CInt)
  -> BG.Ptr BG.Void -> IO BG.CInt

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> IO BG.CInt)@
hs_bindgen_46b04bdda1fdaafc ::
     BG.FunPtr (BG.Ptr Measurement -> IO BG.CInt)
  -> BG.Ptr Measurement -> IO BG.CInt
hs_bindgen_46b04bdda1fdaafc =
  \funPtr0 ->
    \x1 ->
      fmap BG.fromFFIType (hs_bindgen_46b04bdda1fdaafc_base (BG.castFunPtr funPtr0) (BG.toFFIType x1))

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_0e70e3ef331da59c_base ::
     (BG.Ptr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> IO ())@
hs_bindgen_0e70e3ef331da59c ::
     (BG.Ptr Measurement -> IO ())
  -> IO (BG.FunPtr (BG.Ptr Measurement -> IO ()))
hs_bindgen_0e70e3ef331da59c =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_0e70e3ef331da59c_base (\x1 ->
                                                            fun0 (BG.fromFFIType x1)))

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_900e47c9e85104f3_base ::
     BG.FunPtr (BG.Ptr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> IO ())@
hs_bindgen_900e47c9e85104f3 ::
     BG.FunPtr (BG.Ptr Measurement -> IO ())
  -> BG.Ptr Measurement -> IO ()
hs_bindgen_900e47c9e85104f3 =
  \funPtr0 ->
    \x1 ->
      hs_bindgen_900e47c9e85104f3_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> DataValidator -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_ee5115ecd6cd3b7d_base ::
     (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> DataValidator -> IO ())@
hs_bindgen_ee5115ecd6cd3b7d ::
     (BG.Ptr Measurement -> DataValidator -> IO ())
  -> IO (BG.FunPtr (BG.Ptr Measurement -> DataValidator -> IO ()))
hs_bindgen_ee5115ecd6cd3b7d =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_ee5115ecd6cd3b7d_base (\x1 ->
                                                            \x2 ->
                                                              fun0 (BG.fromFFIType x1) (BG.fromFFIType x2)))

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> DataValidator -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_767024eb494db15e_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> BG.FunPtr BG.Void -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> DataValidator -> IO ())@
hs_bindgen_767024eb494db15e ::
     BG.FunPtr (BG.Ptr Measurement -> DataValidator -> IO ())
  -> BG.Ptr Measurement -> DataValidator -> IO ()
hs_bindgen_767024eb494db15e =
  \funPtr0 ->
    \x1 ->
      \x2 ->
        hs_bindgen_767024eb494db15e_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2)

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_9c62e3e4c665b341_base ::
     (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.CInt -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ())@
hs_bindgen_9c62e3e4c665b341 ::
     (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()))
hs_bindgen_9c62e3e4c665b341 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_9c62e3e4c665b341_base (\x1 ->
                                                            \x2 ->
                                                              \x3 ->
                                                                fun0 (BG.fromFFIType x1) (BG.fromFFIType x2) (BG.fromFFIType x3)))

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_be989dc6cfcbaa3c_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.CInt -> IO ())
  -> BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.CInt -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ())@
hs_bindgen_be989dc6cfcbaa3c ::
     BG.FunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ())
  -> BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()
hs_bindgen_be989dc6cfcbaa3c =
  \funPtr0 ->
    \x1 ->
      \x2 ->
        \x3 ->
          hs_bindgen_be989dc6cfcbaa3c_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2) (BG.toFFIType x3)

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> FileOpenedNotification -> BG.CInt -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_cbc1799daa163d3d_base ::
     (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.CInt -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> FileOpenedNotification -> BG.CInt -> IO ())@
hs_bindgen_cbc1799daa163d3d ::
     (BG.Ptr Measurement -> FileOpenedNotification -> BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.Ptr Measurement -> FileOpenedNotification -> BG.CInt -> IO ()))
hs_bindgen_cbc1799daa163d3d =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_cbc1799daa163d3d_base (\x1 ->
                                                            \x2 ->
                                                              \x3 ->
                                                                fun0 (BG.fromFFIType x1) (BG.fromFFIType x2) (BG.fromFFIType x3)))

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> FileOpenedNotification -> BG.CInt -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_b313881940dff0dc_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.CInt -> IO ())
  -> BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.CInt -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> FileOpenedNotification -> BG.CInt -> IO ())@
hs_bindgen_b313881940dff0dc ::
     BG.FunPtr (BG.Ptr Measurement -> FileOpenedNotification -> BG.CInt -> IO ())
  -> BG.Ptr Measurement -> FileOpenedNotification -> BG.CInt -> IO ()
hs_bindgen_b313881940dff0dc =
  \funPtr0 ->
    \x1 ->
      \x2 ->
        \x3 ->
          hs_bindgen_b313881940dff0dc_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2) (BG.toFFIType x3)

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> ProgressUpdate -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_7b1e0c518e5f0b1b_base ::
     (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> ProgressUpdate -> IO ())@
hs_bindgen_7b1e0c518e5f0b1b ::
     (BG.Ptr Measurement -> ProgressUpdate -> IO ())
  -> IO (BG.FunPtr (BG.Ptr Measurement -> ProgressUpdate -> IO ()))
hs_bindgen_7b1e0c518e5f0b1b =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_7b1e0c518e5f0b1b_base (\x1 ->
                                                            \x2 ->
                                                              fun0 (BG.fromFFIType x1) (BG.fromFFIType x2)))

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> ProgressUpdate -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_0a3b532434d7e43c_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> BG.FunPtr BG.Void -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> ProgressUpdate -> IO ())@
hs_bindgen_0a3b532434d7e43c ::
     BG.FunPtr (BG.Ptr Measurement -> ProgressUpdate -> IO ())
  -> BG.Ptr Measurement -> ProgressUpdate -> IO ()
hs_bindgen_0a3b532434d7e43c =
  \funPtr0 ->
    \x1 ->
      \x2 ->
        hs_bindgen_0a3b532434d7e43c_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2)

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.CDouble -> BG.CInt -> IO BG.CDouble) -> BG.CInt -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_ba073f1e0ec5008b_base ::
     (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.CInt -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.CDouble -> BG.CInt -> IO BG.CDouble) -> BG.CInt -> IO ())@
hs_bindgen_ba073f1e0ec5008b ::
     (BG.Ptr Measurement -> BG.FunPtr (BG.CDouble -> BG.CInt -> IO BG.CDouble) -> BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.CDouble -> BG.CInt -> IO BG.CDouble) -> BG.CInt -> IO ()))
hs_bindgen_ba073f1e0ec5008b =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_ba073f1e0ec5008b_base (\x1 ->
                                                            \x2 ->
                                                              \x3 ->
                                                                fun0 (BG.fromFFIType x1) (BG.fromFFIType x2) (BG.fromFFIType x3)))

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.CDouble -> BG.CInt -> IO BG.CDouble) -> BG.CInt -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_090d079ea979a39d_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.CInt -> IO ())
  -> BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.CInt -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.CDouble -> BG.CInt -> IO BG.CDouble) -> BG.CInt -> IO ())@
hs_bindgen_090d079ea979a39d ::
     BG.FunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.CDouble -> BG.CInt -> IO BG.CDouble) -> BG.CInt -> IO ())
  -> BG.Ptr Measurement -> BG.FunPtr (BG.CDouble -> BG.CInt -> IO BG.CDouble) -> BG.CInt -> IO ()
hs_bindgen_090d079ea979a39d =
  \funPtr0 ->
    \x1 ->
      \x2 ->
        \x3 ->
          hs_bindgen_090d079ea979a39d_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2) (BG.toFFIType x3)

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()) -> DataValidator -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_e1d08f76a35007c0_base ::
     (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.FunPtr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.FunPtr BG.Void -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()) -> DataValidator -> IO ())@
hs_bindgen_e1d08f76a35007c0 ::
     (BG.Ptr Measurement -> BG.FunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()) -> DataValidator -> IO ())
  -> IO (BG.FunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()) -> DataValidator -> IO ()))
hs_bindgen_e1d08f76a35007c0 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_e1d08f76a35007c0_base (\x1 ->
                                                            \x2 ->
                                                              \x3 ->
                                                                fun0 (BG.fromFFIType x1) (BG.fromFFIType x2) (BG.fromFFIType x3)))

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()) -> DataValidator -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_74f2cb0fd2d13cc1_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.FunPtr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.FunPtr BG.Void -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()) -> DataValidator -> IO ())@
hs_bindgen_74f2cb0fd2d13cc1 ::
     BG.FunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()) -> DataValidator -> IO ())
  -> BG.Ptr Measurement -> BG.FunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()) -> DataValidator -> IO ()
hs_bindgen_74f2cb0fd2d13cc1 =
  \funPtr0 ->
    \x1 ->
      \x2 ->
        \x3 ->
          hs_bindgen_74f2cb0fd2d13cc1_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2) (BG.toFFIType x3)

-- __unique:__ @instance ToFunPtr (Foo2 -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_235fa4a89af25f04_base ::
     (BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.CInt -> IO ()))

-- __unique:__ @instance ToFunPtr (Foo2 -> IO ())@
hs_bindgen_235fa4a89af25f04 ::
     (Foo2 -> IO ())
  -> IO (BG.FunPtr (Foo2 -> IO ()))
hs_bindgen_235fa4a89af25f04 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_235fa4a89af25f04_base (\x1 ->
                                                            fun0 (BG.fromFFIType x1)))

-- __unique:__ @instance FromFunPtr (Foo2 -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_8605b223a9ab9562_base ::
     BG.FunPtr (BG.CInt -> IO ())
  -> BG.CInt -> IO ()

-- __unique:__ @instance FromFunPtr (Foo2 -> IO ())@
hs_bindgen_8605b223a9ab9562 ::
     BG.FunPtr (Foo2 -> IO ())
  -> Foo2 -> IO ()
hs_bindgen_8605b223a9ab9562 =
  \funPtr0 ->
    \x1 ->
      hs_bindgen_8605b223a9ab9562_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)
