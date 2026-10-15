{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Example
import Prelude (IO, fmap)

-- __unique:__ @toFunptr_typedef1_Aux@
foreign import ccall safe "wrapper" hs_bindgen_c584d0f839fd43de_base ::
     IO BG.CInt
  -> IO (BG.FunPtr (IO BG.CInt))

-- __unique:__ @toFunptr_typedef1_Aux@
hs_bindgen_c584d0f839fd43de ::
     Funptr_typedef1_Aux
  -> IO (BG.FunPtr Funptr_typedef1_Aux)
hs_bindgen_c584d0f839fd43de =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_c584d0f839fd43de_base (fmap BG.toFFIType (BG.getField @"unwrapFunptr_typedef1_Aux" fun0)))

-- __unique:__ @fromFunptr_typedef1_Aux@
foreign import ccall safe "dynamic" hs_bindgen_806a46dc418a062c_base ::
     BG.FunPtr (IO BG.CInt)
  -> IO BG.CInt

-- __unique:__ @fromFunptr_typedef1_Aux@
hs_bindgen_806a46dc418a062c ::
     BG.FunPtr Funptr_typedef1_Aux
  -> Funptr_typedef1_Aux
hs_bindgen_806a46dc418a062c =
  \funPtr0 ->
    Funptr_typedef1_Aux (fmap BG.fromFFIType (hs_bindgen_806a46dc418a062c_base (BG.castFunPtr funPtr0)))

-- __unique:__ @toFunptr_typedef2_Aux@
foreign import ccall safe "wrapper" hs_bindgen_f174457a161ac5a0_base ::
     IO (BG.Ptr BG.Void)
  -> IO (BG.FunPtr (IO (BG.Ptr BG.Void)))

-- __unique:__ @toFunptr_typedef2_Aux@
hs_bindgen_f174457a161ac5a0 ::
     Funptr_typedef2_Aux
  -> IO (BG.FunPtr Funptr_typedef2_Aux)
hs_bindgen_f174457a161ac5a0 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_f174457a161ac5a0_base (fmap BG.toFFIType (BG.getField @"unwrapFunptr_typedef2_Aux" fun0)))

-- __unique:__ @fromFunptr_typedef2_Aux@
foreign import ccall safe "dynamic" hs_bindgen_323d07dff85b802c_base ::
     BG.FunPtr (IO (BG.Ptr BG.Void))
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @fromFunptr_typedef2_Aux@
hs_bindgen_323d07dff85b802c ::
     BG.FunPtr Funptr_typedef2_Aux
  -> Funptr_typedef2_Aux
hs_bindgen_323d07dff85b802c =
  \funPtr0 ->
    Funptr_typedef2_Aux (fmap BG.fromFFIType (hs_bindgen_323d07dff85b802c_base (BG.castFunPtr funPtr0)))

-- __unique:__ @toFunptr_typedef3_Aux@
foreign import ccall safe "wrapper" hs_bindgen_031d1a7decd790d8_base ::
     IO (BG.Ptr BG.Void)
  -> IO (BG.FunPtr (IO (BG.Ptr BG.Void)))

-- __unique:__ @toFunptr_typedef3_Aux@
hs_bindgen_031d1a7decd790d8 ::
     Funptr_typedef3_Aux
  -> IO (BG.FunPtr Funptr_typedef3_Aux)
hs_bindgen_031d1a7decd790d8 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_031d1a7decd790d8_base (fmap BG.toFFIType (BG.getField @"unwrapFunptr_typedef3_Aux" fun0)))

-- __unique:__ @fromFunptr_typedef3_Aux@
foreign import ccall safe "dynamic" hs_bindgen_82dc7b932974117e_base ::
     BG.FunPtr (IO (BG.Ptr BG.Void))
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @fromFunptr_typedef3_Aux@
hs_bindgen_82dc7b932974117e ::
     BG.FunPtr Funptr_typedef3_Aux
  -> Funptr_typedef3_Aux
hs_bindgen_82dc7b932974117e =
  \funPtr0 ->
    Funptr_typedef3_Aux (fmap BG.fromFFIType (hs_bindgen_82dc7b932974117e_base (BG.castFunPtr funPtr0)))

-- __unique:__ @toFunptr_typedef4_Aux@
foreign import ccall safe "wrapper" hs_bindgen_da2336d254667386_base ::
     (BG.CInt -> BG.CDouble -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CInt))

-- __unique:__ @toFunptr_typedef4_Aux@
hs_bindgen_da2336d254667386 ::
     Funptr_typedef4_Aux
  -> IO (BG.FunPtr Funptr_typedef4_Aux)
hs_bindgen_da2336d254667386 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_da2336d254667386_base (\x1 ->
                                                            \x2 ->
                                                              fmap BG.toFFIType (BG.getField @"unwrapFunptr_typedef4_Aux" fun0 (BG.fromFFIType x1) (BG.fromFFIType x2))))

-- __unique:__ @fromFunptr_typedef4_Aux@
foreign import ccall safe "dynamic" hs_bindgen_d4a97954476da161_base ::
     BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CInt)
  -> BG.CInt -> BG.CDouble -> IO BG.CInt

-- __unique:__ @fromFunptr_typedef4_Aux@
hs_bindgen_d4a97954476da161 ::
     BG.FunPtr Funptr_typedef4_Aux
  -> Funptr_typedef4_Aux
hs_bindgen_d4a97954476da161 =
  \funPtr0 ->
    Funptr_typedef4_Aux (\x1 ->
                           \x2 ->
                             fmap BG.fromFFIType (hs_bindgen_d4a97954476da161_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2)))

-- __unique:__ @toFunptr_typedef5_Aux@
foreign import ccall safe "wrapper" hs_bindgen_1f45632f07742a46_base ::
     (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void))
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void)))

-- __unique:__ @toFunptr_typedef5_Aux@
hs_bindgen_1f45632f07742a46 ::
     Funptr_typedef5_Aux
  -> IO (BG.FunPtr Funptr_typedef5_Aux)
hs_bindgen_1f45632f07742a46 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_1f45632f07742a46_base (\x1 ->
                                                            \x2 ->
                                                              fmap BG.toFFIType (BG.getField @"unwrapFunptr_typedef5_Aux" fun0 (BG.fromFFIType x1) (BG.fromFFIType x2))))

-- __unique:__ @fromFunptr_typedef5_Aux@
foreign import ccall safe "dynamic" hs_bindgen_0bd1877eaaba0d3e_base ::
     BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void))
  -> BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void)

-- __unique:__ @fromFunptr_typedef5_Aux@
hs_bindgen_0bd1877eaaba0d3e ::
     BG.FunPtr Funptr_typedef5_Aux
  -> Funptr_typedef5_Aux
hs_bindgen_0bd1877eaaba0d3e =
  \funPtr0 ->
    Funptr_typedef5_Aux (\x1 ->
                           \x2 ->
                             fmap BG.fromFFIType (hs_bindgen_0bd1877eaaba0d3e_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2)))

-- __unique:__ @toConst_funptr1_Aux@
foreign import ccall safe "wrapper" hs_bindgen_7f125e20a9d4075b_base ::
     (BG.CInt -> BG.CDouble -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CInt))

-- __unique:__ @toConst_funptr1_Aux@
hs_bindgen_7f125e20a9d4075b ::
     Const_funptr1_Aux
  -> IO (BG.FunPtr Const_funptr1_Aux)
hs_bindgen_7f125e20a9d4075b =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_7f125e20a9d4075b_base (\x1 ->
                                                            \x2 ->
                                                              fmap BG.toFFIType (BG.getField @"unwrapConst_funptr1_Aux" fun0 (BG.fromFFIType x1) (BG.fromFFIType x2))))

-- __unique:__ @fromConst_funptr1_Aux@
foreign import ccall safe "dynamic" hs_bindgen_ac4bd8d789bba94b_base ::
     BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CInt)
  -> BG.CInt -> BG.CDouble -> IO BG.CInt

-- __unique:__ @fromConst_funptr1_Aux@
hs_bindgen_ac4bd8d789bba94b ::
     BG.FunPtr Const_funptr1_Aux
  -> Const_funptr1_Aux
hs_bindgen_ac4bd8d789bba94b =
  \funPtr0 ->
    Const_funptr1_Aux (\x1 ->
                         \x2 ->
                           fmap BG.fromFFIType (hs_bindgen_ac4bd8d789bba94b_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2)))

-- __unique:__ @toConst_funptr2_Aux@
foreign import ccall safe "wrapper" hs_bindgen_c7b1e36d845634fb_base ::
     (BG.CInt -> BG.CDouble -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CInt))

-- __unique:__ @toConst_funptr2_Aux@
hs_bindgen_c7b1e36d845634fb ::
     Const_funptr2_Aux
  -> IO (BG.FunPtr Const_funptr2_Aux)
hs_bindgen_c7b1e36d845634fb =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_c7b1e36d845634fb_base (\x1 ->
                                                            \x2 ->
                                                              fmap BG.toFFIType (BG.getField @"unwrapConst_funptr2_Aux" fun0 (BG.fromFFIType x1) (BG.fromFFIType x2))))

-- __unique:__ @fromConst_funptr2_Aux@
foreign import ccall safe "dynamic" hs_bindgen_352cebf463125ca9_base ::
     BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CInt)
  -> BG.CInt -> BG.CDouble -> IO BG.CInt

-- __unique:__ @fromConst_funptr2_Aux@
hs_bindgen_352cebf463125ca9 ::
     BG.FunPtr Const_funptr2_Aux
  -> Const_funptr2_Aux
hs_bindgen_352cebf463125ca9 =
  \funPtr0 ->
    Const_funptr2_Aux (\x1 ->
                         \x2 ->
                           fmap BG.fromFFIType (hs_bindgen_352cebf463125ca9_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2)))

-- __unique:__ @toConst_funptr3_Aux@
foreign import ccall safe "wrapper" hs_bindgen_2dcbfe1c2502178c_base ::
     (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void))
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void)))

-- __unique:__ @toConst_funptr3_Aux@
hs_bindgen_2dcbfe1c2502178c ::
     Const_funptr3_Aux
  -> IO (BG.FunPtr Const_funptr3_Aux)
hs_bindgen_2dcbfe1c2502178c =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_2dcbfe1c2502178c_base (\x1 ->
                                                            \x2 ->
                                                              fmap BG.toFFIType (BG.getField @"unwrapConst_funptr3_Aux" fun0 (BG.fromFFIType x1) (BG.fromFFIType x2))))

-- __unique:__ @fromConst_funptr3_Aux@
foreign import ccall safe "dynamic" hs_bindgen_86738dcfd7c9d33c_base ::
     BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void))
  -> BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void)

-- __unique:__ @fromConst_funptr3_Aux@
hs_bindgen_86738dcfd7c9d33c ::
     BG.FunPtr Const_funptr3_Aux
  -> Const_funptr3_Aux
hs_bindgen_86738dcfd7c9d33c =
  \funPtr0 ->
    Const_funptr3_Aux (\x1 ->
                         \x2 ->
                           fmap BG.fromFFIType (hs_bindgen_86738dcfd7c9d33c_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2)))

-- __unique:__ @toConst_funptr4_Aux@
foreign import ccall safe "wrapper" hs_bindgen_5461deeda491de0b_base ::
     (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void))
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void)))

-- __unique:__ @toConst_funptr4_Aux@
hs_bindgen_5461deeda491de0b ::
     Const_funptr4_Aux
  -> IO (BG.FunPtr Const_funptr4_Aux)
hs_bindgen_5461deeda491de0b =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_5461deeda491de0b_base (\x1 ->
                                                            \x2 ->
                                                              fmap BG.toFFIType (BG.getField @"unwrapConst_funptr4_Aux" fun0 (BG.fromFFIType x1) (BG.fromFFIType x2))))

-- __unique:__ @fromConst_funptr4_Aux@
foreign import ccall safe "dynamic" hs_bindgen_de7846fca3bfd1b6_base ::
     BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void))
  -> BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void)

-- __unique:__ @fromConst_funptr4_Aux@
hs_bindgen_de7846fca3bfd1b6 ::
     BG.FunPtr Const_funptr4_Aux
  -> Const_funptr4_Aux
hs_bindgen_de7846fca3bfd1b6 =
  \funPtr0 ->
    Const_funptr4_Aux (\x1 ->
                         \x2 ->
                           fmap BG.fromFFIType (hs_bindgen_de7846fca3bfd1b6_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2)))

-- __unique:__ @toConst_funptr5_Aux@
foreign import ccall safe "wrapper" hs_bindgen_7b0174fc978a1ce1_base ::
     (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void))
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void)))

-- __unique:__ @toConst_funptr5_Aux@
hs_bindgen_7b0174fc978a1ce1 ::
     Const_funptr5_Aux
  -> IO (BG.FunPtr Const_funptr5_Aux)
hs_bindgen_7b0174fc978a1ce1 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_7b0174fc978a1ce1_base (\x1 ->
                                                            \x2 ->
                                                              fmap BG.toFFIType (BG.getField @"unwrapConst_funptr5_Aux" fun0 (BG.fromFFIType x1) (BG.fromFFIType x2))))

-- __unique:__ @fromConst_funptr5_Aux@
foreign import ccall safe "dynamic" hs_bindgen_38a21d84bb7115b5_base ::
     BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void))
  -> BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void)

-- __unique:__ @fromConst_funptr5_Aux@
hs_bindgen_38a21d84bb7115b5 ::
     BG.FunPtr Const_funptr5_Aux
  -> Const_funptr5_Aux
hs_bindgen_38a21d84bb7115b5 =
  \funPtr0 ->
    Const_funptr5_Aux (\x1 ->
                         \x2 ->
                           fmap BG.fromFFIType (hs_bindgen_38a21d84bb7115b5_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2)))

-- __unique:__ @toConst_funptr6_Aux@
foreign import ccall safe "wrapper" hs_bindgen_4e32721222f4df9f_base ::
     (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void))
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void)))

-- __unique:__ @toConst_funptr6_Aux@
hs_bindgen_4e32721222f4df9f ::
     Const_funptr6_Aux
  -> IO (BG.FunPtr Const_funptr6_Aux)
hs_bindgen_4e32721222f4df9f =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_4e32721222f4df9f_base (\x1 ->
                                                            \x2 ->
                                                              fmap BG.toFFIType (BG.getField @"unwrapConst_funptr6_Aux" fun0 (BG.fromFFIType x1) (BG.fromFFIType x2))))

-- __unique:__ @fromConst_funptr6_Aux@
foreign import ccall safe "dynamic" hs_bindgen_45251216b04aa8b5_base ::
     BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void))
  -> BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void)

-- __unique:__ @fromConst_funptr6_Aux@
hs_bindgen_45251216b04aa8b5 ::
     BG.FunPtr Const_funptr6_Aux
  -> Const_funptr6_Aux
hs_bindgen_45251216b04aa8b5 =
  \funPtr0 ->
    Const_funptr6_Aux (\x1 ->
                         \x2 ->
                           fmap BG.fromFFIType (hs_bindgen_45251216b04aa8b5_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2)))

-- __unique:__ @toConst_funptr7_Aux@
foreign import ccall safe "wrapper" hs_bindgen_0d04fc96ffb9de06_base ::
     (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void))
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void)))

-- __unique:__ @toConst_funptr7_Aux@
hs_bindgen_0d04fc96ffb9de06 ::
     Const_funptr7_Aux
  -> IO (BG.FunPtr Const_funptr7_Aux)
hs_bindgen_0d04fc96ffb9de06 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_0d04fc96ffb9de06_base (\x1 ->
                                                            \x2 ->
                                                              fmap BG.toFFIType (BG.getField @"unwrapConst_funptr7_Aux" fun0 (BG.fromFFIType x1) (BG.fromFFIType x2))))

-- __unique:__ @fromConst_funptr7_Aux@
foreign import ccall safe "dynamic" hs_bindgen_42fbcebf75a973ba_base ::
     BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void))
  -> BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.Void)

-- __unique:__ @fromConst_funptr7_Aux@
hs_bindgen_42fbcebf75a973ba ::
     BG.FunPtr Const_funptr7_Aux
  -> Const_funptr7_Aux
hs_bindgen_42fbcebf75a973ba =
  \funPtr0 ->
    Const_funptr7_Aux (\x1 ->
                         \x2 ->
                           fmap BG.fromFFIType (hs_bindgen_42fbcebf75a973ba_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2)))
