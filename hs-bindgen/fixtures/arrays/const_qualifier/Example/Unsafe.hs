{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.PtrConst
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <arrays/const_qualifier.h>"
  , "void hs_bindgen_7eb8358d30ebb28b ("
  , "  signed int const (*arg1)[]"
  , ")"
  , "{"
  , "  foo(*arg1);"
  , "}"
  , "void hs_bindgen_2fc1970fe11a3461 ("
  , "  S *arg1"
  , ")"
  , "{"
  , "  fooA(*arg1);"
  , "}"
  , "void hs_bindgen_58689a7e4b1d44f6 ("
  , "  S const *arg1"
  , ")"
  , "{"
  , "  fooB(*arg1);"
  , "}"
  , "void hs_bindgen_f88b417ee6e28c0c ("
  , "  T const *arg1"
  , ")"
  , "{"
  , "  fooC(*arg1);"
  , "}"
  , "void hs_bindgen_78493aa01e9cc5da ("
  , "  signed int const (*arg1)[3]"
  , ")"
  , "{"
  , "  bar(*arg1);"
  , "}"
  , "void hs_bindgen_bee2c20f290d25d6 ("
  , "  U *arg1"
  , ")"
  , "{"
  , "  barA(*arg1);"
  , "}"
  , "void hs_bindgen_71f805899a96d9f1 ("
  , "  U const *arg1"
  , ")"
  , "{"
  , "  barB(*arg1);"
  , "}"
  , "void hs_bindgen_89af56a24c359834 ("
  , "  V const *arg1"
  , ")"
  , "{"
  , "  barC(*arg1);"
  , "}"
  , "void hs_bindgen_71d7d347923cf6e5 ("
  , "  signed int const (*arg1)[3]"
  , ")"
  , "{"
  , "  baz(arg1);"
  , "}"
  , "void hs_bindgen_25cc50be574bcc6c ("
  , "  W *arg1"
  , ")"
  , "{"
  , "  bazA(arg1);"
  , "}"
  , "void hs_bindgen_3fd3913cc13f61be ("
  , "  W const *arg1"
  , ")"
  , "{"
  , "  bazB(arg1);"
  , "}"
  , "void hs_bindgen_a91ac27216749da9 ("
  , "  X const *arg1"
  , ")"
  , "{"
  , "  bazC(arg1);"
  , "}"
  ]))

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_7eb8358d30ebb28b" hs_bindgen_7eb8358d30ebb28b_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_foo@
hs_bindgen_7eb8358d30ebb28b ::
     HsBindgen.Runtime.PtrConst.PtrConst (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
  -> IO ()
hs_bindgen_7eb8358d30ebb28b =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_7eb8358d30ebb28b_base

{-| __C declaration:__ @foo@

    __defined at:__ @arrays\/const_qualifier.h 5:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
foo ::
     HsBindgen.Runtime.PtrConst.PtrConst (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_7eb8358d30ebb28b

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_2fc1970fe11a3461" hs_bindgen_2fc1970fe11a3461_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_fooA@
hs_bindgen_2fc1970fe11a3461 ::
     HsBindgen.Runtime.PtrConst.PtrConst S
  -> IO ()
hs_bindgen_2fc1970fe11a3461 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_2fc1970fe11a3461_base

{-| __C declaration:__ @fooA@

    __defined at:__ @arrays\/const_qualifier.h 10:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
fooA ::
     HsBindgen.Runtime.PtrConst.PtrConst S
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_2fc1970fe11a3461

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_58689a7e4b1d44f6" hs_bindgen_58689a7e4b1d44f6_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_fooB@
hs_bindgen_58689a7e4b1d44f6 ::
     HsBindgen.Runtime.PtrConst.PtrConst S
  -> IO ()
hs_bindgen_58689a7e4b1d44f6 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_58689a7e4b1d44f6_base

{-| __C declaration:__ @fooB@

    __defined at:__ @arrays\/const_qualifier.h 11:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
fooB ::
     HsBindgen.Runtime.PtrConst.PtrConst S
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_58689a7e4b1d44f6

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_fooC@
foreign import ccall unsafe "hs_bindgen_f88b417ee6e28c0c" hs_bindgen_f88b417ee6e28c0c_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_fooC@
hs_bindgen_f88b417ee6e28c0c ::
     HsBindgen.Runtime.PtrConst.PtrConst T
  -> IO ()
hs_bindgen_f88b417ee6e28c0c =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_f88b417ee6e28c0c_base

{-| __C declaration:__ @fooC@

    __defined at:__ @arrays\/const_qualifier.h 12:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
fooC ::
     HsBindgen.Runtime.PtrConst.PtrConst T
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_f88b417ee6e28c0c

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_78493aa01e9cc5da" hs_bindgen_78493aa01e9cc5da_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_bar@
hs_bindgen_78493aa01e9cc5da ::
     HsBindgen.Runtime.PtrConst.PtrConst ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO ()
hs_bindgen_78493aa01e9cc5da =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_78493aa01e9cc5da_base

{-| __C declaration:__ @bar@

    __defined at:__ @arrays\/const_qualifier.h 16:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
bar ::
     HsBindgen.Runtime.PtrConst.PtrConst ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_78493aa01e9cc5da

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_barA@
foreign import ccall unsafe "hs_bindgen_bee2c20f290d25d6" hs_bindgen_bee2c20f290d25d6_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_barA@
hs_bindgen_bee2c20f290d25d6 ::
     HsBindgen.Runtime.PtrConst.PtrConst U
  -> IO ()
hs_bindgen_bee2c20f290d25d6 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_bee2c20f290d25d6_base

{-| __C declaration:__ @barA@

    __defined at:__ @arrays\/const_qualifier.h 21:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
barA ::
     HsBindgen.Runtime.PtrConst.PtrConst U
     -- ^ __C declaration:__ @x@
  -> IO ()
barA = hs_bindgen_bee2c20f290d25d6

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_barB@
foreign import ccall unsafe "hs_bindgen_71f805899a96d9f1" hs_bindgen_71f805899a96d9f1_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_barB@
hs_bindgen_71f805899a96d9f1 ::
     HsBindgen.Runtime.PtrConst.PtrConst U
  -> IO ()
hs_bindgen_71f805899a96d9f1 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_71f805899a96d9f1_base

{-| __C declaration:__ @barB@

    __defined at:__ @arrays\/const_qualifier.h 22:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
barB ::
     HsBindgen.Runtime.PtrConst.PtrConst U
     -- ^ __C declaration:__ @x@
  -> IO ()
barB = hs_bindgen_71f805899a96d9f1

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_barC@
foreign import ccall unsafe "hs_bindgen_89af56a24c359834" hs_bindgen_89af56a24c359834_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_barC@
hs_bindgen_89af56a24c359834 ::
     HsBindgen.Runtime.PtrConst.PtrConst V
  -> IO ()
hs_bindgen_89af56a24c359834 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_89af56a24c359834_base

{-| __C declaration:__ @barC@

    __defined at:__ @arrays\/const_qualifier.h 23:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
barC ::
     HsBindgen.Runtime.PtrConst.PtrConst V
     -- ^ __C declaration:__ @x@
  -> IO ()
barC = hs_bindgen_89af56a24c359834

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_baz@
foreign import ccall unsafe "hs_bindgen_71d7d347923cf6e5" hs_bindgen_71d7d347923cf6e5_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_baz@
hs_bindgen_71d7d347923cf6e5 ::
     HsBindgen.Runtime.PtrConst.PtrConst ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO ()
hs_bindgen_71d7d347923cf6e5 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_71d7d347923cf6e5_base

{-| __C declaration:__ @baz@

    __defined at:__ @arrays\/const_qualifier.h 27:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
baz ::
     HsBindgen.Runtime.PtrConst.PtrConst ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
baz = hs_bindgen_71d7d347923cf6e5

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_bazA@
foreign import ccall unsafe "hs_bindgen_25cc50be574bcc6c" hs_bindgen_25cc50be574bcc6c_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_bazA@
hs_bindgen_25cc50be574bcc6c ::
     HsBindgen.Runtime.PtrConst.PtrConst W
  -> IO ()
hs_bindgen_25cc50be574bcc6c =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_25cc50be574bcc6c_base

{-| __C declaration:__ @bazA@

    __defined at:__ @arrays\/const_qualifier.h 32:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
bazA ::
     HsBindgen.Runtime.PtrConst.PtrConst W
     -- ^ __C declaration:__ @x@
  -> IO ()
bazA = hs_bindgen_25cc50be574bcc6c

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_bazB@
foreign import ccall unsafe "hs_bindgen_3fd3913cc13f61be" hs_bindgen_3fd3913cc13f61be_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_bazB@
hs_bindgen_3fd3913cc13f61be ::
     HsBindgen.Runtime.PtrConst.PtrConst W
  -> IO ()
hs_bindgen_3fd3913cc13f61be =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_3fd3913cc13f61be_base

{-| __C declaration:__ @bazB@

    __defined at:__ @arrays\/const_qualifier.h 33:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
bazB ::
     HsBindgen.Runtime.PtrConst.PtrConst W
     -- ^ __C declaration:__ @x@
  -> IO ()
bazB = hs_bindgen_3fd3913cc13f61be

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_bazC@
foreign import ccall unsafe "hs_bindgen_a91ac27216749da9" hs_bindgen_a91ac27216749da9_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Unsafe_bazC@
hs_bindgen_a91ac27216749da9 ::
     HsBindgen.Runtime.PtrConst.PtrConst X
  -> IO ()
hs_bindgen_a91ac27216749da9 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_a91ac27216749da9_base

{-| __C declaration:__ @bazC@

    __defined at:__ @arrays\/const_qualifier.h 34:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
bazC ::
     HsBindgen.Runtime.PtrConst.PtrConst X
     -- ^ __C declaration:__ @x@
  -> IO ()
bazC = hs_bindgen_a91ac27216749da9
