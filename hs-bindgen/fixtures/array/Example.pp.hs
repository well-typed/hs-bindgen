{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.IncompleteArray
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @triplet@

    __defined at:__ @array.h:41:13@

    __exported by:__ @array.h@
-}
newtype Triplet = Triplet
  { un_Triplet :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @list@

    __defined at:__ @array.h:43:13@

    __exported by:__ @array.h@
-}
newtype List = List
  { un_List :: HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt
  }
  deriving stock (Eq, Show)

{-| __C declaration:__ @matrix@

    __defined at:__ @array.h:45:13@

    __exported by:__ @array.h@
-}
newtype Matrix = Matrix
  { un_Matrix :: (HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @tripletlist@

    __defined at:__ @array.h:47:13@

    __exported by:__ @array.h@
-}
newtype Tripletlist = Tripletlist
  { un_Tripletlist :: HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  }
  deriving stock (Eq, Show)

{-| __C declaration:__ @Example@

    __defined at:__ @array.h:49:8@

    __exported by:__ @array.h@
-}
data Example = Example
  { example_triple :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
    {- ^ __C declaration:__ @triple@

         __defined at:__ @array.h:50:9@

         __exported by:__ @array.h@
    -}
  , example_sudoku :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
    {- ^ __C declaration:__ @sudoku@

         __defined at:__ @array.h:51:9@

         __exported by:__ @array.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Example where

  sizeOf = \_ -> (48 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Example
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (12 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Example example_triple2 example_sudoku3 ->
               F.pokeByteOff ptr0 (0 :: Int) example_triple2
            >> F.pokeByteOff ptr0 (12 :: Int) example_sudoku3

{-| Typedef-in-typedef

__C declaration:__ @sudoku@

__defined at:__ @array.h:55:17@

__exported by:__ @array.h@
-}
newtype Sudoku = Sudoku
  { un_Sudoku :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) Triplet
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)
