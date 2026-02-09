{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Marshal
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @triplet@

    __defined at:__ @arrays\/array.h 41:13@

    __exported by:__ @arrays\/array.h@
-}
newtype Triplet = Triplet
  { unwrapTriplet :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance GHC.Records.HasField "unwrapTriplet" (Ptr.Ptr Triplet) (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapTriplet")

instance HsBindgen.Runtime.HasCField.HasCField Triplet "unwrapTriplet" where

  type CFieldType Triplet "unwrapTriplet" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @list@

    __defined at:__ @arrays\/array.h 43:13@

    __exported by:__ @arrays\/array.h@
-}
newtype List = List
  { unwrapList :: HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance GHC.Records.HasField "unwrapList" (Ptr.Ptr List) (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapList")

instance HsBindgen.Runtime.HasCField.HasCField List "unwrapList" where

  type CFieldType List "unwrapList" =
    HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @matrix@

    __defined at:__ @arrays\/array.h 45:13@

    __exported by:__ @arrays\/array.h@
-}
newtype Matrix = Matrix
  { unwrapMatrix :: (HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance GHC.Records.HasField "unwrapMatrix" (Ptr.Ptr Matrix) (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapMatrix")

instance HsBindgen.Runtime.HasCField.HasCField Matrix "unwrapMatrix" where

  type CFieldType Matrix "unwrapMatrix" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @tripletlist@

    __defined at:__ @arrays\/array.h 47:13@

    __exported by:__ @arrays\/array.h@
-}
newtype Tripletlist = Tripletlist
  { unwrapTripletlist :: HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance GHC.Records.HasField "unwrapTripletlist" (Ptr.Ptr Tripletlist) (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapTripletlist")

instance HsBindgen.Runtime.HasCField.HasCField Tripletlist "unwrapTripletlist" where

  type CFieldType Tripletlist "unwrapTripletlist" =
    HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct Example@

    __defined at:__ @arrays\/array.h 49:8@

    __exported by:__ @arrays\/array.h@
-}
data Example = Example
  { example_triple :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
    {- ^ __C declaration:__ @triple@

         __defined at:__ @arrays\/array.h 50:9@

         __exported by:__ @arrays\/array.h@
    -}
  , example_sudoku :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
    {- ^ __C declaration:__ @sudoku@

         __defined at:__ @arrays\/array.h 51:9@

         __exported by:__ @arrays\/array.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Example where

  staticSizeOf = \_ -> (48 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Example where

  readRaw =
    \ptr0 ->
          pure Example
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"example_triple") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"example_sudoku") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Example where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Example example_triple2 example_sudoku3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"example_triple") ptr0 example_triple2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"example_sudoku") ptr0 example_sudoku3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Example instance F.Storable Example

instance HsBindgen.Runtime.HasCField.HasCField Example "example_triple" where

  type CFieldType Example "example_triple" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "example_triple" (Ptr.Ptr Example) (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"example_triple")

instance HsBindgen.Runtime.HasCField.HasCField Example "example_sudoku" where

  type CFieldType Example "example_sudoku" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)

  offset# = \_ -> \_ -> 12

instance GHC.Records.HasField "example_sudoku" (Ptr.Ptr Example) (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"example_sudoku")

{-| Typedef-in-typedef

__C declaration:__ @sudoku@

__defined at:__ @arrays\/array.h 55:17@

__exported by:__ @arrays\/array.h@
-}
newtype Sudoku = Sudoku
  { unwrapSudoku :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) Triplet
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance GHC.Records.HasField "unwrapSudoku" (Ptr.Ptr Sudoku) (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) Triplet)) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapSudoku")

instance HsBindgen.Runtime.HasCField.HasCField Sudoku "unwrapSudoku" where

  type CFieldType Sudoku "unwrapSudoku" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) Triplet

  offset# = \_ -> \_ -> 0
