{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Triplet(..)
    , Example.List(..)
    , Example.Matrix(..)
    , Example.Tripletlist(..)
    , Example.Example(..)
    , Example.Sudoku(..)
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @triplet@

    __defined at:__ @arrays\/array.h 41:13@

    __exported by:__ @arrays\/array.h@
-}
newtype Triplet = Triplet
  { unwrapTriplet :: CA.ConstantArray 3 BG.CInt
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ CA.ConstantArray 3 BG.CInt
         ) => BG.CompatHasField.HasField "unwrapTriplet" Triplet ty where

  hasField =
    \x0 ->
      (\y1 ->
         Triplet {unwrapTriplet = y1}, BG.getField @"unwrapTriplet" x0)

instance ( ty ~ CA.ConstantArray 3 BG.CInt
         ) => BG.HasField "unwrapTriplet" (BG.Ptr Triplet) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapTriplet")

instance HasCField.HasCField Triplet "unwrapTriplet" where

  type CFieldType Triplet "unwrapTriplet" =
    CA.ConstantArray 3 BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @list@

    __defined at:__ @arrays\/array.h 43:13@

    __exported by:__ @arrays\/array.h@
-}
newtype List = List
  { unwrapList :: IA.IncompleteArray BG.CInt
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype (IsA.IsArray)

instance ( ty ~ IA.IncompleteArray BG.CInt
         ) => BG.CompatHasField.HasField "unwrapList" List ty where

  hasField =
    \x0 ->
      (\y1 ->
         List {unwrapList = y1}, BG.getField @"unwrapList" x0)

instance ( ty ~ IA.IncompleteArray BG.CInt
         ) => BG.HasField "unwrapList" (BG.Ptr List) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapList")

instance HasCField.HasCField List "unwrapList" where

  type CFieldType List "unwrapList" =
    IA.IncompleteArray BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @matrix@

    __defined at:__ @arrays\/array.h 45:13@

    __exported by:__ @arrays\/array.h@
-}
newtype Matrix = Matrix
  { unwrapMatrix :: CA.ConstantArray 4 (CA.ConstantArray 3 BG.CInt)
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ CA.ConstantArray 4 (CA.ConstantArray 3 BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapMatrix" Matrix ty where

  hasField =
    \x0 ->
      (\y1 ->
         Matrix {unwrapMatrix = y1}, BG.getField @"unwrapMatrix" x0)

instance ( ty ~ CA.ConstantArray 4 (CA.ConstantArray 3 BG.CInt)
         ) => BG.HasField "unwrapMatrix" (BG.Ptr Matrix) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMatrix")

instance HasCField.HasCField Matrix "unwrapMatrix" where

  type CFieldType Matrix "unwrapMatrix" =
    CA.ConstantArray 4 (CA.ConstantArray 3 BG.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @tripletlist@

    __defined at:__ @arrays\/array.h 47:13@

    __exported by:__ @arrays\/array.h@
-}
newtype Tripletlist = Tripletlist
  { unwrapTripletlist :: IA.IncompleteArray (CA.ConstantArray 3 BG.CInt)
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype (IsA.IsArray)

instance ( ty ~ IA.IncompleteArray (CA.ConstantArray 3 BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapTripletlist" Tripletlist ty where

  hasField =
    \x0 ->
      ( \y1 -> Tripletlist {unwrapTripletlist = y1}
      , BG.getField @"unwrapTripletlist" x0
      )

instance ( ty ~ IA.IncompleteArray (CA.ConstantArray 3 BG.CInt)
         ) => BG.HasField "unwrapTripletlist" (BG.Ptr Tripletlist) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapTripletlist")

instance HasCField.HasCField Tripletlist "unwrapTripletlist" where

  type CFieldType Tripletlist "unwrapTripletlist" =
    IA.IncompleteArray (CA.ConstantArray 3 BG.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct Example@

    __defined at:__ @arrays\/array.h 49:8@

    __exported by:__ @arrays\/array.h@
-}
data Example = Example
  { example_triple :: CA.ConstantArray 3 BG.CInt
    {- ^ __C declaration:__ @triple@

         __defined at:__ @arrays\/array.h 50:9@

         __exported by:__ @arrays\/array.h@
    -}
  , example_sudoku :: CA.ConstantArray 3 (CA.ConstantArray 3 BG.CInt)
    {- ^ __C declaration:__ @sudoku@

         __defined at:__ @arrays\/array.h 51:9@

         __exported by:__ @arrays\/array.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Example where

  staticSizeOf = \_ -> (48 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Example where

  readRaw =
    \ptr0 ->
          pure Example
      <*> HasCField.readRaw (BG.Proxy @"example_triple") ptr0
      <*> HasCField.readRaw (BG.Proxy @"example_sudoku") ptr0

instance Marshal.WriteRaw Example where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Example example_triple2 example_sudoku3 ->
               HasCField.writeRaw (BG.Proxy @"example_triple") ptr0 example_triple2
            >> HasCField.writeRaw (BG.Proxy @"example_sudoku") ptr0 example_sudoku3

deriving via Marshal.EquivStorable Example instance BG.Storable Example

deriving via Struct.IsStructViaStorable Example instance Struct.IsStruct Example

{-| __C declaration:__ @triple@

    __defined at:__ @arrays\/array.h 50:9@

    __exported by:__ @arrays\/array.h@
-}
instance ( ty ~ CA.ConstantArray 3 BG.CInt
         ) => BG.CompatHasField.HasField "example_triple" Example ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Example {example_triple = y1, example_sudoku = BG.getField @"example_sudoku" x0}
      , BG.getField @"example_triple" x0
      )

instance ( ty ~ CA.ConstantArray 3 BG.CInt
         ) => BG.HasField "example_triple" (BG.Ptr Example) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"example_triple")

instance HasCField.HasCField Example "example_triple" where

  type CFieldType Example "example_triple" =
    CA.ConstantArray 3 BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @sudoku@

    __defined at:__ @arrays\/array.h 51:9@

    __exported by:__ @arrays\/array.h@
-}
instance ( ty ~ CA.ConstantArray 3 (CA.ConstantArray 3 BG.CInt)
         ) => BG.CompatHasField.HasField "example_sudoku" Example ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Example {example_sudoku = y1, example_triple = BG.getField @"example_triple" x0}
      , BG.getField @"example_sudoku" x0
      )

instance ( ty ~ CA.ConstantArray 3 (CA.ConstantArray 3 BG.CInt)
         ) => BG.HasField "example_sudoku" (BG.Ptr Example) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"example_sudoku")

instance HasCField.HasCField Example "example_sudoku" where

  type CFieldType Example "example_sudoku" =
    CA.ConstantArray 3 (CA.ConstantArray 3 BG.CInt)

  offset# = \_ -> \_ -> 12

{-| Typedef-in-typedef.

    __C declaration:__ @sudoku@

    __defined at:__ @arrays\/array.h 55:17@

    __exported by:__ @arrays\/array.h@
-}
newtype Sudoku = Sudoku
  { unwrapSudoku :: CA.ConstantArray 3 Triplet
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ CA.ConstantArray 3 Triplet
         ) => BG.CompatHasField.HasField "unwrapSudoku" Sudoku ty where

  hasField =
    \x0 ->
      (\y1 ->
         Sudoku {unwrapSudoku = y1}, BG.getField @"unwrapSudoku" x0)

instance ( ty ~ CA.ConstantArray 3 Triplet
         ) => BG.HasField "unwrapSudoku" (BG.Ptr Sudoku) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapSudoku")

instance HasCField.HasCField Sudoku "unwrapSudoku" where

  type CFieldType Sudoku "unwrapSudoku" =
    CA.ConstantArray 3 Triplet

  offset# = \_ -> \_ -> 0
