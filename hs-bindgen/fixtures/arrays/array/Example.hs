{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @triplet@

    __defined at:__ @arrays\/array.h 41:13@

    __exported by:__ @arrays\/array.h@
-}
newtype Triplet = Triplet
  { unwrapTriplet :: (CA.ConstantArray 3) RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) ((CA.ConstantArray 3) RIP.CInt)
         ) => RIP.HasField "unwrapTriplet" (RIP.Ptr Triplet) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapTriplet")

instance HasCField.HasCField Triplet "unwrapTriplet" where

  type CFieldType Triplet "unwrapTriplet" =
    (CA.ConstantArray 3) RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @list@

    __defined at:__ @arrays\/array.h 43:13@

    __exported by:__ @arrays\/array.h@
-}
newtype List = List
  { unwrapList :: IA.IncompleteArray RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype (IsA.IsArray)

instance ( ((~) ty) (IA.IncompleteArray RIP.CInt)
         ) => RIP.HasField "unwrapList" (RIP.Ptr List) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapList")

instance HasCField.HasCField List "unwrapList" where

  type CFieldType List "unwrapList" =
    IA.IncompleteArray RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @matrix@

    __defined at:__ @arrays\/array.h 45:13@

    __exported by:__ @arrays\/array.h@
-}
newtype Matrix = Matrix
  { unwrapMatrix :: (CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt)
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) ((CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt))
         ) => RIP.HasField "unwrapMatrix" (RIP.Ptr Matrix) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMatrix")

instance HasCField.HasCField Matrix "unwrapMatrix" where

  type CFieldType Matrix "unwrapMatrix" =
    (CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @tripletlist@

    __defined at:__ @arrays\/array.h 47:13@

    __exported by:__ @arrays\/array.h@
-}
newtype Tripletlist = Tripletlist
  { unwrapTripletlist :: IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt)
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype (IsA.IsArray)

instance ( ((~) ty) (IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt))
         ) => RIP.HasField "unwrapTripletlist" (RIP.Ptr Tripletlist) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapTripletlist")

instance HasCField.HasCField Tripletlist "unwrapTripletlist" where

  type CFieldType Tripletlist "unwrapTripletlist" =
    IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct Example@

    __defined at:__ @arrays\/array.h 49:8@

    __exported by:__ @arrays\/array.h@
-}
data Example = Example
  { example_triple :: (CA.ConstantArray 3) RIP.CInt
    {- ^ __C declaration:__ @triple@

         __defined at:__ @arrays\/array.h 50:9@

         __exported by:__ @arrays\/array.h@
    -}
  , example_sudoku :: (CA.ConstantArray 3) ((CA.ConstantArray 3) RIP.CInt)
    {- ^ __C declaration:__ @sudoku@

         __defined at:__ @arrays\/array.h 51:9@

         __exported by:__ @arrays\/array.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Example where

  staticSizeOf = \_ -> (48 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Example where

  readRaw =
    \ptr0 ->
          pure Example
      <*> HasCField.readRaw (RIP.Proxy @"example_triple") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"example_sudoku") ptr0

instance Marshal.WriteRaw Example where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Example example_triple2 example_sudoku3 ->
               HasCField.writeRaw (RIP.Proxy @"example_triple") ptr0 example_triple2
            >> HasCField.writeRaw (RIP.Proxy @"example_sudoku") ptr0 example_sudoku3

deriving via Marshal.EquivStorable Example instance RIP.Storable Example

instance HasCField.HasCField Example "example_triple" where

  type CFieldType Example "example_triple" =
    (CA.ConstantArray 3) RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) ((CA.ConstantArray 3) RIP.CInt)
         ) => RIP.HasField "example_triple" (RIP.Ptr Example) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"example_triple")

instance HasCField.HasCField Example "example_sudoku" where

  type CFieldType Example "example_sudoku" =
    (CA.ConstantArray 3) ((CA.ConstantArray 3) RIP.CInt)

  offset# = \_ -> \_ -> 12

instance ( ((~) ty) ((CA.ConstantArray 3) ((CA.ConstantArray 3) RIP.CInt))
         ) => RIP.HasField "example_sudoku" (RIP.Ptr Example) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"example_sudoku")

{-| Typedef-in-typedef

__C declaration:__ @sudoku@

__defined at:__ @arrays\/array.h 55:17@

__exported by:__ @arrays\/array.h@
-}
newtype Sudoku = Sudoku
  { unwrapSudoku :: (CA.ConstantArray 3) Triplet
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) ((CA.ConstantArray 3) Triplet)
         ) => RIP.HasField "unwrapSudoku" (RIP.Ptr Sudoku) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapSudoku")

instance HasCField.HasCField Sudoku "unwrapSudoku" where

  type CFieldType Sudoku "unwrapSudoku" =
    (CA.ConstantArray 3) Triplet

  offset# = \_ -> \_ -> 0
