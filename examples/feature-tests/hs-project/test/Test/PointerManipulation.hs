module Test.PointerManipulation (
    tests
  ) where

import Data.Constraint (Dict (Dict))
import Foreign.C.Types (CChar, CInt, CUChar, CUInt)
import Foreign.Ptr (Ptr)
import GHC.Records (HasField)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (expectFailure, testProperty)

import HsBindgen.Runtime.BitfieldPtr (BitfieldPtr)
import HsBindgen.Runtime.ConstantArray (ConstantArray)
import HsBindgen.Runtime.IncompleteArray (IncompleteArray)

import Generated.PointerManipulation qualified as Types
import Test.Util (dictProperty, type ($))

tests :: TestTree
tests = testGroup "Test.PointerManipulation" [
      -- Structs
      testProperty "dict_MyStruct_hasField_x" $
        dictProperty dict_hasField_MyStruct_x
    , testProperty "dict_MyStruct_hasField_y" $
        dictProperty dict_hasField_MyStruct_y

      -- Unions
    , testProperty "dict_hasField_MyUnion_x" $
        dictProperty dict_hasField_MyUnion_x
    , testProperty "dict_hasField_MyUnion_y" $
        dictProperty dict_hasField_MyUnion_y

      -- Enums
    , testProperty "dict_hasField_MyEnum_unwrap" $
        dictProperty dict_hasField_MyEnum_unwrap

      -- Bit-fields
    , testProperty "dict_hasField_MyStructBF_x" $
        dictProperty dict_hasField_MyStructBF_x
    , testProperty "dict_hasField_MyStructBF_y" $
        dictProperty dict_hasField_MyStructBF_y
    , testProperty "dict_hasField_MyUnionBF_x" $
        dictProperty dict_hasField_MyUnionBF_x
    , testProperty "dict_hasField_MyUnionBF_y" $
        dictProperty dict_hasField_MyUnionBF_y

      -- Typedefs
    , testProperty "dict_hasField_MyTypedef_unwrap" $
        dictProperty dict_hasField_MyTypedef_unwrap

      -- Arrays
    , testProperty "dict_hasField_MyArrayKnownSize_unwrap" $
        dictProperty dict_hasField_MyArrayKnownSize_unwrap
    , testProperty "dict_hasField_ConstantArray_toFirstElemPtr" $
        dictProperty dict_hasField_ConstantArray_toFirstElemPtr
    , testProperty "dict_hasField_MyArrayUnknownSize_unwrap" $
        dictProperty dict_hasField_MyArrayUnknownSize_unwrap
    , testProperty "dict_hasField_IncompleteArray_toFirstElemPtr" $
        dictProperty dict_hasField_IncompleteArray_toFirstElemPtr

      -- FLAM
    , testProperty "dict_hasField_MyStructFLAM_len" $
        expectFailure $ dictProperty dict_hasField_MyStructFLAM_len
    , testProperty "dict_hasField_MyStructFLAM_data" $
        expectFailure $ dictProperty dict_hasField_MyStructFLAM_data
    , testProperty "dict_hasField_MyStructFLAM_Aux_len" $
        dictProperty dict_hasField_MyStructFLAM_Aux_len

      -- Anonymous structs/unions
    , testProperty "dict_hasField_MyStructAnon_x" $
        dictProperty dict_hasField_MyStructAnon_x
    , testProperty "dict_hasField_MyUnionAnon_x" $
        dictProperty dict_hasField_MyUnionAnon_x
    ]

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

dict_hasField_MyStruct_x :: Dict $
     HasField "x" (Ptr Types.MyStruct) (Ptr CInt)
dict_hasField_MyStruct_x = Dict

dict_hasField_MyStruct_y :: Dict $
     HasField "y" (Ptr Types.MyStruct) (Ptr CChar)
dict_hasField_MyStruct_y = Dict

{-------------------------------------------------------------------------------
  Unions
-------------------------------------------------------------------------------}

dict_hasField_MyUnion_x :: Dict $
     HasField "x" (Ptr Types.MyUnion) (Ptr CInt)
dict_hasField_MyUnion_x = Dict

dict_hasField_MyUnion_y :: Dict $
     HasField "y" (Ptr Types.MyUnion) (Ptr CChar)
dict_hasField_MyUnion_y = Dict

{-------------------------------------------------------------------------------
  Enums
-------------------------------------------------------------------------------}

dict_hasField_MyEnum_unwrap :: Dict $
     HasField "unwrap" (Ptr Types.MyEnum) (Ptr CUInt)
dict_hasField_MyEnum_unwrap = Dict

{-------------------------------------------------------------------------------
  Bit-fields
-------------------------------------------------------------------------------}

dict_hasField_MyStructBF_x :: Dict $
     HasField "x" (Ptr Types.MyStructBF) (BitfieldPtr CUInt)
dict_hasField_MyStructBF_x = Dict

dict_hasField_MyStructBF_y :: Dict $
     HasField "y" (Ptr Types.MyStructBF) (BitfieldPtr CUChar)
dict_hasField_MyStructBF_y = Dict

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1253>: It should be
-- @BitfieldPtr CInt@ instead of @Ptr CInt@
dict_hasField_MyUnionBF_x :: Dict $
     HasField "x" (Ptr Types.MyUnionBF) (Ptr CUInt)
dict_hasField_MyUnionBF_x = Dict

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1253>: It should be
-- @BitfieldPtr CChar@ instead of @Ptr CChar@
dict_hasField_MyUnionBF_y :: Dict $
     HasField "y" (Ptr Types.MyUnionBF) (Ptr CUChar)
dict_hasField_MyUnionBF_y = Dict

{-------------------------------------------------------------------------------
  Typedefs
-------------------------------------------------------------------------------}

dict_hasField_MyTypedef_unwrap :: Dict $
     HasField "unwrap" (Ptr Types.MyTypedef) (Ptr CInt)
dict_hasField_MyTypedef_unwrap = Dict

{-------------------------------------------------------------------------------
  Arrays
-------------------------------------------------------------------------------}

dict_hasField_MyArrayKnownSize_unwrap :: Dict $
     HasField "unwrap" (Ptr Types.MyArrayKnownSize) (Ptr (ConstantArray 3 CInt))
dict_hasField_MyArrayKnownSize_unwrap = Dict

dict_hasField_ConstantArray_toFirstElemPtr :: Dict $
     HasField "toFirstElemPtr" (Ptr (ConstantArray 3 CInt)) (Ptr CInt)
dict_hasField_ConstantArray_toFirstElemPtr = Dict

dict_hasField_MyArrayUnknownSize_unwrap :: Dict $
     HasField "unwrap" (Ptr Types.MyArrayUnknownSize) (Ptr (IncompleteArray CInt))
dict_hasField_MyArrayUnknownSize_unwrap = Dict

dict_hasField_IncompleteArray_toFirstElemPtr :: Dict $
     HasField "toFirstElemPtr" (Ptr (IncompleteArray CInt)) (Ptr CInt)
dict_hasField_IncompleteArray_toFirstElemPtr = Dict

{-------------------------------------------------------------------------------
  FLAM
-------------------------------------------------------------------------------}

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1760>: Implement
-- support for strucs with FLAMs in the pointer manipulation API.
dict_hasField_MyStructFLAM_len :: Dict $
     HasField "len" (Ptr Types.MyStructFLAM) (Ptr CInt)
dict_hasField_MyStructFLAM_len =
    error "the pointer manipulation API does not support structs with FLAMs"

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1760>: Implement
-- support for strucs with FLAMs in the pointer manipulation API.
dict_hasField_MyStructFLAM_data :: Dict $
     HasField "data" (Ptr Types.MyStructFLAM) (Ptr (IncompleteArray CChar))
dict_hasField_MyStructFLAM_data =
    error "the pointer manipulation API does not support structs with FLAMs"

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1760>: Implement
-- support for strucs with FLAMs in the pointer manipulation API. Also, should
-- the field name be "myStructFLAM_Aux_len" instead of "myStructFLAM_len"? That
-- would be more consistent with the usual field naming approach.
dict_hasField_MyStructFLAM_Aux_len :: Dict $
     HasField "len" (Ptr Types.MyStructFLAM_Aux) (Ptr CInt)
dict_hasField_MyStructFLAM_Aux_len = Dict

{-------------------------------------------------------------------------------
  Anonymous structs/unions
-------------------------------------------------------------------------------}

dict_hasField_MyStructAnon_x :: Dict $
     HasField "x" (Ptr Types.MyStructAnon) (Ptr Types.MyStructAnon_x)
dict_hasField_MyStructAnon_x = Dict

dict_hasField_MyUnionAnon_x :: Dict $
     HasField "x" (Ptr Types.MyUnionAnon) (Ptr Types.MyUnionAnon_x)
dict_hasField_MyUnionAnon_x = Dict
