{-# OPTIONS_GHC -Wno-orphans #-}

-- | Enum instances (requires the help of the @hsc2hs@ preprocessor)
--
-- Since we get no HLS support in modules that are preprocessed, we use a
-- separate module for these instances. Technically speaking this results in
-- orphans, but the benefit of defining the Haskell definitions separately is
-- that when working on the high-level Clang API, we can "jump to definition"
-- for the enums and land in a regular Haskell source file rather than the
-- result of the @hsc2hs@ preprocessor.
--
-- This module should only be imported by "HsBingen.Clang.LowLevel".
module HsBindgen.Clang.Core.Instances () where

import HsBindgen.Clang.Core.Enums
import HsBindgen.Patterns

#include <clang-c/Index.h>

{-------------------------------------------------------------------------------
  CXTranslationUnit_Flag
-------------------------------------------------------------------------------}

instance IsSingleFlag CXTranslationUnit_Flag where
  flagToC CXTranslationUnit_None                                 = #const CXTranslationUnit_None
  flagToC CXTranslationUnit_DetailedPreprocessingRecord          = #const CXTranslationUnit_DetailedPreprocessingRecord
  flagToC CXTranslationUnit_Incomplete                           = #const CXTranslationUnit_Incomplete
  flagToC CXTranslationUnit_PrecompiledPreamble                  = #const CXTranslationUnit_PrecompiledPreamble
  flagToC CXTranslationUnit_CacheCompletionResults               = #const CXTranslationUnit_CacheCompletionResults
  flagToC CXTranslationUnit_ForSerialization                     = #const CXTranslationUnit_ForSerialization
  flagToC CXTranslationUnit_SkipFunctionBodies                   = #const CXTranslationUnit_SkipFunctionBodies
  flagToC CXTranslationUnit_IncludeBriefCommentsInCodeCompletion = #const CXTranslationUnit_IncludeBriefCommentsInCodeCompletion
  flagToC CXTranslationUnit_CreatePreambleOnFirstParse           = #const CXTranslationUnit_CreatePreambleOnFirstParse
  flagToC CXTranslationUnit_KeepGoing                            = #const CXTranslationUnit_KeepGoing
  flagToC CXTranslationUnit_SingleFileParse                      = #const CXTranslationUnit_SingleFileParse
  flagToC CXTranslationUnit_LimitSkipFunctionBodiesToPreamble    = #const CXTranslationUnit_LimitSkipFunctionBodiesToPreamble
  flagToC CXTranslationUnit_IncludeAttributedTypes               = #const CXTranslationUnit_IncludeAttributedTypes
  flagToC CXTranslationUnit_VisitImplicitAttributes              = #const CXTranslationUnit_VisitImplicitAttributes
  flagToC CXTranslationUnit_IgnoreNonErrorsFromIncludedFiles     = #const CXTranslationUnit_IgnoreNonErrorsFromIncludedFiles
  flagToC CXTranslationUnit_RetainExcludedConditionalBlocks      = #const CXTranslationUnit_RetainExcludedConditionalBlocks

{-------------------------------------------------------------------------------
  CXTypeKind
-------------------------------------------------------------------------------}

instance IsSimpleEnum CXTypeKind where
  simpleToC CXType_Invalid             = #const CXType_Invalid
  simpleToC CXType_Unexposed           = #const CXType_Unexposed
  simpleToC CXType_Void                = #const CXType_Void
  simpleToC CXType_Bool                = #const CXType_Bool
  simpleToC CXType_Char_U              = #const CXType_Char_U
  simpleToC CXType_UChar               = #const CXType_UChar
  simpleToC CXType_Char16              = #const CXType_Char16
  simpleToC CXType_Char32              = #const CXType_Char32
  simpleToC CXType_UShort              = #const CXType_UShort
  simpleToC CXType_UInt                = #const CXType_UInt
  simpleToC CXType_ULong               = #const CXType_ULong
  simpleToC CXType_ULongLong           = #const CXType_ULongLong
  simpleToC CXType_UInt128             = #const CXType_UInt128
  simpleToC CXType_Char_S              = #const CXType_Char_S
  simpleToC CXType_SChar               = #const CXType_SChar
  simpleToC CXType_WChar               = #const CXType_WChar
  simpleToC CXType_Short               = #const CXType_Short
  simpleToC CXType_Int                 = #const CXType_Int
  simpleToC CXType_Long                = #const CXType_Long
  simpleToC CXType_LongLong            = #const CXType_LongLong
  simpleToC CXType_Int128              = #const CXType_Int128
  simpleToC CXType_Float               = #const CXType_Float
  simpleToC CXType_Double              = #const CXType_Double
  simpleToC CXType_LongDouble          = #const CXType_LongDouble
  simpleToC CXType_NullPtr             = #const CXType_NullPtr
  simpleToC CXType_Overload            = #const CXType_Overload
  simpleToC CXType_Dependent           = #const CXType_Dependent
  simpleToC CXType_ObjCId              = #const CXType_ObjCId
  simpleToC CXType_ObjCClass           = #const CXType_ObjCClass
  simpleToC CXType_ObjCSel             = #const CXType_ObjCSel
  simpleToC CXType_Float128            = #const CXType_Float128
  simpleToC CXType_Half                = #const CXType_Half
  simpleToC CXType_Float16             = #const CXType_Float16
  simpleToC CXType_ShortAccum          = #const CXType_ShortAccum
  simpleToC CXType_Accum               = #const CXType_Accum
  simpleToC CXType_LongAccum           = #const CXType_LongAccum
  simpleToC CXType_UShortAccum         = #const CXType_UShortAccum
  simpleToC CXType_UAccum              = #const CXType_UAccum
  simpleToC CXType_ULongAccum          = #const CXType_ULongAccum
  simpleToC CXType_BFloat16            = #const CXType_BFloat16
  simpleToC CXType_Ibm128              = #const CXType_Ibm128
  simpleToC CXType_Complex             = #const CXType_Complex
  simpleToC CXType_Pointer             = #const CXType_Pointer
  simpleToC CXType_BlockPointer        = #const CXType_BlockPointer
  simpleToC CXType_LValueReference     = #const CXType_LValueReference
  simpleToC CXType_RValueReference     = #const CXType_RValueReference
  simpleToC CXType_Record              = #const CXType_Record
  simpleToC CXType_Enum                = #const CXType_Enum
  simpleToC CXType_Typedef             = #const CXType_Typedef
  simpleToC CXType_ObjCInterface       = #const CXType_ObjCInterface
  simpleToC CXType_ObjCObjectPointer   = #const CXType_ObjCObjectPointer
  simpleToC CXType_FunctionNoProto     = #const CXType_FunctionNoProto
  simpleToC CXType_FunctionProto       = #const CXType_FunctionProto
  simpleToC CXType_ConstantArray       = #const CXType_ConstantArray
  simpleToC CXType_Vector              = #const CXType_Vector
  simpleToC CXType_IncompleteArray     = #const CXType_IncompleteArray
  simpleToC CXType_VariableArray       = #const CXType_VariableArray
  simpleToC CXType_DependentSizedArray = #const CXType_DependentSizedArray
  simpleToC CXType_MemberPointer       = #const CXType_MemberPointer
  simpleToC CXType_Auto                = #const CXType_Auto
  simpleToC CXType_Elaborated          = #const CXType_Elaborated

  simpleFromC (#const CXType_Invalid)             = Just CXType_Invalid
  simpleFromC (#const CXType_Unexposed)           = Just CXType_Unexposed
  simpleFromC (#const CXType_Void)                = Just CXType_Void
  simpleFromC (#const CXType_Bool)                = Just CXType_Bool
  simpleFromC (#const CXType_Char_U)              = Just CXType_Char_U
  simpleFromC (#const CXType_UChar)               = Just CXType_UChar
  simpleFromC (#const CXType_Char16)              = Just CXType_Char16
  simpleFromC (#const CXType_Char32)              = Just CXType_Char32
  simpleFromC (#const CXType_UShort)              = Just CXType_UShort
  simpleFromC (#const CXType_UInt)                = Just CXType_UInt
  simpleFromC (#const CXType_ULong)               = Just CXType_ULong
  simpleFromC (#const CXType_ULongLong)           = Just CXType_ULongLong
  simpleFromC (#const CXType_UInt128)             = Just CXType_UInt128
  simpleFromC (#const CXType_Char_S)              = Just CXType_Char_S
  simpleFromC (#const CXType_SChar)               = Just CXType_SChar
  simpleFromC (#const CXType_WChar)               = Just CXType_WChar
  simpleFromC (#const CXType_Short)               = Just CXType_Short
  simpleFromC (#const CXType_Int)                 = Just CXType_Int
  simpleFromC (#const CXType_Long)                = Just CXType_Long
  simpleFromC (#const CXType_LongLong)            = Just CXType_LongLong
  simpleFromC (#const CXType_Int128)              = Just CXType_Int128
  simpleFromC (#const CXType_Float)               = Just CXType_Float
  simpleFromC (#const CXType_Double)              = Just CXType_Double
  simpleFromC (#const CXType_LongDouble)          = Just CXType_LongDouble
  simpleFromC (#const CXType_NullPtr)             = Just CXType_NullPtr
  simpleFromC (#const CXType_Overload)            = Just CXType_Overload
  simpleFromC (#const CXType_Dependent)           = Just CXType_Dependent
  simpleFromC (#const CXType_ObjCId)              = Just CXType_ObjCId
  simpleFromC (#const CXType_ObjCClass)           = Just CXType_ObjCClass
  simpleFromC (#const CXType_ObjCSel)             = Just CXType_ObjCSel
  simpleFromC (#const CXType_Float128)            = Just CXType_Float128
  simpleFromC (#const CXType_Half)                = Just CXType_Half
  simpleFromC (#const CXType_Float16)             = Just CXType_Float16
  simpleFromC (#const CXType_ShortAccum)          = Just CXType_ShortAccum
  simpleFromC (#const CXType_Accum)               = Just CXType_Accum
  simpleFromC (#const CXType_LongAccum)           = Just CXType_LongAccum
  simpleFromC (#const CXType_UShortAccum)         = Just CXType_UShortAccum
  simpleFromC (#const CXType_UAccum)              = Just CXType_UAccum
  simpleFromC (#const CXType_ULongAccum)          = Just CXType_ULongAccum
  simpleFromC (#const CXType_BFloat16)            = Just CXType_BFloat16
  simpleFromC (#const CXType_Ibm128)              = Just CXType_Ibm128
  simpleFromC (#const CXType_Complex)             = Just CXType_Complex
  simpleFromC (#const CXType_Pointer)             = Just CXType_Pointer
  simpleFromC (#const CXType_BlockPointer)        = Just CXType_BlockPointer
  simpleFromC (#const CXType_LValueReference)     = Just CXType_LValueReference
  simpleFromC (#const CXType_RValueReference)     = Just CXType_RValueReference
  simpleFromC (#const CXType_Record)              = Just CXType_Record
  simpleFromC (#const CXType_Enum)                = Just CXType_Enum
  simpleFromC (#const CXType_Typedef)             = Just CXType_Typedef
  simpleFromC (#const CXType_ObjCInterface)       = Just CXType_ObjCInterface
  simpleFromC (#const CXType_ObjCObjectPointer)   = Just CXType_ObjCObjectPointer
  simpleFromC (#const CXType_FunctionNoProto)     = Just CXType_FunctionNoProto
  simpleFromC (#const CXType_FunctionProto)       = Just CXType_FunctionProto
  simpleFromC (#const CXType_ConstantArray)       = Just CXType_ConstantArray
  simpleFromC (#const CXType_Vector)              = Just CXType_Vector
  simpleFromC (#const CXType_IncompleteArray)     = Just CXType_IncompleteArray
  simpleFromC (#const CXType_VariableArray)       = Just CXType_VariableArray
  simpleFromC (#const CXType_DependentSizedArray) = Just CXType_DependentSizedArray
  simpleFromC (#const CXType_MemberPointer)       = Just CXType_MemberPointer
  simpleFromC (#const CXType_Auto)                = Just CXType_Auto
  simpleFromC (#const CXType_Elaborated)          = Just CXType_Elaborated

  simpleFromC _otherwise = Nothing

{-------------------------------------------------------------------------------
  CXChildVisitResult
-------------------------------------------------------------------------------}

instance IsSimpleEnum CXChildVisitResult where
  simpleToC CXChildVisit_Break    = #const CXChildVisit_Break
  simpleToC CXChildVisit_Continue = #const CXChildVisit_Continue
  simpleToC CXChildVisit_Recurse  = #const CXChildVisit_Recurse

  simpleFromC (#const CXChildVisit_Break)    = Just CXChildVisit_Break
  simpleFromC (#const CXChildVisit_Continue) = Just CXChildVisit_Continue
  simpleFromC (#const CXChildVisit_Recurse)  = Just CXChildVisit_Recurse

  simpleFromC _otherwise = Nothing

{-------------------------------------------------------------------------------
  CXTypeLayoutError
-------------------------------------------------------------------------------}

instance IsSimpleEnum CXTypeLayoutError where
  simpleToC CXTypeLayoutError_Invalid          = #const CXTypeLayoutError_Invalid
  simpleToC CXTypeLayoutError_Incomplete       = #const CXTypeLayoutError_Incomplete
  simpleToC CXTypeLayoutError_Dependent        = #const CXTypeLayoutError_Dependent
  simpleToC CXTypeLayoutError_NotConstantSize  = #const CXTypeLayoutError_NotConstantSize
  simpleToC CXTypeLayoutError_InvalidFieldName = #const CXTypeLayoutError_InvalidFieldName
  simpleToC CXTypeLayoutError_Undeduced        = #const CXTypeLayoutError_Undeduced

  simpleFromC (#const CXTypeLayoutError_Invalid)          = Just CXTypeLayoutError_Invalid
  simpleFromC (#const CXTypeLayoutError_Incomplete)       = Just CXTypeLayoutError_Incomplete
  simpleFromC (#const CXTypeLayoutError_Dependent)        = Just CXTypeLayoutError_Dependent
  simpleFromC (#const CXTypeLayoutError_NotConstantSize)  = Just CXTypeLayoutError_NotConstantSize
  simpleFromC (#const CXTypeLayoutError_InvalidFieldName) = Just CXTypeLayoutError_InvalidFieldName
  simpleFromC (#const CXTypeLayoutError_Undeduced)        = Just CXTypeLayoutError_Undeduced

  simpleFromC _otherwise = Nothing
