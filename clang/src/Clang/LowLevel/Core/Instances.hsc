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
-- This module should only be imported by "Clang.LowLevel".
module Clang.LowLevel.Core.Instances () where

import Clang.Enum.Bitfield
import Clang.Enum.Simple
import Clang.Internal.ByValue
import Clang.LowLevel.Core.Enums
import Clang.LowLevel.Core.Structs

#include <clang-c/Index.h>
#include "clang_wrappers.h"

{-------------------------------------------------------------------------------
  HasKnownSize instances
-------------------------------------------------------------------------------}

instance HasKnownSize CXCursor_         where knownSize = #size CXCursor
instance HasKnownSize CXSourceLocation_ where knownSize = #size CXSourceLocation
instance HasKnownSize CXSourceRange_    where knownSize = #size CXSourceRange
instance HasKnownSize CXString_         where knownSize = #size CXString
instance HasKnownSize CXToken_          where knownSize = #size CXToken
instance HasKnownSize CXType_           where knownSize = #size CXType

{-------------------------------------------------------------------------------
  CXErrorCode
-------------------------------------------------------------------------------}

instance IsSimpleEnum CXErrorCode where
  simpleToC CXError_Failure          = #const CXError_Failure
  simpleToC CXError_Crashed          = #const CXError_Crashed
  simpleToC CXError_InvalidArguments = #const CXError_InvalidArguments
  simpleToC CXError_ASTReadError     = #const CXError_ASTReadError

  simpleFromC (#const CXError_Failure)          = Just CXError_Failure
  simpleFromC (#const CXError_Crashed)          = Just CXError_Crashed
  simpleFromC (#const CXError_InvalidArguments) = Just CXError_InvalidArguments
  simpleFromC (#const CXError_ASTReadError)     = Just CXError_ASTReadError

  simpleFromC _ = Nothing

instance IsSimpleEnum (Maybe CXErrorCode) where
  simpleToC Nothing   = #const CXError_Success
  simpleToC (Just hs) = simpleToC hs

  simpleFromC (#const CXError_Success) = Just Nothing
  simpleFromC c                        = Just <$> simpleFromC c

{-------------------------------------------------------------------------------
  CXTranslationUnit_Flag
-------------------------------------------------------------------------------}

instance IsSingleFlag CXTranslationUnit_Flags where
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
  simpleToC CXType_ObjCObject          = #const CXType_ObjCObject
  simpleToC CXType_ObjCTypeParam       = #const CXType_ObjCTypeParam
  simpleToC CXType_Attributed          = #const CXType_Attributed
  simpleToC CXType_ExtVector           = #const CXType_ExtVector
  simpleToC CXType_Atomic              = #const CXType_Atomic

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
  simpleFromC (#const CXType_ObjCObject)          = Just CXType_ObjCObject
  simpleFromC (#const CXType_ObjCTypeParam)       = Just CXType_ObjCTypeParam
  simpleFromC (#const CXType_Attributed)          = Just CXType_Attributed
  simpleFromC (#const CXType_ExtVector)           = Just CXType_ExtVector
  simpleFromC (#const CXType_Atomic)              = Just CXType_Atomic

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

{-------------------------------------------------------------------------------
  CXTokenKind
-------------------------------------------------------------------------------}

instance IsSimpleEnum CXTokenKind where
  simpleToC CXToken_Punctuation = #const CXToken_Punctuation
  simpleToC CXToken_Keyword     = #const CXToken_Keyword
  simpleToC CXToken_Identifier  = #const CXToken_Identifier
  simpleToC CXToken_Literal     = #const CXToken_Literal
  simpleToC CXToken_Comment     = #const CXToken_Comment

  simpleFromC (#const CXToken_Punctuation) = Just CXToken_Punctuation
  simpleFromC (#const CXToken_Keyword)     = Just CXToken_Keyword
  simpleFromC (#const CXToken_Identifier)  = Just CXToken_Identifier
  simpleFromC (#const CXToken_Literal)     = Just CXToken_Literal
  simpleFromC (#const CXToken_Comment)     = Just CXToken_Comment

  simpleFromC _otherwise = Nothing

{-------------------------------------------------------------------------------
  CXCursorKind
-------------------------------------------------------------------------------}

instance IsSimpleEnum CXCursorKind where
  simpleToC CXCursor_UnexposedDecl                                    = #const CXCursor_UnexposedDecl
  simpleToC CXCursor_StructDecl                                       = #const CXCursor_StructDecl
  simpleToC CXCursor_UnionDecl                                        = #const CXCursor_UnionDecl
  simpleToC CXCursor_ClassDecl                                        = #const CXCursor_ClassDecl
  simpleToC CXCursor_EnumDecl                                         = #const CXCursor_EnumDecl
  simpleToC CXCursor_FieldDecl                                        = #const CXCursor_FieldDecl
  simpleToC CXCursor_EnumConstantDecl                                 = #const CXCursor_EnumConstantDecl
  simpleToC CXCursor_FunctionDecl                                     = #const CXCursor_FunctionDecl
  simpleToC CXCursor_VarDecl                                          = #const CXCursor_VarDecl
  simpleToC CXCursor_ParmDecl                                         = #const CXCursor_ParmDecl
  simpleToC CXCursor_ObjCInterfaceDecl                                = #const CXCursor_ObjCInterfaceDecl
  simpleToC CXCursor_ObjCCategoryDecl                                 = #const CXCursor_ObjCCategoryDecl
  simpleToC CXCursor_ObjCProtocolDecl                                 = #const CXCursor_ObjCProtocolDecl
  simpleToC CXCursor_ObjCPropertyDecl                                 = #const CXCursor_ObjCPropertyDecl
  simpleToC CXCursor_ObjCIvarDecl                                     = #const CXCursor_ObjCIvarDecl
  simpleToC CXCursor_ObjCInstanceMethodDecl                           = #const CXCursor_ObjCInstanceMethodDecl
  simpleToC CXCursor_ObjCClassMethodDecl                              = #const CXCursor_ObjCClassMethodDecl
  simpleToC CXCursor_ObjCImplementationDecl                           = #const CXCursor_ObjCImplementationDecl
  simpleToC CXCursor_ObjCCategoryImplDecl                             = #const CXCursor_ObjCCategoryImplDecl
  simpleToC CXCursor_TypedefDecl                                      = #const CXCursor_TypedefDecl
  simpleToC CXCursor_CXXMethod                                        = #const CXCursor_CXXMethod
  simpleToC CXCursor_Namespace                                        = #const CXCursor_Namespace
  simpleToC CXCursor_LinkageSpec                                      = #const CXCursor_LinkageSpec
  simpleToC CXCursor_Constructor                                      = #const CXCursor_Constructor
  simpleToC CXCursor_Destructor                                       = #const CXCursor_Destructor
  simpleToC CXCursor_ConversionFunction                               = #const CXCursor_ConversionFunction
  simpleToC CXCursor_TemplateTypeParameter                            = #const CXCursor_TemplateTypeParameter
  simpleToC CXCursor_NonTypeTemplateParameter                         = #const CXCursor_NonTypeTemplateParameter
  simpleToC CXCursor_TemplateTemplateParameter                        = #const CXCursor_TemplateTemplateParameter
  simpleToC CXCursor_FunctionTemplate                                 = #const CXCursor_FunctionTemplate
  simpleToC CXCursor_ClassTemplate                                    = #const CXCursor_ClassTemplate
  simpleToC CXCursor_ClassTemplatePartialSpecialization               = #const CXCursor_ClassTemplatePartialSpecialization
  simpleToC CXCursor_NamespaceAlias                                   = #const CXCursor_NamespaceAlias
  simpleToC CXCursor_UsingDirective                                   = #const CXCursor_UsingDirective
  simpleToC CXCursor_UsingDeclaration                                 = #const CXCursor_UsingDeclaration
  simpleToC CXCursor_TypeAliasDecl                                    = #const CXCursor_TypeAliasDecl
  simpleToC CXCursor_ObjCSynthesizeDecl                               = #const CXCursor_ObjCSynthesizeDecl
  simpleToC CXCursor_ObjCDynamicDecl                                  = #const CXCursor_ObjCDynamicDecl
  simpleToC CXCursor_CXXAccessSpecifier                               = #const CXCursor_CXXAccessSpecifier
  simpleToC CXCursor_ObjCSuperClassRef                                = #const CXCursor_ObjCSuperClassRef
  simpleToC CXCursor_ObjCProtocolRef                                  = #const CXCursor_ObjCProtocolRef
  simpleToC CXCursor_ObjCClassRef                                     = #const CXCursor_ObjCClassRef
  simpleToC CXCursor_TypeRef                                          = #const CXCursor_TypeRef
  simpleToC CXCursor_CXXBaseSpecifier                                 = #const CXCursor_CXXBaseSpecifier
  simpleToC CXCursor_TemplateRef                                      = #const CXCursor_TemplateRef
  simpleToC CXCursor_NamespaceRef                                     = #const CXCursor_NamespaceRef
  simpleToC CXCursor_MemberRef                                        = #const CXCursor_MemberRef
  simpleToC CXCursor_LabelRef                                         = #const CXCursor_LabelRef
  simpleToC CXCursor_OverloadedDeclRef                                = #const CXCursor_OverloadedDeclRef
  simpleToC CXCursor_VariableRef                                      = #const CXCursor_VariableRef
  simpleToC CXCursor_InvalidFile                                      = #const CXCursor_InvalidFile
  simpleToC CXCursor_NoDeclFound                                      = #const CXCursor_NoDeclFound
  simpleToC CXCursor_NotImplemented                                   = #const CXCursor_NotImplemented
  simpleToC CXCursor_InvalidCode                                      = #const CXCursor_InvalidCode
  simpleToC CXCursor_UnexposedExpr                                    = #const CXCursor_UnexposedExpr
  simpleToC CXCursor_DeclRefExpr                                      = #const CXCursor_DeclRefExpr
  simpleToC CXCursor_MemberRefExpr                                    = #const CXCursor_MemberRefExpr
  simpleToC CXCursor_CallExpr                                         = #const CXCursor_CallExpr
  simpleToC CXCursor_ObjCMessageExpr                                  = #const CXCursor_ObjCMessageExpr
  simpleToC CXCursor_BlockExpr                                        = #const CXCursor_BlockExpr
  simpleToC CXCursor_IntegerLiteral                                   = #const CXCursor_IntegerLiteral
  simpleToC CXCursor_FloatingLiteral                                  = #const CXCursor_FloatingLiteral
  simpleToC CXCursor_ImaginaryLiteral                                 = #const CXCursor_ImaginaryLiteral
  simpleToC CXCursor_StringLiteral                                    = #const CXCursor_StringLiteral
  simpleToC CXCursor_CharacterLiteral                                 = #const CXCursor_CharacterLiteral
  simpleToC CXCursor_ParenExpr                                        = #const CXCursor_ParenExpr
  simpleToC CXCursor_UnaryOperator                                    = #const CXCursor_UnaryOperator
  simpleToC CXCursor_ArraySubscriptExpr                               = #const CXCursor_ArraySubscriptExpr
  simpleToC CXCursor_BinaryOperator                                   = #const CXCursor_BinaryOperator
  simpleToC CXCursor_CompoundAssignOperator                           = #const CXCursor_CompoundAssignOperator
  simpleToC CXCursor_ConditionalOperator                              = #const CXCursor_ConditionalOperator
  simpleToC CXCursor_CStyleCastExpr                                   = #const CXCursor_CStyleCastExpr
  simpleToC CXCursor_CompoundLiteralExpr                              = #const CXCursor_CompoundLiteralExpr
  simpleToC CXCursor_InitListExpr                                     = #const CXCursor_InitListExpr
  simpleToC CXCursor_AddrLabelExpr                                    = #const CXCursor_AddrLabelExpr
  simpleToC CXCursor_StmtExpr                                         = #const CXCursor_StmtExpr
  simpleToC CXCursor_GenericSelectionExpr                             = #const CXCursor_GenericSelectionExpr
  simpleToC CXCursor_GNUNullExpr                                      = #const CXCursor_GNUNullExpr
  simpleToC CXCursor_CXXStaticCastExpr                                = #const CXCursor_CXXStaticCastExpr
  simpleToC CXCursor_CXXDynamicCastExpr                               = #const CXCursor_CXXDynamicCastExpr
  simpleToC CXCursor_CXXReinterpretCastExpr                           = #const CXCursor_CXXReinterpretCastExpr
  simpleToC CXCursor_CXXConstCastExpr                                 = #const CXCursor_CXXConstCastExpr
  simpleToC CXCursor_CXXFunctionalCastExpr                            = #const CXCursor_CXXFunctionalCastExpr
  simpleToC CXCursor_CXXTypeidExpr                                    = #const CXCursor_CXXTypeidExpr
  simpleToC CXCursor_CXXBoolLiteralExpr                               = #const CXCursor_CXXBoolLiteralExpr
  simpleToC CXCursor_CXXNullPtrLiteralExpr                            = #const CXCursor_CXXNullPtrLiteralExpr
  simpleToC CXCursor_CXXThisExpr                                      = #const CXCursor_CXXThisExpr
  simpleToC CXCursor_CXXThrowExpr                                     = #const CXCursor_CXXThrowExpr
  simpleToC CXCursor_CXXNewExpr                                       = #const CXCursor_CXXNewExpr
  simpleToC CXCursor_CXXDeleteExpr                                    = #const CXCursor_CXXDeleteExpr
  simpleToC CXCursor_UnaryExpr                                        = #const CXCursor_UnaryExpr
  simpleToC CXCursor_ObjCStringLiteral                                = #const CXCursor_ObjCStringLiteral
  simpleToC CXCursor_ObjCEncodeExpr                                   = #const CXCursor_ObjCEncodeExpr
  simpleToC CXCursor_ObjCSelectorExpr                                 = #const CXCursor_ObjCSelectorExpr
  simpleToC CXCursor_ObjCProtocolExpr                                 = #const CXCursor_ObjCProtocolExpr
  simpleToC CXCursor_ObjCBridgedCastExpr                              = #const CXCursor_ObjCBridgedCastExpr
  simpleToC CXCursor_PackExpansionExpr                                = #const CXCursor_PackExpansionExpr
  simpleToC CXCursor_SizeOfPackExpr                                   = #const CXCursor_SizeOfPackExpr
  simpleToC CXCursor_LambdaExpr                                       = #const CXCursor_LambdaExpr
  simpleToC CXCursor_ObjCBoolLiteralExpr                              = #const CXCursor_ObjCBoolLiteralExpr
  simpleToC CXCursor_ObjCSelfExpr                                     = #const CXCursor_ObjCSelfExpr
  simpleToC CXCursor_ObjCAvailabilityCheckExpr                        = #const CXCursor_ObjCAvailabilityCheckExpr
  simpleToC CXCursor_FixedPointLiteral                                = #const CXCursor_FixedPointLiteral
  simpleToC CXCursor_OMPArrayShapingExpr                              = #const CXCursor_OMPArrayShapingExpr
  simpleToC CXCursor_OMPIteratorExpr                                  = #const CXCursor_OMPIteratorExpr
  simpleToC CXCursor_CXXAddrspaceCastExpr                             = #const CXCursor_CXXAddrspaceCastExpr
  simpleToC CXCursor_UnexposedStmt                                    = #const CXCursor_UnexposedStmt
  simpleToC CXCursor_LabelStmt                                        = #const CXCursor_LabelStmt
  simpleToC CXCursor_CompoundStmt                                     = #const CXCursor_CompoundStmt
  simpleToC CXCursor_CaseStmt                                         = #const CXCursor_CaseStmt
  simpleToC CXCursor_DefaultStmt                                      = #const CXCursor_DefaultStmt
  simpleToC CXCursor_IfStmt                                           = #const CXCursor_IfStmt
  simpleToC CXCursor_SwitchStmt                                       = #const CXCursor_SwitchStmt
  simpleToC CXCursor_WhileStmt                                        = #const CXCursor_WhileStmt
  simpleToC CXCursor_DoStmt                                           = #const CXCursor_DoStmt
  simpleToC CXCursor_ForStmt                                          = #const CXCursor_ForStmt
  simpleToC CXCursor_GotoStmt                                         = #const CXCursor_GotoStmt
  simpleToC CXCursor_IndirectGotoStmt                                 = #const CXCursor_IndirectGotoStmt
  simpleToC CXCursor_ContinueStmt                                     = #const CXCursor_ContinueStmt
  simpleToC CXCursor_BreakStmt                                        = #const CXCursor_BreakStmt
  simpleToC CXCursor_ReturnStmt                                       = #const CXCursor_ReturnStmt
  simpleToC CXCursor_GCCAsmStmt                                       = #const CXCursor_GCCAsmStmt
  simpleToC CXCursor_ObjCAtTryStmt                                    = #const CXCursor_ObjCAtTryStmt
  simpleToC CXCursor_ObjCAtCatchStmt                                  = #const CXCursor_ObjCAtCatchStmt
  simpleToC CXCursor_ObjCAtFinallyStmt                                = #const CXCursor_ObjCAtFinallyStmt
  simpleToC CXCursor_ObjCAtThrowStmt                                  = #const CXCursor_ObjCAtThrowStmt
  simpleToC CXCursor_ObjCAtSynchronizedStmt                           = #const CXCursor_ObjCAtSynchronizedStmt
  simpleToC CXCursor_ObjCAutoreleasePoolStmt                          = #const CXCursor_ObjCAutoreleasePoolStmt
  simpleToC CXCursor_ObjCForCollectionStmt                            = #const CXCursor_ObjCForCollectionStmt
  simpleToC CXCursor_CXXCatchStmt                                     = #const CXCursor_CXXCatchStmt
  simpleToC CXCursor_CXXTryStmt                                       = #const CXCursor_CXXTryStmt
  simpleToC CXCursor_CXXForRangeStmt                                  = #const CXCursor_CXXForRangeStmt
  simpleToC CXCursor_SEHTryStmt                                       = #const CXCursor_SEHTryStmt
  simpleToC CXCursor_SEHExceptStmt                                    = #const CXCursor_SEHExceptStmt
  simpleToC CXCursor_SEHFinallyStmt                                   = #const CXCursor_SEHFinallyStmt
  simpleToC CXCursor_MSAsmStmt                                        = #const CXCursor_MSAsmStmt
  simpleToC CXCursor_NullStmt                                         = #const CXCursor_NullStmt
  simpleToC CXCursor_DeclStmt                                         = #const CXCursor_DeclStmt
  simpleToC CXCursor_OMPParallelDirective                             = #const CXCursor_OMPParallelDirective
  simpleToC CXCursor_OMPSimdDirective                                 = #const CXCursor_OMPSimdDirective
  simpleToC CXCursor_OMPForDirective                                  = #const CXCursor_OMPForDirective
  simpleToC CXCursor_OMPSectionsDirective                             = #const CXCursor_OMPSectionsDirective
  simpleToC CXCursor_OMPSectionDirective                              = #const CXCursor_OMPSectionDirective
  simpleToC CXCursor_OMPSingleDirective                               = #const CXCursor_OMPSingleDirective
  simpleToC CXCursor_OMPParallelForDirective                          = #const CXCursor_OMPParallelForDirective
  simpleToC CXCursor_OMPParallelSectionsDirective                     = #const CXCursor_OMPParallelSectionsDirective
  simpleToC CXCursor_OMPTaskDirective                                 = #const CXCursor_OMPTaskDirective
  simpleToC CXCursor_OMPMasterDirective                               = #const CXCursor_OMPMasterDirective
  simpleToC CXCursor_OMPCriticalDirective                             = #const CXCursor_OMPCriticalDirective
  simpleToC CXCursor_OMPTaskyieldDirective                            = #const CXCursor_OMPTaskyieldDirective
  simpleToC CXCursor_OMPBarrierDirective                              = #const CXCursor_OMPBarrierDirective
  simpleToC CXCursor_OMPTaskwaitDirective                             = #const CXCursor_OMPTaskwaitDirective
  simpleToC CXCursor_OMPFlushDirective                                = #const CXCursor_OMPFlushDirective
  simpleToC CXCursor_SEHLeaveStmt                                     = #const CXCursor_SEHLeaveStmt
  simpleToC CXCursor_OMPOrderedDirective                              = #const CXCursor_OMPOrderedDirective
  simpleToC CXCursor_OMPAtomicDirective                               = #const CXCursor_OMPAtomicDirective
  simpleToC CXCursor_OMPForSimdDirective                              = #const CXCursor_OMPForSimdDirective
  simpleToC CXCursor_OMPParallelForSimdDirective                      = #const CXCursor_OMPParallelForSimdDirective
  simpleToC CXCursor_OMPTargetDirective                               = #const CXCursor_OMPTargetDirective
  simpleToC CXCursor_OMPTeamsDirective                                = #const CXCursor_OMPTeamsDirective
  simpleToC CXCursor_OMPTaskgroupDirective                            = #const CXCursor_OMPTaskgroupDirective
  simpleToC CXCursor_OMPCancellationPointDirective                    = #const CXCursor_OMPCancellationPointDirective
  simpleToC CXCursor_OMPCancelDirective                               = #const CXCursor_OMPCancelDirective
  simpleToC CXCursor_OMPTargetDataDirective                           = #const CXCursor_OMPTargetDataDirective
  simpleToC CXCursor_OMPTaskLoopDirective                             = #const CXCursor_OMPTaskLoopDirective
  simpleToC CXCursor_OMPTaskLoopSimdDirective                         = #const CXCursor_OMPTaskLoopSimdDirective
  simpleToC CXCursor_OMPDistributeDirective                           = #const CXCursor_OMPDistributeDirective
  simpleToC CXCursor_OMPTargetEnterDataDirective                      = #const CXCursor_OMPTargetEnterDataDirective
  simpleToC CXCursor_OMPTargetExitDataDirective                       = #const CXCursor_OMPTargetExitDataDirective
  simpleToC CXCursor_OMPTargetParallelDirective                       = #const CXCursor_OMPTargetParallelDirective
  simpleToC CXCursor_OMPTargetParallelForDirective                    = #const CXCursor_OMPTargetParallelForDirective
  simpleToC CXCursor_OMPTargetUpdateDirective                         = #const CXCursor_OMPTargetUpdateDirective
  simpleToC CXCursor_OMPDistributeParallelForDirective                = #const CXCursor_OMPDistributeParallelForDirective
  simpleToC CXCursor_OMPDistributeParallelForSimdDirective            = #const CXCursor_OMPDistributeParallelForSimdDirective
  simpleToC CXCursor_OMPDistributeSimdDirective                       = #const CXCursor_OMPDistributeSimdDirective
  simpleToC CXCursor_OMPTargetParallelForSimdDirective                = #const CXCursor_OMPTargetParallelForSimdDirective
  simpleToC CXCursor_OMPTargetSimdDirective                           = #const CXCursor_OMPTargetSimdDirective
  simpleToC CXCursor_OMPTeamsDistributeDirective                      = #const CXCursor_OMPTeamsDistributeDirective
  simpleToC CXCursor_OMPTeamsDistributeSimdDirective                  = #const CXCursor_OMPTeamsDistributeSimdDirective
  simpleToC CXCursor_OMPTeamsDistributeParallelForSimdDirective       = #const CXCursor_OMPTeamsDistributeParallelForSimdDirective
  simpleToC CXCursor_OMPTeamsDistributeParallelForDirective           = #const CXCursor_OMPTeamsDistributeParallelForDirective
  simpleToC CXCursor_OMPTargetTeamsDirective                          = #const CXCursor_OMPTargetTeamsDirective
  simpleToC CXCursor_OMPTargetTeamsDistributeDirective                = #const CXCursor_OMPTargetTeamsDistributeDirective
  simpleToC CXCursor_OMPTargetTeamsDistributeParallelForDirective     = #const CXCursor_OMPTargetTeamsDistributeParallelForDirective
  simpleToC CXCursor_OMPTargetTeamsDistributeParallelForSimdDirective = #const CXCursor_OMPTargetTeamsDistributeParallelForSimdDirective
  simpleToC CXCursor_OMPTargetTeamsDistributeSimdDirective            = #const CXCursor_OMPTargetTeamsDistributeSimdDirective
  simpleToC CXCursor_BuiltinBitCastExpr                               = #const CXCursor_BuiltinBitCastExpr
  simpleToC CXCursor_OMPMasterTaskLoopDirective                       = #const CXCursor_OMPMasterTaskLoopDirective
  simpleToC CXCursor_OMPParallelMasterTaskLoopDirective               = #const CXCursor_OMPParallelMasterTaskLoopDirective
  simpleToC CXCursor_OMPMasterTaskLoopSimdDirective                   = #const CXCursor_OMPMasterTaskLoopSimdDirective
  simpleToC CXCursor_OMPParallelMasterTaskLoopSimdDirective           = #const CXCursor_OMPParallelMasterTaskLoopSimdDirective
  simpleToC CXCursor_OMPParallelMasterDirective                       = #const CXCursor_OMPParallelMasterDirective
  simpleToC CXCursor_OMPDepobjDirective                               = #const CXCursor_OMPDepobjDirective
  simpleToC CXCursor_OMPScanDirective                                 = #const CXCursor_OMPScanDirective
  simpleToC CXCursor_OMPTileDirective                                 = #const CXCursor_OMPTileDirective
  simpleToC CXCursor_OMPCanonicalLoop                                 = #const CXCursor_OMPCanonicalLoop
  simpleToC CXCursor_OMPInteropDirective                              = #const CXCursor_OMPInteropDirective
  simpleToC CXCursor_OMPDispatchDirective                             = #const CXCursor_OMPDispatchDirective
  simpleToC CXCursor_OMPMaskedDirective                               = #const CXCursor_OMPMaskedDirective
  simpleToC CXCursor_OMPUnrollDirective                               = #const CXCursor_OMPUnrollDirective
  simpleToC CXCursor_OMPMetaDirective                                 = #const CXCursor_OMPMetaDirective
  simpleToC CXCursor_OMPGenericLoopDirective                          = #const CXCursor_OMPGenericLoopDirective
  simpleToC CXCursor_TranslationUnit                                  = #const CXCursor_TranslationUnit
  simpleToC CXCursor_UnexposedAttr                                    = #const CXCursor_UnexposedAttr
  simpleToC CXCursor_IBActionAttr                                     = #const CXCursor_IBActionAttr
  simpleToC CXCursor_IBOutletAttr                                     = #const CXCursor_IBOutletAttr
  simpleToC CXCursor_IBOutletCollectionAttr                           = #const CXCursor_IBOutletCollectionAttr
  simpleToC CXCursor_CXXFinalAttr                                     = #const CXCursor_CXXFinalAttr
  simpleToC CXCursor_CXXOverrideAttr                                  = #const CXCursor_CXXOverrideAttr
  simpleToC CXCursor_AnnotateAttr                                     = #const CXCursor_AnnotateAttr
  simpleToC CXCursor_AsmLabelAttr                                     = #const CXCursor_AsmLabelAttr
  simpleToC CXCursor_PackedAttr                                       = #const CXCursor_PackedAttr
  simpleToC CXCursor_PureAttr                                         = #const CXCursor_PureAttr
  simpleToC CXCursor_ConstAttr                                        = #const CXCursor_ConstAttr
  simpleToC CXCursor_NoDuplicateAttr                                  = #const CXCursor_NoDuplicateAttr
  simpleToC CXCursor_CUDAConstantAttr                                 = #const CXCursor_CUDAConstantAttr
  simpleToC CXCursor_CUDADeviceAttr                                   = #const CXCursor_CUDADeviceAttr
  simpleToC CXCursor_CUDAGlobalAttr                                   = #const CXCursor_CUDAGlobalAttr
  simpleToC CXCursor_CUDAHostAttr                                     = #const CXCursor_CUDAHostAttr
  simpleToC CXCursor_CUDASharedAttr                                   = #const CXCursor_CUDASharedAttr
  simpleToC CXCursor_VisibilityAttr                                   = #const CXCursor_VisibilityAttr
  simpleToC CXCursor_DLLExport                                        = #const CXCursor_DLLExport
  simpleToC CXCursor_DLLImport                                        = #const CXCursor_DLLImport
  simpleToC CXCursor_NSReturnsRetained                                = #const CXCursor_NSReturnsRetained
  simpleToC CXCursor_NSReturnsNotRetained                             = #const CXCursor_NSReturnsNotRetained
  simpleToC CXCursor_NSReturnsAutoreleased                            = #const CXCursor_NSReturnsAutoreleased
  simpleToC CXCursor_NSConsumesSelf                                   = #const CXCursor_NSConsumesSelf
  simpleToC CXCursor_NSConsumed                                       = #const CXCursor_NSConsumed
  simpleToC CXCursor_ObjCException                                    = #const CXCursor_ObjCException
  simpleToC CXCursor_ObjCNSObject                                     = #const CXCursor_ObjCNSObject
  simpleToC CXCursor_ObjCIndependentClass                             = #const CXCursor_ObjCIndependentClass
  simpleToC CXCursor_ObjCPreciseLifetime                              = #const CXCursor_ObjCPreciseLifetime
  simpleToC CXCursor_ObjCReturnsInnerPointer                          = #const CXCursor_ObjCReturnsInnerPointer
  simpleToC CXCursor_ObjCRequiresSuper                                = #const CXCursor_ObjCRequiresSuper
  simpleToC CXCursor_ObjCRootClass                                    = #const CXCursor_ObjCRootClass
  simpleToC CXCursor_ObjCSubclassingRestricted                        = #const CXCursor_ObjCSubclassingRestricted
  simpleToC CXCursor_ObjCExplicitProtocolImpl                         = #const CXCursor_ObjCExplicitProtocolImpl
  simpleToC CXCursor_ObjCDesignatedInitializer                        = #const CXCursor_ObjCDesignatedInitializer
  simpleToC CXCursor_ObjCRuntimeVisible                               = #const CXCursor_ObjCRuntimeVisible
  simpleToC CXCursor_ObjCBoxable                                      = #const CXCursor_ObjCBoxable
  simpleToC CXCursor_FlagEnum                                         = #const CXCursor_FlagEnum
  simpleToC CXCursor_ConvergentAttr                                   = #const CXCursor_ConvergentAttr
  simpleToC CXCursor_WarnUnusedAttr                                   = #const CXCursor_WarnUnusedAttr
  simpleToC CXCursor_WarnUnusedResultAttr                             = #const CXCursor_WarnUnusedResultAttr
  simpleToC CXCursor_AlignedAttr                                      = #const CXCursor_AlignedAttr
  simpleToC CXCursor_PreprocessingDirective                           = #const CXCursor_PreprocessingDirective
  simpleToC CXCursor_MacroDefinition                                  = #const CXCursor_MacroDefinition
  simpleToC CXCursor_MacroExpansion                                   = #const CXCursor_MacroExpansion
  simpleToC CXCursor_InclusionDirective                               = #const CXCursor_InclusionDirective
  simpleToC CXCursor_ModuleImportDecl                                 = #const CXCursor_ModuleImportDecl
  simpleToC CXCursor_TypeAliasTemplateDecl                            = #const CXCursor_TypeAliasTemplateDecl
  simpleToC CXCursor_StaticAssert                                     = #const CXCursor_StaticAssert
  simpleToC CXCursor_FriendDecl                                       = #const CXCursor_FriendDecl
  simpleToC CXCursor_OverloadCandidate                                = #const CXCursor_OverloadCandidate

  simpleFromC (#const CXCursor_UnexposedDecl)                                    = Just CXCursor_UnexposedDecl
  simpleFromC (#const CXCursor_StructDecl)                                       = Just CXCursor_StructDecl
  simpleFromC (#const CXCursor_UnionDecl)                                        = Just CXCursor_UnionDecl
  simpleFromC (#const CXCursor_ClassDecl)                                        = Just CXCursor_ClassDecl
  simpleFromC (#const CXCursor_EnumDecl)                                         = Just CXCursor_EnumDecl
  simpleFromC (#const CXCursor_FieldDecl)                                        = Just CXCursor_FieldDecl
  simpleFromC (#const CXCursor_EnumConstantDecl)                                 = Just CXCursor_EnumConstantDecl
  simpleFromC (#const CXCursor_FunctionDecl)                                     = Just CXCursor_FunctionDecl
  simpleFromC (#const CXCursor_VarDecl)                                          = Just CXCursor_VarDecl
  simpleFromC (#const CXCursor_ParmDecl)                                         = Just CXCursor_ParmDecl
  simpleFromC (#const CXCursor_ObjCInterfaceDecl)                                = Just CXCursor_ObjCInterfaceDecl
  simpleFromC (#const CXCursor_ObjCCategoryDecl)                                 = Just CXCursor_ObjCCategoryDecl
  simpleFromC (#const CXCursor_ObjCProtocolDecl)                                 = Just CXCursor_ObjCProtocolDecl
  simpleFromC (#const CXCursor_ObjCPropertyDecl)                                 = Just CXCursor_ObjCPropertyDecl
  simpleFromC (#const CXCursor_ObjCIvarDecl)                                     = Just CXCursor_ObjCIvarDecl
  simpleFromC (#const CXCursor_ObjCInstanceMethodDecl)                           = Just CXCursor_ObjCInstanceMethodDecl
  simpleFromC (#const CXCursor_ObjCClassMethodDecl)                              = Just CXCursor_ObjCClassMethodDecl
  simpleFromC (#const CXCursor_ObjCImplementationDecl)                           = Just CXCursor_ObjCImplementationDecl
  simpleFromC (#const CXCursor_ObjCCategoryImplDecl)                             = Just CXCursor_ObjCCategoryImplDecl
  simpleFromC (#const CXCursor_TypedefDecl)                                      = Just CXCursor_TypedefDecl
  simpleFromC (#const CXCursor_CXXMethod)                                        = Just CXCursor_CXXMethod
  simpleFromC (#const CXCursor_Namespace)                                        = Just CXCursor_Namespace
  simpleFromC (#const CXCursor_LinkageSpec)                                      = Just CXCursor_LinkageSpec
  simpleFromC (#const CXCursor_Constructor)                                      = Just CXCursor_Constructor
  simpleFromC (#const CXCursor_Destructor)                                       = Just CXCursor_Destructor
  simpleFromC (#const CXCursor_ConversionFunction)                               = Just CXCursor_ConversionFunction
  simpleFromC (#const CXCursor_TemplateTypeParameter)                            = Just CXCursor_TemplateTypeParameter
  simpleFromC (#const CXCursor_NonTypeTemplateParameter)                         = Just CXCursor_NonTypeTemplateParameter
  simpleFromC (#const CXCursor_TemplateTemplateParameter)                        = Just CXCursor_TemplateTemplateParameter
  simpleFromC (#const CXCursor_FunctionTemplate)                                 = Just CXCursor_FunctionTemplate
  simpleFromC (#const CXCursor_ClassTemplate)                                    = Just CXCursor_ClassTemplate
  simpleFromC (#const CXCursor_ClassTemplatePartialSpecialization)               = Just CXCursor_ClassTemplatePartialSpecialization
  simpleFromC (#const CXCursor_NamespaceAlias)                                   = Just CXCursor_NamespaceAlias
  simpleFromC (#const CXCursor_UsingDirective)                                   = Just CXCursor_UsingDirective
  simpleFromC (#const CXCursor_UsingDeclaration)                                 = Just CXCursor_UsingDeclaration
  simpleFromC (#const CXCursor_TypeAliasDecl)                                    = Just CXCursor_TypeAliasDecl
  simpleFromC (#const CXCursor_ObjCSynthesizeDecl)                               = Just CXCursor_ObjCSynthesizeDecl
  simpleFromC (#const CXCursor_ObjCDynamicDecl)                                  = Just CXCursor_ObjCDynamicDecl
  simpleFromC (#const CXCursor_CXXAccessSpecifier)                               = Just CXCursor_CXXAccessSpecifier
  simpleFromC (#const CXCursor_ObjCSuperClassRef)                                = Just CXCursor_ObjCSuperClassRef
  simpleFromC (#const CXCursor_ObjCProtocolRef)                                  = Just CXCursor_ObjCProtocolRef
  simpleFromC (#const CXCursor_ObjCClassRef)                                     = Just CXCursor_ObjCClassRef
  simpleFromC (#const CXCursor_TypeRef)                                          = Just CXCursor_TypeRef
  simpleFromC (#const CXCursor_CXXBaseSpecifier)                                 = Just CXCursor_CXXBaseSpecifier
  simpleFromC (#const CXCursor_TemplateRef)                                      = Just CXCursor_TemplateRef
  simpleFromC (#const CXCursor_NamespaceRef)                                     = Just CXCursor_NamespaceRef
  simpleFromC (#const CXCursor_MemberRef)                                        = Just CXCursor_MemberRef
  simpleFromC (#const CXCursor_LabelRef)                                         = Just CXCursor_LabelRef
  simpleFromC (#const CXCursor_OverloadedDeclRef)                                = Just CXCursor_OverloadedDeclRef
  simpleFromC (#const CXCursor_VariableRef)                                      = Just CXCursor_VariableRef
  simpleFromC (#const CXCursor_InvalidFile)                                      = Just CXCursor_InvalidFile
  simpleFromC (#const CXCursor_NoDeclFound)                                      = Just CXCursor_NoDeclFound
  simpleFromC (#const CXCursor_NotImplemented)                                   = Just CXCursor_NotImplemented
  simpleFromC (#const CXCursor_InvalidCode)                                      = Just CXCursor_InvalidCode
  simpleFromC (#const CXCursor_UnexposedExpr)                                    = Just CXCursor_UnexposedExpr
  simpleFromC (#const CXCursor_DeclRefExpr)                                      = Just CXCursor_DeclRefExpr
  simpleFromC (#const CXCursor_MemberRefExpr)                                    = Just CXCursor_MemberRefExpr
  simpleFromC (#const CXCursor_CallExpr)                                         = Just CXCursor_CallExpr
  simpleFromC (#const CXCursor_ObjCMessageExpr)                                  = Just CXCursor_ObjCMessageExpr
  simpleFromC (#const CXCursor_BlockExpr)                                        = Just CXCursor_BlockExpr
  simpleFromC (#const CXCursor_IntegerLiteral)                                   = Just CXCursor_IntegerLiteral
  simpleFromC (#const CXCursor_FloatingLiteral)                                  = Just CXCursor_FloatingLiteral
  simpleFromC (#const CXCursor_ImaginaryLiteral)                                 = Just CXCursor_ImaginaryLiteral
  simpleFromC (#const CXCursor_StringLiteral)                                    = Just CXCursor_StringLiteral
  simpleFromC (#const CXCursor_CharacterLiteral)                                 = Just CXCursor_CharacterLiteral
  simpleFromC (#const CXCursor_ParenExpr)                                        = Just CXCursor_ParenExpr
  simpleFromC (#const CXCursor_UnaryOperator)                                    = Just CXCursor_UnaryOperator
  simpleFromC (#const CXCursor_ArraySubscriptExpr)                               = Just CXCursor_ArraySubscriptExpr
  simpleFromC (#const CXCursor_BinaryOperator)                                   = Just CXCursor_BinaryOperator
  simpleFromC (#const CXCursor_CompoundAssignOperator)                           = Just CXCursor_CompoundAssignOperator
  simpleFromC (#const CXCursor_ConditionalOperator)                              = Just CXCursor_ConditionalOperator
  simpleFromC (#const CXCursor_CStyleCastExpr)                                   = Just CXCursor_CStyleCastExpr
  simpleFromC (#const CXCursor_CompoundLiteralExpr)                              = Just CXCursor_CompoundLiteralExpr
  simpleFromC (#const CXCursor_InitListExpr)                                     = Just CXCursor_InitListExpr
  simpleFromC (#const CXCursor_AddrLabelExpr)                                    = Just CXCursor_AddrLabelExpr
  simpleFromC (#const CXCursor_StmtExpr)                                         = Just CXCursor_StmtExpr
  simpleFromC (#const CXCursor_GenericSelectionExpr)                             = Just CXCursor_GenericSelectionExpr
  simpleFromC (#const CXCursor_GNUNullExpr)                                      = Just CXCursor_GNUNullExpr
  simpleFromC (#const CXCursor_CXXStaticCastExpr)                                = Just CXCursor_CXXStaticCastExpr
  simpleFromC (#const CXCursor_CXXDynamicCastExpr)                               = Just CXCursor_CXXDynamicCastExpr
  simpleFromC (#const CXCursor_CXXReinterpretCastExpr)                           = Just CXCursor_CXXReinterpretCastExpr
  simpleFromC (#const CXCursor_CXXConstCastExpr)                                 = Just CXCursor_CXXConstCastExpr
  simpleFromC (#const CXCursor_CXXFunctionalCastExpr)                            = Just CXCursor_CXXFunctionalCastExpr
  simpleFromC (#const CXCursor_CXXTypeidExpr)                                    = Just CXCursor_CXXTypeidExpr
  simpleFromC (#const CXCursor_CXXBoolLiteralExpr)                               = Just CXCursor_CXXBoolLiteralExpr
  simpleFromC (#const CXCursor_CXXNullPtrLiteralExpr)                            = Just CXCursor_CXXNullPtrLiteralExpr
  simpleFromC (#const CXCursor_CXXThisExpr)                                      = Just CXCursor_CXXThisExpr
  simpleFromC (#const CXCursor_CXXThrowExpr)                                     = Just CXCursor_CXXThrowExpr
  simpleFromC (#const CXCursor_CXXNewExpr)                                       = Just CXCursor_CXXNewExpr
  simpleFromC (#const CXCursor_CXXDeleteExpr)                                    = Just CXCursor_CXXDeleteExpr
  simpleFromC (#const CXCursor_UnaryExpr)                                        = Just CXCursor_UnaryExpr
  simpleFromC (#const CXCursor_ObjCStringLiteral)                                = Just CXCursor_ObjCStringLiteral
  simpleFromC (#const CXCursor_ObjCEncodeExpr)                                   = Just CXCursor_ObjCEncodeExpr
  simpleFromC (#const CXCursor_ObjCSelectorExpr)                                 = Just CXCursor_ObjCSelectorExpr
  simpleFromC (#const CXCursor_ObjCProtocolExpr)                                 = Just CXCursor_ObjCProtocolExpr
  simpleFromC (#const CXCursor_ObjCBridgedCastExpr)                              = Just CXCursor_ObjCBridgedCastExpr
  simpleFromC (#const CXCursor_PackExpansionExpr)                                = Just CXCursor_PackExpansionExpr
  simpleFromC (#const CXCursor_SizeOfPackExpr)                                   = Just CXCursor_SizeOfPackExpr
  simpleFromC (#const CXCursor_LambdaExpr)                                       = Just CXCursor_LambdaExpr
  simpleFromC (#const CXCursor_ObjCBoolLiteralExpr)                              = Just CXCursor_ObjCBoolLiteralExpr
  simpleFromC (#const CXCursor_ObjCSelfExpr)                                     = Just CXCursor_ObjCSelfExpr
  simpleFromC (#const CXCursor_ObjCAvailabilityCheckExpr)                        = Just CXCursor_ObjCAvailabilityCheckExpr
  simpleFromC (#const CXCursor_FixedPointLiteral)                                = Just CXCursor_FixedPointLiteral
  simpleFromC (#const CXCursor_OMPArrayShapingExpr)                              = Just CXCursor_OMPArrayShapingExpr
  simpleFromC (#const CXCursor_OMPIteratorExpr)                                  = Just CXCursor_OMPIteratorExpr
  simpleFromC (#const CXCursor_CXXAddrspaceCastExpr)                             = Just CXCursor_CXXAddrspaceCastExpr
  simpleFromC (#const CXCursor_UnexposedStmt)                                    = Just CXCursor_UnexposedStmt
  simpleFromC (#const CXCursor_LabelStmt)                                        = Just CXCursor_LabelStmt
  simpleFromC (#const CXCursor_CompoundStmt)                                     = Just CXCursor_CompoundStmt
  simpleFromC (#const CXCursor_CaseStmt)                                         = Just CXCursor_CaseStmt
  simpleFromC (#const CXCursor_DefaultStmt)                                      = Just CXCursor_DefaultStmt
  simpleFromC (#const CXCursor_IfStmt)                                           = Just CXCursor_IfStmt
  simpleFromC (#const CXCursor_SwitchStmt)                                       = Just CXCursor_SwitchStmt
  simpleFromC (#const CXCursor_WhileStmt)                                        = Just CXCursor_WhileStmt
  simpleFromC (#const CXCursor_DoStmt)                                           = Just CXCursor_DoStmt
  simpleFromC (#const CXCursor_ForStmt)                                          = Just CXCursor_ForStmt
  simpleFromC (#const CXCursor_GotoStmt)                                         = Just CXCursor_GotoStmt
  simpleFromC (#const CXCursor_IndirectGotoStmt)                                 = Just CXCursor_IndirectGotoStmt
  simpleFromC (#const CXCursor_ContinueStmt)                                     = Just CXCursor_ContinueStmt
  simpleFromC (#const CXCursor_BreakStmt)                                        = Just CXCursor_BreakStmt
  simpleFromC (#const CXCursor_ReturnStmt)                                       = Just CXCursor_ReturnStmt
  simpleFromC (#const CXCursor_GCCAsmStmt)                                       = Just CXCursor_GCCAsmStmt
  simpleFromC (#const CXCursor_ObjCAtTryStmt)                                    = Just CXCursor_ObjCAtTryStmt
  simpleFromC (#const CXCursor_ObjCAtCatchStmt)                                  = Just CXCursor_ObjCAtCatchStmt
  simpleFromC (#const CXCursor_ObjCAtFinallyStmt)                                = Just CXCursor_ObjCAtFinallyStmt
  simpleFromC (#const CXCursor_ObjCAtThrowStmt)                                  = Just CXCursor_ObjCAtThrowStmt
  simpleFromC (#const CXCursor_ObjCAtSynchronizedStmt)                           = Just CXCursor_ObjCAtSynchronizedStmt
  simpleFromC (#const CXCursor_ObjCAutoreleasePoolStmt)                          = Just CXCursor_ObjCAutoreleasePoolStmt
  simpleFromC (#const CXCursor_ObjCForCollectionStmt)                            = Just CXCursor_ObjCForCollectionStmt
  simpleFromC (#const CXCursor_CXXCatchStmt)                                     = Just CXCursor_CXXCatchStmt
  simpleFromC (#const CXCursor_CXXTryStmt)                                       = Just CXCursor_CXXTryStmt
  simpleFromC (#const CXCursor_CXXForRangeStmt)                                  = Just CXCursor_CXXForRangeStmt
  simpleFromC (#const CXCursor_SEHTryStmt)                                       = Just CXCursor_SEHTryStmt
  simpleFromC (#const CXCursor_SEHExceptStmt)                                    = Just CXCursor_SEHExceptStmt
  simpleFromC (#const CXCursor_SEHFinallyStmt)                                   = Just CXCursor_SEHFinallyStmt
  simpleFromC (#const CXCursor_MSAsmStmt)                                        = Just CXCursor_MSAsmStmt
  simpleFromC (#const CXCursor_NullStmt)                                         = Just CXCursor_NullStmt
  simpleFromC (#const CXCursor_DeclStmt)                                         = Just CXCursor_DeclStmt
  simpleFromC (#const CXCursor_OMPParallelDirective)                             = Just CXCursor_OMPParallelDirective
  simpleFromC (#const CXCursor_OMPSimdDirective)                                 = Just CXCursor_OMPSimdDirective
  simpleFromC (#const CXCursor_OMPForDirective)                                  = Just CXCursor_OMPForDirective
  simpleFromC (#const CXCursor_OMPSectionsDirective)                             = Just CXCursor_OMPSectionsDirective
  simpleFromC (#const CXCursor_OMPSectionDirective)                              = Just CXCursor_OMPSectionDirective
  simpleFromC (#const CXCursor_OMPSingleDirective)                               = Just CXCursor_OMPSingleDirective
  simpleFromC (#const CXCursor_OMPParallelForDirective)                          = Just CXCursor_OMPParallelForDirective
  simpleFromC (#const CXCursor_OMPParallelSectionsDirective)                     = Just CXCursor_OMPParallelSectionsDirective
  simpleFromC (#const CXCursor_OMPTaskDirective)                                 = Just CXCursor_OMPTaskDirective
  simpleFromC (#const CXCursor_OMPMasterDirective)                               = Just CXCursor_OMPMasterDirective
  simpleFromC (#const CXCursor_OMPCriticalDirective)                             = Just CXCursor_OMPCriticalDirective
  simpleFromC (#const CXCursor_OMPTaskyieldDirective)                            = Just CXCursor_OMPTaskyieldDirective
  simpleFromC (#const CXCursor_OMPBarrierDirective)                              = Just CXCursor_OMPBarrierDirective
  simpleFromC (#const CXCursor_OMPTaskwaitDirective)                             = Just CXCursor_OMPTaskwaitDirective
  simpleFromC (#const CXCursor_OMPFlushDirective)                                = Just CXCursor_OMPFlushDirective
  simpleFromC (#const CXCursor_SEHLeaveStmt)                                     = Just CXCursor_SEHLeaveStmt
  simpleFromC (#const CXCursor_OMPOrderedDirective)                              = Just CXCursor_OMPOrderedDirective
  simpleFromC (#const CXCursor_OMPAtomicDirective)                               = Just CXCursor_OMPAtomicDirective
  simpleFromC (#const CXCursor_OMPForSimdDirective)                              = Just CXCursor_OMPForSimdDirective
  simpleFromC (#const CXCursor_OMPParallelForSimdDirective)                      = Just CXCursor_OMPParallelForSimdDirective
  simpleFromC (#const CXCursor_OMPTargetDirective)                               = Just CXCursor_OMPTargetDirective
  simpleFromC (#const CXCursor_OMPTeamsDirective)                                = Just CXCursor_OMPTeamsDirective
  simpleFromC (#const CXCursor_OMPTaskgroupDirective)                            = Just CXCursor_OMPTaskgroupDirective
  simpleFromC (#const CXCursor_OMPCancellationPointDirective)                    = Just CXCursor_OMPCancellationPointDirective
  simpleFromC (#const CXCursor_OMPCancelDirective)                               = Just CXCursor_OMPCancelDirective
  simpleFromC (#const CXCursor_OMPTargetDataDirective)                           = Just CXCursor_OMPTargetDataDirective
  simpleFromC (#const CXCursor_OMPTaskLoopDirective)                             = Just CXCursor_OMPTaskLoopDirective
  simpleFromC (#const CXCursor_OMPTaskLoopSimdDirective)                         = Just CXCursor_OMPTaskLoopSimdDirective
  simpleFromC (#const CXCursor_OMPDistributeDirective)                           = Just CXCursor_OMPDistributeDirective
  simpleFromC (#const CXCursor_OMPTargetEnterDataDirective)                      = Just CXCursor_OMPTargetEnterDataDirective
  simpleFromC (#const CXCursor_OMPTargetExitDataDirective)                       = Just CXCursor_OMPTargetExitDataDirective
  simpleFromC (#const CXCursor_OMPTargetParallelDirective)                       = Just CXCursor_OMPTargetParallelDirective
  simpleFromC (#const CXCursor_OMPTargetParallelForDirective)                    = Just CXCursor_OMPTargetParallelForDirective
  simpleFromC (#const CXCursor_OMPTargetUpdateDirective)                         = Just CXCursor_OMPTargetUpdateDirective
  simpleFromC (#const CXCursor_OMPDistributeParallelForDirective)                = Just CXCursor_OMPDistributeParallelForDirective
  simpleFromC (#const CXCursor_OMPDistributeParallelForSimdDirective)            = Just CXCursor_OMPDistributeParallelForSimdDirective
  simpleFromC (#const CXCursor_OMPDistributeSimdDirective)                       = Just CXCursor_OMPDistributeSimdDirective
  simpleFromC (#const CXCursor_OMPTargetParallelForSimdDirective)                = Just CXCursor_OMPTargetParallelForSimdDirective
  simpleFromC (#const CXCursor_OMPTargetSimdDirective)                           = Just CXCursor_OMPTargetSimdDirective
  simpleFromC (#const CXCursor_OMPTeamsDistributeDirective)                      = Just CXCursor_OMPTeamsDistributeDirective
  simpleFromC (#const CXCursor_OMPTeamsDistributeSimdDirective)                  = Just CXCursor_OMPTeamsDistributeSimdDirective
  simpleFromC (#const CXCursor_OMPTeamsDistributeParallelForSimdDirective)       = Just CXCursor_OMPTeamsDistributeParallelForSimdDirective
  simpleFromC (#const CXCursor_OMPTeamsDistributeParallelForDirective)           = Just CXCursor_OMPTeamsDistributeParallelForDirective
  simpleFromC (#const CXCursor_OMPTargetTeamsDirective)                          = Just CXCursor_OMPTargetTeamsDirective
  simpleFromC (#const CXCursor_OMPTargetTeamsDistributeDirective)                = Just CXCursor_OMPTargetTeamsDistributeDirective
  simpleFromC (#const CXCursor_OMPTargetTeamsDistributeParallelForDirective)     = Just CXCursor_OMPTargetTeamsDistributeParallelForDirective
  simpleFromC (#const CXCursor_OMPTargetTeamsDistributeParallelForSimdDirective) = Just CXCursor_OMPTargetTeamsDistributeParallelForSimdDirective
  simpleFromC (#const CXCursor_OMPTargetTeamsDistributeSimdDirective)            = Just CXCursor_OMPTargetTeamsDistributeSimdDirective
  simpleFromC (#const CXCursor_BuiltinBitCastExpr)                               = Just CXCursor_BuiltinBitCastExpr
  simpleFromC (#const CXCursor_OMPMasterTaskLoopDirective)                       = Just CXCursor_OMPMasterTaskLoopDirective
  simpleFromC (#const CXCursor_OMPParallelMasterTaskLoopDirective)               = Just CXCursor_OMPParallelMasterTaskLoopDirective
  simpleFromC (#const CXCursor_OMPMasterTaskLoopSimdDirective)                   = Just CXCursor_OMPMasterTaskLoopSimdDirective
  simpleFromC (#const CXCursor_OMPParallelMasterTaskLoopSimdDirective)           = Just CXCursor_OMPParallelMasterTaskLoopSimdDirective
  simpleFromC (#const CXCursor_OMPParallelMasterDirective)                       = Just CXCursor_OMPParallelMasterDirective
  simpleFromC (#const CXCursor_OMPDepobjDirective)                               = Just CXCursor_OMPDepobjDirective
  simpleFromC (#const CXCursor_OMPScanDirective)                                 = Just CXCursor_OMPScanDirective
  simpleFromC (#const CXCursor_OMPTileDirective)                                 = Just CXCursor_OMPTileDirective
  simpleFromC (#const CXCursor_OMPCanonicalLoop)                                 = Just CXCursor_OMPCanonicalLoop
  simpleFromC (#const CXCursor_OMPInteropDirective)                              = Just CXCursor_OMPInteropDirective
  simpleFromC (#const CXCursor_OMPDispatchDirective)                             = Just CXCursor_OMPDispatchDirective
  simpleFromC (#const CXCursor_OMPMaskedDirective)                               = Just CXCursor_OMPMaskedDirective
  simpleFromC (#const CXCursor_OMPUnrollDirective)                               = Just CXCursor_OMPUnrollDirective
  simpleFromC (#const CXCursor_OMPMetaDirective)                                 = Just CXCursor_OMPMetaDirective
  simpleFromC (#const CXCursor_OMPGenericLoopDirective)                          = Just CXCursor_OMPGenericLoopDirective
  simpleFromC (#const CXCursor_TranslationUnit)                                  = Just CXCursor_TranslationUnit
  simpleFromC (#const CXCursor_UnexposedAttr)                                    = Just CXCursor_UnexposedAttr
  simpleFromC (#const CXCursor_IBActionAttr)                                     = Just CXCursor_IBActionAttr
  simpleFromC (#const CXCursor_IBOutletAttr)                                     = Just CXCursor_IBOutletAttr
  simpleFromC (#const CXCursor_IBOutletCollectionAttr)                           = Just CXCursor_IBOutletCollectionAttr
  simpleFromC (#const CXCursor_CXXFinalAttr)                                     = Just CXCursor_CXXFinalAttr
  simpleFromC (#const CXCursor_CXXOverrideAttr)                                  = Just CXCursor_CXXOverrideAttr
  simpleFromC (#const CXCursor_AnnotateAttr)                                     = Just CXCursor_AnnotateAttr
  simpleFromC (#const CXCursor_AsmLabelAttr)                                     = Just CXCursor_AsmLabelAttr
  simpleFromC (#const CXCursor_PackedAttr)                                       = Just CXCursor_PackedAttr
  simpleFromC (#const CXCursor_PureAttr)                                         = Just CXCursor_PureAttr
  simpleFromC (#const CXCursor_ConstAttr)                                        = Just CXCursor_ConstAttr
  simpleFromC (#const CXCursor_NoDuplicateAttr)                                  = Just CXCursor_NoDuplicateAttr
  simpleFromC (#const CXCursor_CUDAConstantAttr)                                 = Just CXCursor_CUDAConstantAttr
  simpleFromC (#const CXCursor_CUDADeviceAttr)                                   = Just CXCursor_CUDADeviceAttr
  simpleFromC (#const CXCursor_CUDAGlobalAttr)                                   = Just CXCursor_CUDAGlobalAttr
  simpleFromC (#const CXCursor_CUDAHostAttr)                                     = Just CXCursor_CUDAHostAttr
  simpleFromC (#const CXCursor_CUDASharedAttr)                                   = Just CXCursor_CUDASharedAttr
  simpleFromC (#const CXCursor_VisibilityAttr)                                   = Just CXCursor_VisibilityAttr
  simpleFromC (#const CXCursor_DLLExport)                                        = Just CXCursor_DLLExport
  simpleFromC (#const CXCursor_DLLImport)                                        = Just CXCursor_DLLImport
  simpleFromC (#const CXCursor_NSReturnsRetained)                                = Just CXCursor_NSReturnsRetained
  simpleFromC (#const CXCursor_NSReturnsNotRetained)                             = Just CXCursor_NSReturnsNotRetained
  simpleFromC (#const CXCursor_NSReturnsAutoreleased)                            = Just CXCursor_NSReturnsAutoreleased
  simpleFromC (#const CXCursor_NSConsumesSelf)                                   = Just CXCursor_NSConsumesSelf
  simpleFromC (#const CXCursor_NSConsumed)                                       = Just CXCursor_NSConsumed
  simpleFromC (#const CXCursor_ObjCException)                                    = Just CXCursor_ObjCException
  simpleFromC (#const CXCursor_ObjCNSObject)                                     = Just CXCursor_ObjCNSObject
  simpleFromC (#const CXCursor_ObjCIndependentClass)                             = Just CXCursor_ObjCIndependentClass
  simpleFromC (#const CXCursor_ObjCPreciseLifetime)                              = Just CXCursor_ObjCPreciseLifetime
  simpleFromC (#const CXCursor_ObjCReturnsInnerPointer)                          = Just CXCursor_ObjCReturnsInnerPointer
  simpleFromC (#const CXCursor_ObjCRequiresSuper)                                = Just CXCursor_ObjCRequiresSuper
  simpleFromC (#const CXCursor_ObjCRootClass)                                    = Just CXCursor_ObjCRootClass
  simpleFromC (#const CXCursor_ObjCSubclassingRestricted)                        = Just CXCursor_ObjCSubclassingRestricted
  simpleFromC (#const CXCursor_ObjCExplicitProtocolImpl)                         = Just CXCursor_ObjCExplicitProtocolImpl
  simpleFromC (#const CXCursor_ObjCDesignatedInitializer)                        = Just CXCursor_ObjCDesignatedInitializer
  simpleFromC (#const CXCursor_ObjCRuntimeVisible)                               = Just CXCursor_ObjCRuntimeVisible
  simpleFromC (#const CXCursor_ObjCBoxable)                                      = Just CXCursor_ObjCBoxable
  simpleFromC (#const CXCursor_FlagEnum)                                         = Just CXCursor_FlagEnum
  simpleFromC (#const CXCursor_ConvergentAttr)                                   = Just CXCursor_ConvergentAttr
  simpleFromC (#const CXCursor_WarnUnusedAttr)                                   = Just CXCursor_WarnUnusedAttr
  simpleFromC (#const CXCursor_WarnUnusedResultAttr)                             = Just CXCursor_WarnUnusedResultAttr
  simpleFromC (#const CXCursor_AlignedAttr)                                      = Just CXCursor_AlignedAttr
  simpleFromC (#const CXCursor_PreprocessingDirective)                           = Just CXCursor_PreprocessingDirective
  simpleFromC (#const CXCursor_MacroDefinition)                                  = Just CXCursor_MacroDefinition
  simpleFromC (#const CXCursor_MacroExpansion)                                   = Just CXCursor_MacroExpansion
  simpleFromC (#const CXCursor_InclusionDirective)                               = Just CXCursor_InclusionDirective
  simpleFromC (#const CXCursor_ModuleImportDecl)                                 = Just CXCursor_ModuleImportDecl
  simpleFromC (#const CXCursor_TypeAliasTemplateDecl)                            = Just CXCursor_TypeAliasTemplateDecl
  simpleFromC (#const CXCursor_StaticAssert)                                     = Just CXCursor_StaticAssert
  simpleFromC (#const CXCursor_FriendDecl)                                       = Just CXCursor_FriendDecl
  simpleFromC (#const CXCursor_OverloadCandidate)                                = Just CXCursor_OverloadCandidate

  simpleFromC _otherwise = Nothing

{-------------------------------------------------------------------------------
  CXDiagnosticDisplayOptions
-------------------------------------------------------------------------------}

instance IsSingleFlag CXDiagnosticDisplayOptions where
  flagToC CXDiagnostic_DisplaySourceLocation = #const CXDiagnostic_DisplaySourceLocation
  flagToC CXDiagnostic_DisplayColumn         = #const CXDiagnostic_DisplayColumn
  flagToC CXDiagnostic_DisplaySourceRanges   = #const CXDiagnostic_DisplaySourceRanges
  flagToC CXDiagnostic_DisplayOption         = #const CXDiagnostic_DisplayOption
  flagToC CXDiagnostic_DisplayCategoryId     = #const CXDiagnostic_DisplayCategoryId
  flagToC CXDiagnostic_DisplayCategoryName   = #const CXDiagnostic_DisplayCategoryName

{-------------------------------------------------------------------------------
  CXDiagnosticSeverity
-------------------------------------------------------------------------------}

instance IsSimpleEnum CXDiagnosticSeverity where
  simpleToC CXDiagnostic_Ignored  = #const CXDiagnostic_Ignored
  simpleToC CXDiagnostic_Note     = #const CXDiagnostic_Note
  simpleToC CXDiagnostic_Warning  = #const CXDiagnostic_Warning
  simpleToC CXDiagnostic_Error    = #const CXDiagnostic_Error
  simpleToC CXDiagnostic_Fatal    = #const CXDiagnostic_Fatal

  simpleFromC (#const CXDiagnostic_Ignored) = Just CXDiagnostic_Ignored
  simpleFromC (#const CXDiagnostic_Note)    = Just CXDiagnostic_Note
  simpleFromC (#const CXDiagnostic_Warning) = Just CXDiagnostic_Warning
  simpleFromC (#const CXDiagnostic_Error)   = Just CXDiagnostic_Error
  simpleFromC (#const CXDiagnostic_Fatal)   = Just CXDiagnostic_Fatal

  simpleFromC _otherwise = Nothing

{-------------------------------------------------------------------------------
  CX_StorageClass
-------------------------------------------------------------------------------}

instance IsSimpleEnum CX_StorageClass where
  simpleToC CX_SC_None                 = #const CX_SC_None
  simpleToC CX_SC_Extern               = #const CX_SC_Extern
  simpleToC CX_SC_Static               = #const CX_SC_Static
  simpleToC CX_SC_PrivateExtern        = #const CX_SC_PrivateExtern
  simpleToC CX_SC_OpenCLWorkGroupLocal = #const CX_SC_OpenCLWorkGroupLocal
  simpleToC CX_SC_Auto                 = #const CX_SC_Auto
  simpleToC CX_SC_Register             = #const CX_SC_Register

  simpleFromC (#const CX_SC_None)                 = Just CX_SC_None
  simpleFromC (#const CX_SC_Extern)               = Just CX_SC_Extern
  simpleFromC (#const CX_SC_Static)               = Just CX_SC_Static
  simpleFromC (#const CX_SC_PrivateExtern)        = Just CX_SC_PrivateExtern
  simpleFromC (#const CX_SC_OpenCLWorkGroupLocal) = Just CX_SC_OpenCLWorkGroupLocal
  simpleFromC (#const CX_SC_Auto)                 = Just CX_SC_Auto
  simpleFromC (#const CX_SC_Register)             = Just CX_SC_Register

  simpleFromC _otherwise = Nothing

{-------------------------------------------------------------------------------
  CXTLSKind
-------------------------------------------------------------------------------}

instance IsSimpleEnum CXTLSKind where
  simpleToC CXTLS_None    = #const CXTLS_None
  simpleToC CXTLS_Dynamic = #const CXTLS_Dynamic
  simpleToC CXTLS_Static  = #const CXTLS_Static

  simpleFromC (#const CXTLS_None)    = Just CXTLS_None
  simpleFromC (#const CXTLS_Dynamic) = Just CXTLS_Dynamic
  simpleFromC (#const CXTLS_Static)  = Just CXTLS_Static

  simpleFromC _otherwise = Nothing
