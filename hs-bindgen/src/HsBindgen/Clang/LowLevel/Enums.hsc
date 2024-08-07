-- | Enums (requires the help of the @hsc2hs@ preprocessor)
--
-- We need only minimal help; since we get no HLS support in modules that are
-- preprocessed, we use a separate module for bindings that require @hsc2hs@.
--
-- This module should only be imported from "HsBindgen.Clang.Lowlevel" (which
-- re-exports the relevant definitions).
module HsBindgen.Clang.LowLevel.Enums (
    CXTranslationUnit_Flag(..)
  , CXTypeKind(..)
  , CXChildVisitResult(..)
  ) where

import HsBindgen.Patterns

#include <clang-c/Index.h>

{-------------------------------------------------------------------------------
  CXTranslationUnit_Flag
-------------------------------------------------------------------------------}

-- | Single flag of 'CXTranslationUnit_Flags'
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TRANSLATION__UNIT.html#gab1e4965c1ebe8e41d71e90203a723fe9>
data CXTranslationUnit_Flag =
    -- | Used to indicate that no special translation-unit options are needed.
    CXTranslationUnit_None

    -- | Used to indicate that the parser should construct a "detailed"
    -- preprocessing record, including all macro definitions and instantiations.
    --
    -- Constructing a detailed preprocessing record requires more memory and
    -- time to parse, since the information contained in the record is usually
    -- not retained. However, it can be useful for applications that require
    -- more detailed information about the behavior of the preprocessor.
  | CXTranslationUnit_DetailedPreprocessingRecord

    -- | Used to indicate that the translation unit is incomplete.
    --
    -- When a translation unit is considered "incomplete", semantic analysis
    -- that is typically performed at the end of the translation unit will be
    -- suppressed. For example, this suppresses the completion of tentative
    -- declarations in C and of instantiation of implicitly-instantiation
    -- function templates in C++. This option is typically used when parsing a
    -- header with the intent of producing a precompiled header.
  | CXTranslationUnit_Incomplete

    -- | Used to indicate that the translation unit should be built with an
    -- implicit precompiled header for the preamble.
    --
    -- An implicit precompiled header is used as an optimization when a
    -- particular translation unit is likely to be reparsed many times when the
    -- sources aren't changing that often. In this case, an implicit precompiled
    -- header will be built containing all of the initial includes at the top of
    -- the main file (what we refer to as the "preamble" of the file). In
    -- subsequent parses, if the preamble or the files in it have not changed,
    -- \c clang_reparseTranslationUnit() will re-use the implicit precompiled
    -- header to improve parsing performance.
  | CXTranslationUnit_PrecompiledPreamble

    -- | Used to indicate that the translation unit should cache some
    -- code-completion results with each reparse of the source file.
    --
    -- Caching of code-completion results is a performance optimization that
    -- introduces some overhead to reparsing but improves the performance of
    -- code-completion operations.
  | CXTranslationUnit_CacheCompletionResults

    -- | Used to indicate that the translation unit will be serialized with
    -- 'clang_saveTranslationUnit'.
    --
    -- This option is typically used when parsing a header with the intent of
    -- producing a precompiled header.
  | CXTranslationUnit_ForSerialization

    -- | Enabled chained precompiled preambles in C++.
  | CXTranslationUnit_CXXChainedPCH

    -- | Used to indicate that function/method bodies should be skipped while
    -- parsing.
    --
    -- This option can be used to search for declarations/definitions while
    -- ignoring the usages.
  | CXTranslationUnit_SkipFunctionBodies

    -- | Used to indicate that brief documentation comments should be included
    -- into the set of code completions returned from this translation unit.
  | CXTranslationUnit_IncludeBriefCommentsInCodeCompletion

    -- | Used to indicate that the precompiled preamble should be created on the
    -- first parse. Otherwise it will be created on the first reparse. This
    -- trades runtime on the first parse (serializing the preamble takes time)
    -- for reduced runtime on the second parse (can now reuse the preamble).
  | CXTranslationUnit_CreatePreambleOnFirstParse

    -- | Do not stop processing when fatal errors are encountered.
    --
    -- When fatal errors are encountered while parsing a translation unit,
    -- semantic analysis is typically stopped early when compiling code. A
    -- common source for fatal errors are unresolvable include files. For the
    -- purposes of an IDE, this is undesirable behavior and as much information
    -- as possible should be reported. Use this flag to enable this behavior.
  | CXTranslationUnit_KeepGoing

    -- | Sets the preprocessor in a mode for parsing a single file only.
  | CXTranslationUnit_SingleFileParse

    -- | Used in combination with CXTranslationUnit_SkipFunctionBodies to
    -- constrain the skipping of function bodies to the preamble.
    --
    -- The function bodies of the main file are not skipped.
  | CXTranslationUnit_LimitSkipFunctionBodiesToPreamble

    -- | Used to indicate that attributed types should be included in CXType.
  | CXTranslationUnit_IncludeAttributedTypes

    -- | Used to indicate that implicit attributes should be visited.
  | CXTranslationUnit_VisitImplicitAttributes

    -- | Used to indicate that non-errors from included files should be ignored.
    --
    -- If set, 'clang_getDiagnosticSetFromTU' will not report e.g. warnings from
    -- included files anymore. This speeds up 'clang_getDiagnosticSetFromTU' for
    -- the case where these warnings are not of interest, as for an IDE for
    -- example, which typically shows only the diagnostics in the main file.
  | CXTranslationUnit_IgnoreNonErrorsFromIncludedFiles

    -- | Tells the preprocessor not to skip excluded conditional blocks.
  | CXTranslationUnit_RetainExcludedConditionalBlocks
  deriving stock (Show, Eq, Ord, Enum, Bounded)

{-# DEPRECATED CXTranslationUnit_CXXChainedPCH
      "Note: this is a *temporary* option that is available only while we are testing C++ precompiled preamble support."
#-}

instance IsSingleFlag CXTranslationUnit_Flag where
  flagToC CXTranslationUnit_None                                 = #const CXTranslationUnit_None
  flagToC CXTranslationUnit_DetailedPreprocessingRecord          = #const CXTranslationUnit_DetailedPreprocessingRecord
  flagToC CXTranslationUnit_Incomplete                           = #const CXTranslationUnit_Incomplete
  flagToC CXTranslationUnit_PrecompiledPreamble                  = #const CXTranslationUnit_PrecompiledPreamble
  flagToC CXTranslationUnit_CacheCompletionResults               = #const CXTranslationUnit_CacheCompletionResults
  flagToC CXTranslationUnit_ForSerialization                     = #const CXTranslationUnit_ForSerialization
  flagToC CXTranslationUnit_CXXChainedPCH                        = #const CXTranslationUnit_CXXChainedPCH
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

-- | Describes the kind of type
--
-- NOTE: This definition is not complete; we omit all OpenCL related kinds.
-- We don't need them, and by omitting them we are compatible with a larger
-- range of @libclang@ versions.
--
-- NOTE: We omit @CXType_FirstBuiltin@ and @CXType_LastBuiltin@, which are
-- aliases for the first and last builtin type in the list, respectively. If
-- we need them, we should define them as separate constants.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gaad39de597b13a18882c21860f92b095a>
data CXTypeKind =
    -- | Represents an invalid type (e.g., where no type is available).
    CXType_Invalid

    -- | A type whose specific kind is not exposed via this
  | CXType_Unexposed

    --
    -- Builtin types
    --

  | CXType_Void
  | CXType_Bool
  | CXType_Char_U
  | CXType_UChar
  | CXType_Char16
  | CXType_Char32
  | CXType_UShort
  | CXType_UInt
  | CXType_ULong
  | CXType_ULongLong
  | CXType_UInt128
  | CXType_Char_S
  | CXType_SChar
  | CXType_WChar
  | CXType_Short
  | CXType_Int
  | CXType_Long
  | CXType_LongLong
  | CXType_Int128
  | CXType_Float
  | CXType_Double
  | CXType_LongDouble
  | CXType_NullPtr
  | CXType_Overload
  | CXType_Dependent
  | CXType_ObjCId
  | CXType_ObjCClass
  | CXType_ObjCSel
  | CXType_Float128
  | CXType_Half
  | CXType_Float16
  | CXType_ShortAccum
  | CXType_Accum
  | CXType_LongAccum
  | CXType_UShortAccum
  | CXType_UAccum
  | CXType_ULongAccum
  | CXType_BFloat16
  | CXType_Ibm128

  | CXType_Complex
  | CXType_Pointer
  | CXType_BlockPointer
  | CXType_LValueReference
  | CXType_RValueReference
  | CXType_Record
  | CXType_Enum
  | CXType_Typedef
  | CXType_ObjCInterface
  | CXType_ObjCObjectPointer
  | CXType_FunctionNoProto
  | CXType_FunctionProto
  | CXType_ConstantArray
  | CXType_Vector
  | CXType_IncompleteArray
  | CXType_VariableArray
  | CXType_DependentSizedArray
  | CXType_MemberPointer
  | CXType_Auto

    -- | Represents a type that was referred to using an elaborated type keyword.
    --
    -- E.g., struct S, or via a qualified name, e.g., N::M::type, or both.
  | CXType_Elaborated
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance IsSimpleEnum CXTypeKind where
  simpleToC CXType_Invalid                                              = #const CXType_Invalid
  simpleToC CXType_Unexposed                                            = #const CXType_Unexposed
  simpleToC CXType_Void                                                 = #const CXType_Void
  simpleToC CXType_Bool                                                 = #const CXType_Bool
  simpleToC CXType_Char_U                                               = #const CXType_Char_U
  simpleToC CXType_UChar                                                = #const CXType_UChar
  simpleToC CXType_Char16                                               = #const CXType_Char16
  simpleToC CXType_Char32                                               = #const CXType_Char32
  simpleToC CXType_UShort                                               = #const CXType_UShort
  simpleToC CXType_UInt                                                 = #const CXType_UInt
  simpleToC CXType_ULong                                                = #const CXType_ULong
  simpleToC CXType_ULongLong                                            = #const CXType_ULongLong
  simpleToC CXType_UInt128                                              = #const CXType_UInt128
  simpleToC CXType_Char_S                                               = #const CXType_Char_S
  simpleToC CXType_SChar                                                = #const CXType_SChar
  simpleToC CXType_WChar                                                = #const CXType_WChar
  simpleToC CXType_Short                                                = #const CXType_Short
  simpleToC CXType_Int                                                  = #const CXType_Int
  simpleToC CXType_Long                                                 = #const CXType_Long
  simpleToC CXType_LongLong                                             = #const CXType_LongLong
  simpleToC CXType_Int128                                               = #const CXType_Int128
  simpleToC CXType_Float                                                = #const CXType_Float
  simpleToC CXType_Double                                               = #const CXType_Double
  simpleToC CXType_LongDouble                                           = #const CXType_LongDouble
  simpleToC CXType_NullPtr                                              = #const CXType_NullPtr
  simpleToC CXType_Overload                                             = #const CXType_Overload
  simpleToC CXType_Dependent                                            = #const CXType_Dependent
  simpleToC CXType_ObjCId                                               = #const CXType_ObjCId
  simpleToC CXType_ObjCClass                                            = #const CXType_ObjCClass
  simpleToC CXType_ObjCSel                                              = #const CXType_ObjCSel
  simpleToC CXType_Float128                                             = #const CXType_Float128
  simpleToC CXType_Half                                                 = #const CXType_Half
  simpleToC CXType_Float16                                              = #const CXType_Float16
  simpleToC CXType_ShortAccum                                           = #const CXType_ShortAccum
  simpleToC CXType_Accum                                                = #const CXType_Accum
  simpleToC CXType_LongAccum                                            = #const CXType_LongAccum
  simpleToC CXType_UShortAccum                                          = #const CXType_UShortAccum
  simpleToC CXType_UAccum                                               = #const CXType_UAccum
  simpleToC CXType_ULongAccum                                           = #const CXType_ULongAccum
  simpleToC CXType_BFloat16                                             = #const CXType_BFloat16
  simpleToC CXType_Ibm128                                               = #const CXType_Ibm128
  simpleToC CXType_Complex                                              = #const CXType_Complex
  simpleToC CXType_Pointer                                              = #const CXType_Pointer
  simpleToC CXType_BlockPointer                                         = #const CXType_BlockPointer
  simpleToC CXType_LValueReference                                      = #const CXType_LValueReference
  simpleToC CXType_RValueReference                                      = #const CXType_RValueReference
  simpleToC CXType_Record                                               = #const CXType_Record
  simpleToC CXType_Enum                                                 = #const CXType_Enum
  simpleToC CXType_Typedef                                              = #const CXType_Typedef
  simpleToC CXType_ObjCInterface                                        = #const CXType_ObjCInterface
  simpleToC CXType_ObjCObjectPointer                                    = #const CXType_ObjCObjectPointer
  simpleToC CXType_FunctionNoProto                                      = #const CXType_FunctionNoProto
  simpleToC CXType_FunctionProto                                        = #const CXType_FunctionProto
  simpleToC CXType_ConstantArray                                        = #const CXType_ConstantArray
  simpleToC CXType_Vector                                               = #const CXType_Vector
  simpleToC CXType_IncompleteArray                                      = #const CXType_IncompleteArray
  simpleToC CXType_VariableArray                                        = #const CXType_VariableArray
  simpleToC CXType_DependentSizedArray                                  = #const CXType_DependentSizedArray
  simpleToC CXType_MemberPointer                                        = #const CXType_MemberPointer
  simpleToC CXType_Auto                                                 = #const CXType_Auto
  simpleToC CXType_Elaborated                                           = #const CXType_Elaborated

  simpleFromC (#const CXType_Invalid)                                              = Just CXType_Invalid
  simpleFromC (#const CXType_Unexposed)                                            = Just CXType_Unexposed
  simpleFromC (#const CXType_Void)                                                 = Just CXType_Void
  simpleFromC (#const CXType_Bool)                                                 = Just CXType_Bool
  simpleFromC (#const CXType_Char_U)                                               = Just CXType_Char_U
  simpleFromC (#const CXType_UChar)                                                = Just CXType_UChar
  simpleFromC (#const CXType_Char16)                                               = Just CXType_Char16
  simpleFromC (#const CXType_Char32)                                               = Just CXType_Char32
  simpleFromC (#const CXType_UShort)                                               = Just CXType_UShort
  simpleFromC (#const CXType_UInt)                                                 = Just CXType_UInt
  simpleFromC (#const CXType_ULong)                                                = Just CXType_ULong
  simpleFromC (#const CXType_ULongLong)                                            = Just CXType_ULongLong
  simpleFromC (#const CXType_UInt128)                                              = Just CXType_UInt128
  simpleFromC (#const CXType_Char_S)                                               = Just CXType_Char_S
  simpleFromC (#const CXType_SChar)                                                = Just CXType_SChar
  simpleFromC (#const CXType_WChar)                                                = Just CXType_WChar
  simpleFromC (#const CXType_Short)                                                = Just CXType_Short
  simpleFromC (#const CXType_Int)                                                  = Just CXType_Int
  simpleFromC (#const CXType_Long)                                                 = Just CXType_Long
  simpleFromC (#const CXType_LongLong)                                             = Just CXType_LongLong
  simpleFromC (#const CXType_Int128)                                               = Just CXType_Int128
  simpleFromC (#const CXType_Float)                                                = Just CXType_Float
  simpleFromC (#const CXType_Double)                                               = Just CXType_Double
  simpleFromC (#const CXType_LongDouble)                                           = Just CXType_LongDouble
  simpleFromC (#const CXType_NullPtr)                                              = Just CXType_NullPtr
  simpleFromC (#const CXType_Overload)                                             = Just CXType_Overload
  simpleFromC (#const CXType_Dependent)                                            = Just CXType_Dependent
  simpleFromC (#const CXType_ObjCId)                                               = Just CXType_ObjCId
  simpleFromC (#const CXType_ObjCClass)                                            = Just CXType_ObjCClass
  simpleFromC (#const CXType_ObjCSel)                                              = Just CXType_ObjCSel
  simpleFromC (#const CXType_Float128)                                             = Just CXType_Float128
  simpleFromC (#const CXType_Half)                                                 = Just CXType_Half
  simpleFromC (#const CXType_Float16)                                              = Just CXType_Float16
  simpleFromC (#const CXType_ShortAccum)                                           = Just CXType_ShortAccum
  simpleFromC (#const CXType_Accum)                                                = Just CXType_Accum
  simpleFromC (#const CXType_LongAccum)                                            = Just CXType_LongAccum
  simpleFromC (#const CXType_UShortAccum)                                          = Just CXType_UShortAccum
  simpleFromC (#const CXType_UAccum)                                               = Just CXType_UAccum
  simpleFromC (#const CXType_ULongAccum)                                           = Just CXType_ULongAccum
  simpleFromC (#const CXType_BFloat16)                                             = Just CXType_BFloat16
  simpleFromC (#const CXType_Ibm128)                                               = Just CXType_Ibm128
  simpleFromC (#const CXType_Complex)                                              = Just CXType_Complex
  simpleFromC (#const CXType_Pointer)                                              = Just CXType_Pointer
  simpleFromC (#const CXType_BlockPointer)                                         = Just CXType_BlockPointer
  simpleFromC (#const CXType_LValueReference)                                      = Just CXType_LValueReference
  simpleFromC (#const CXType_RValueReference)                                      = Just CXType_RValueReference
  simpleFromC (#const CXType_Record)                                               = Just CXType_Record
  simpleFromC (#const CXType_Enum)                                                 = Just CXType_Enum
  simpleFromC (#const CXType_Typedef)                                              = Just CXType_Typedef
  simpleFromC (#const CXType_ObjCInterface)                                        = Just CXType_ObjCInterface
  simpleFromC (#const CXType_ObjCObjectPointer)                                    = Just CXType_ObjCObjectPointer
  simpleFromC (#const CXType_FunctionNoProto)                                      = Just CXType_FunctionNoProto
  simpleFromC (#const CXType_FunctionProto)                                        = Just CXType_FunctionProto
  simpleFromC (#const CXType_ConstantArray)                                        = Just CXType_ConstantArray
  simpleFromC (#const CXType_Vector)                                               = Just CXType_Vector
  simpleFromC (#const CXType_IncompleteArray)                                      = Just CXType_IncompleteArray
  simpleFromC (#const CXType_VariableArray)                                        = Just CXType_VariableArray
  simpleFromC (#const CXType_DependentSizedArray)                                  = Just CXType_DependentSizedArray
  simpleFromC (#const CXType_MemberPointer)                                        = Just CXType_MemberPointer
  simpleFromC (#const CXType_Auto)                                                 = Just CXType_Auto
  simpleFromC (#const CXType_Elaborated)                                           = Just CXType_Elaborated

  simpleFromC _otherwise = Nothing

{-------------------------------------------------------------------------------
  CXChildVisitResult
-------------------------------------------------------------------------------}

-- | Describes how the traversal of the children of a particular cursor should
-- proceed after visiting a particular child cursor.
--
-- A value of this enumeration type should be returned by each 'CXCursorVisitor'
-- to indicate how 'clang_visitChildren' proceed.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__TRAVERSAL.html#ga99a9058656e696b622fbefaf5207d715>
data CXChildVisitResult =
    -- | Terminates the cursor traversal.
    CXChildVisit_Break

    -- | Continues the cursor traversal with the next sibling of the cursor just
    -- visited, without visiting its children.
  | CXChildVisit_Continue

    -- | Recursively traverse the children of this cursor, using the same
    -- visitor and client data.
  | CXChildVisit_Recurse
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance IsSimpleEnum CXChildVisitResult where
  simpleToC CXChildVisit_Break    = #const CXChildVisit_Break
  simpleToC CXChildVisit_Continue = #const CXChildVisit_Continue
  simpleToC CXChildVisit_Recurse  = #const CXChildVisit_Recurse

  simpleFromC (#const CXChildVisit_Break)    = Just CXChildVisit_Break
  simpleFromC (#const CXChildVisit_Continue) = Just CXChildVisit_Continue
  simpleFromC (#const CXChildVisit_Recurse)  = Just CXChildVisit_Recurse

  simpleFromC _otherwise = Nothing

