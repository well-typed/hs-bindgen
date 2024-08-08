-- | Haskell equivalent of C enums using in @libclang@
--
-- This module should only be imported by "HsBingen.Clang.LowLevel".
module HsBindgen.C.Clang.Enums (
    CXTranslationUnit_Flag(..)
  , CXTypeKind(..)
  , CXChildVisitResult(..)
  , CXTypeLayoutError(..)
  ) where

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

{-------------------------------------------------------------------------------
  CXTypeLayoutError
-------------------------------------------------------------------------------}

-- | List the possible error codes for 'clang_Type_getSizeOf',
-- 'clang_Type_getAlignOf', 'clang_Type_getOffsetOf' and
-- 'clang_Cursor_getOffsetOf'.
--
-- A value of this enumeration type can be returned if the target type is not a
-- valid argument to @sizeof@, @alignof@ or @offsetof@.
data CXTypeLayoutError =
    -- | Type is of kind 'CXType_Invalid'.
    CXTypeLayoutError_Invalid

     -- | The type is an incomplete Type.
  | CXTypeLayoutError_Incomplete

    -- | The type is a dependent Type.
  | CXTypeLayoutError_Dependent

    -- | The type is not a constant size type.
  | CXTypeLayoutError_NotConstantSize

    -- | The Field name is not valid for this record.
  | CXTypeLayoutError_InvalidFieldName

    -- | The type is undeduced.
  | CXTypeLayoutError_Undeduced
  deriving stock (Show, Eq, Ord, Enum, Bounded)