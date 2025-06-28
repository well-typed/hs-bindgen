-- | Haskell equivalent of C enums using in @libclang@
--
-- This module should only be imported by "Clang.LowLevel.Core".
module Clang.LowLevel.Core.Enums (
    CXErrorCode(..)
  , CXTranslationUnit_Flags(..)
  , CXTypeKind(..)
  , CXChildVisitResult(..)
  , CXTypeLayoutError(..)
  , CXTokenKind(..)
  , CXCursorKind(..)
  , CXDiagnosticDisplayOptions(..)
  , CXDiagnosticSeverity(..)
  , CX_StorageClass(..)
  , CXTLSKind(..)
  ) where

import GHC.Generics (Generic)

{-------------------------------------------------------------------------------
  CXErrorCode
-------------------------------------------------------------------------------}

-- | Error codes returned by @libclang@ routines.
--
-- NOTE: The docs state:
--
-- > Zero (CXError_Success) is the only error code indicating success. Other
-- > error codes, including not yet assigned non-zero values, indicate errors.
--
-- Since we want to reserve 'CXErrorCode' for actual errors, we omit
-- @CXError_Success@, and define 'IsSimpleEnum' for both @CXErrorCode@ and
-- @Maybe CXErrorCode@.
data CXErrorCode =
    -- | A generic error code, no further details are available.
    --
    -- Errors of this kind can get their own specific error codes in future
    -- libclang versions.
    CXError_Failure

    -- | @libclang@ crashed while performing the requested operation.
  | CXError_Crashed

    -- | The function detected that the arguments violate the function contract.
  | CXError_InvalidArguments

    -- | An AST deserialization error has occurred.
  | CXError_ASTReadError
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

{-------------------------------------------------------------------------------
  CXTranslationUnit_Flag
-------------------------------------------------------------------------------}

-- | Flags that control the creation of translation units.
--
-- The enumerators in this enumeration type are meant to be bitwise ORed
-- together to specify which options should be used when constructing the
-- translation unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TRANSLATION__UNIT.html#gab1e4965c1ebe8e41d71e90203a723fe9>
data CXTranslationUnit_Flags =
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
    -- @clang_reparseTranslationUnit@ will re-use the implicit precompiled
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

    -- | Used in combination with 'CXTranslationUnit_SkipFunctionBodies' to
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
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

{-------------------------------------------------------------------------------
  CXTypeKind
-------------------------------------------------------------------------------}

-- | Describes the kind of type
--
-- NOTE: This definition is not complete; we omit kinds for
--
-- * OpenCL
-- * HLSL
-- * BPF/BTF
--
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

    -- | A type whose specific kind is not exposed via this interface.
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
    -- E.g., @struct S@, or via a qualified name, e.g., @N::M::type@, or both.
  | CXType_Elaborated

  | CXType_ObjCObject
  | CXType_ObjCTypeParam
  | CXType_Attributed

  | CXType_ExtVector
  | CXType_Atomic
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

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
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

{-------------------------------------------------------------------------------
  CXTypeLayoutError
-------------------------------------------------------------------------------}

-- | List the possible error codes for 'clang_Type_getSizeOf',
-- 'clang_Type_getAlignOf', 'clang_Type_getOffsetOf' and
-- 'clang_Cursor_getOffsetOf'.
--
-- A value of this enumeration type can be returned if the target type is not a
-- valid argument to @sizeof@, @alignof@ or @offsetof@.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gaaf1b95e9e7e792a08654563fef7502c1>
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
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

{-------------------------------------------------------------------------------
  CXTokenKind
-------------------------------------------------------------------------------}

-- | Describes a kind of token.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LEX.html#gaf63e37eee4280e2c039829af24bbc201>
data CXTokenKind =
    -- | A token that contains some kind of punctuation.
    CXToken_Punctuation

    -- | A language keyword.
  | CXToken_Keyword

    -- | An identifier (that is not a keyword).
  | CXToken_Identifier

    -- | A numeric, string, or character literal.
  | CXToken_Literal

    -- | A comment.
  | CXToken_Comment
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

{-------------------------------------------------------------------------------
  CXCursorKind
-------------------------------------------------------------------------------}

-- | Describes the kind of entity that a cursor refers to.
--
-- Notes:
--
-- * We only include constants available in @llvm-14@ and up.
-- * We omit the various first and last markers (e.g., @CXCursor_FirstExpr@ and
--   @CXCursor_LastExpr@); if we need them, we should define them as separate
--   constants.
-- * We omit aliases (such as @CXCursor_AsmStmt@, an alias for
--   'CXCursor_GCCAsmStmt').
--
-- <https://clang.llvm.org/doxygen/group__CINDEX.html#gaaccc432245b4cd9f2d470913f9ef0013>
data CXCursorKind =
    --
    -- Declarations
    --

    -- | A declaration whose specific kind is not exposed via this interface.
    --
    -- Unexposed declarations have the same operations as any other kind of
    -- declaration; one can extract their location information, spelling, find
    -- their definitions, etc. However, the specific kind of the declaration is
    -- not reported.
    CXCursor_UnexposedDecl

    -- | A C or C++ struct.
  | CXCursor_StructDecl

    -- | A C or C++ union.
  | CXCursor_UnionDecl

    -- | A C++ class.
  | CXCursor_ClassDecl

    -- | An enumeration.
  | CXCursor_EnumDecl

    -- | A field (in C) or non-static data member (in C++) in a struct, union,
    -- or C++ class.
  | CXCursor_FieldDecl

    -- | An enumerator constant.
  | CXCursor_EnumConstantDecl

    -- | A function.
  | CXCursor_FunctionDecl

    -- | A variable.
  | CXCursor_VarDecl

    -- | A function or method parameter.
  | CXCursor_ParmDecl

    -- | An Objective-C \@interface.
  | CXCursor_ObjCInterfaceDecl

    -- | An Objective-C \@interface for a category.
  | CXCursor_ObjCCategoryDecl

    -- | An Objective-C \@protocol declaration.
  | CXCursor_ObjCProtocolDecl

    -- | An Objective-C \@property declaration.
  | CXCursor_ObjCPropertyDecl

    -- | An Objective-C instance variable.
  | CXCursor_ObjCIvarDecl

    -- | An Objective-C instance method.
  | CXCursor_ObjCInstanceMethodDecl

    -- | An Objective-C class method.
  | CXCursor_ObjCClassMethodDecl

    -- | An Objective-C \@implementation.
  | CXCursor_ObjCImplementationDecl

    -- | An Objective-C \@implementation for a category.
  | CXCursor_ObjCCategoryImplDecl

    -- | A typedef.
  | CXCursor_TypedefDecl

    -- | A C++ class method.
  | CXCursor_CXXMethod

    -- | A C++ namespace.
  | CXCursor_Namespace

    -- | A linkage specification, e.g. 'extern "C"'.
  | CXCursor_LinkageSpec

    -- | A C++ constructor.
  | CXCursor_Constructor

    -- | A C++ destructor.
  | CXCursor_Destructor

    -- | A C++ conversion function.
  | CXCursor_ConversionFunction

    -- | A C++ template type parameter.
  | CXCursor_TemplateTypeParameter

    -- | A C++ non-type template parameter.
  | CXCursor_NonTypeTemplateParameter

    -- | A C++ template template parameter.
  | CXCursor_TemplateTemplateParameter

    -- | A C++ function template.
  | CXCursor_FunctionTemplate

    -- | A C++ class template.
  | CXCursor_ClassTemplate

    -- | A C++ class template partial specialization.
  | CXCursor_ClassTemplatePartialSpecialization

    -- | A C++ namespace alias declaration.
  | CXCursor_NamespaceAlias

    -- | A C++ using directive.
  | CXCursor_UsingDirective

    -- | A C++ using declaration.
  | CXCursor_UsingDeclaration

    -- | A C++ alias declaration
  | CXCursor_TypeAliasDecl

    -- | An Objective-C \@synthesize definition.
  | CXCursor_ObjCSynthesizeDecl

    -- | An Objective-C \@dynamic definition.
  | CXCursor_ObjCDynamicDecl

    -- | An access specifier.
  | CXCursor_CXXAccessSpecifier

    --
    -- References
    --

  | CXCursor_ObjCSuperClassRef
  | CXCursor_ObjCProtocolRef
  | CXCursor_ObjCClassRef

    -- | A reference to a type declaration.
    --
    -- A type reference occurs anywhere where a type is named but not declared.
    -- For example, given:
    --
    -- > typedef unsigned size_type;
    -- > size_type size;
    --
    -- The typedef is a declaration of @size_type@ ('CXCursor_TypedefDecl'),
    -- while the type of the variable \"size\" is referenced. The cursor
    -- referenced by the type of size is the typedef for @size_type@.
  | CXCursor_TypeRef

  | CXCursor_CXXBaseSpecifier

    -- | A reference to a class template, function template, template template
    -- parameter, or class template partial specialization.
  | CXCursor_TemplateRef

    -- | A reference to a namespace or namespace alias.
  | CXCursor_NamespaceRef

    -- | A reference to a member of a struct, union, or class that occurs in
    -- some non-expression context, e.g., a designated initializer.
  | CXCursor_MemberRef

    -- | A reference to a labeled statement.
    --
    -- This cursor kind is used to describe the jump to \"start_over\" in the
    -- goto statement in the following example:
    --
    -- > start_over:
    -- >   ++counter;
    -- >
    -- >   goto start_over;
    --
    -- A label reference cursor refers to a label statement.
  | CXCursor_LabelRef

    -- | A reference to a set of overloaded functions or function templates that
    -- has not yet been resolved to a specific function or function template.
    --
    -- An overloaded declaration reference cursor occurs in C++ templates where
    -- a dependent name refers to a function. For example:
    --
    -- > template<typename T> void swap(T&, T&);
    -- >
    -- > struct X { ... };
    -- > void swap(X&, X&);
    -- >
    -- > template<typename T>
    -- > void reverse(T* first, T* last) {
    -- >   while (first < last - 1) {
    -- >     swap(*first, *--last);
    -- >     ++first;
    -- >   }
    -- > }
    -- >
    -- > struct Y { };
    -- > void swap(Y&, Y&);
    --
    -- Here, the identifier \"swap\" is associated with an overloaded
    -- declaration reference. In the template definition, \"swap\" refers to
    -- either of the two "\swap\" functions declared above, so both results will
    -- be available. At instantiation time, \"swap\" may also refer to other
    -- functions found via argument-dependent lookup (e.g., the \"swap\"
    -- function at the end of the example).
    --
    -- The 'functionsclang_getNumOverloadedDecls' and 'clang_getOverloadedDecl'
    -- can be used to retrieve the definitions referenced by this cursor.
  | CXCursor_OverloadedDeclRef

    -- | A reference to a variable that occurs in some non-expression context,
    -- e.g., a C++ lambda capture list.
  | CXCursor_VariableRef

    --
    -- Error conditions
    --

  | CXCursor_InvalidFile
  | CXCursor_NoDeclFound
  | CXCursor_NotImplemented
  | CXCursor_InvalidCode

    --
    -- Expressions
    --

    -- | An expression whose specific kind is not exposed via this interface.
    --
    -- Unexposed expressions have the same operations as any other kind of
    -- expression; one can extract their location information, spelling,
    -- children, etc. However, the specific kind of the expression is not
    -- reported.
  | CXCursor_UnexposedExpr

    -- | An expression that refers to some value declaration, such as a
    -- function, variable, or enumerator.
  | CXCursor_DeclRefExpr

    -- | An expression that refers to a member of a struct, union, class,
    -- Objective-C class, etc.
  | CXCursor_MemberRefExpr

   -- | An expression that calls a function.
  | CXCursor_CallExpr

   -- | An expression that sends a message to an Objective-C object or class.
  | CXCursor_ObjCMessageExpr

   -- | An expression that represents a block literal.
  | CXCursor_BlockExpr

   -- | An integer literal.
  | CXCursor_IntegerLiteral

   -- | A floating point number literal.
  | CXCursor_FloatingLiteral

   -- | An imaginary number literal.
  | CXCursor_ImaginaryLiteral

   -- | A string literal.
  | CXCursor_StringLiteral

   -- | A character literal.
  | CXCursor_CharacterLiteral

   -- | A parenthesized expression, e.g. @"(1)"@.
   --
   -- This AST node is only formed if full location information is requested.
  | CXCursor_ParenExpr

   -- | This represents the unary-expression's (except sizeof and alignof).
  | CXCursor_UnaryOperator

   -- | [C99 6.5.2.1] Array Subscripting.
  | CXCursor_ArraySubscriptExpr

   -- | A builtin binary operation expression such as @"x + y"@ or @"x <= y"@.
  | CXCursor_BinaryOperator

   -- | Compound assignment such as @"+="@.
  | CXCursor_CompoundAssignOperator

   -- | The @?:@ ternary operator.
  | CXCursor_ConditionalOperator

   -- | An explicit cast in C (C99 6.5.4) or a C-style cast in C++ (C++
   -- [expr.cast]), which uses the syntax (Type)expr.
   --
   -- For example: @(int)f@.
  | CXCursor_CStyleCastExpr

   -- | [C99 6.5.2.5]
  | CXCursor_CompoundLiteralExpr

   -- | Describes an C or C++ initializer list.
  | CXCursor_InitListExpr

   -- | The GNU address of label extension, representing &&label.
  | CXCursor_AddrLabelExpr

   -- | This is the GNU Statement Expression extension: ({int X=4; X;})
  | CXCursor_StmtExpr

   -- | Represents a C11 generic selection.
  | CXCursor_GenericSelectionExpr

   -- | Implements the GNU @__null@ extension, which is a name for a null
   -- pointer constant that has integral type (e.g., int or long) and is the
   -- same size and alignment as a pointer.
   --
   -- The @__null@ extension is typically only used by system headers, which
   -- define NULL as @__null@ in C++ rather than using 0 (which is an integer
   -- that may not match the size of a pointer).
  | CXCursor_GNUNullExpr

   -- | C++'s static_cast<> expression.
  | CXCursor_CXXStaticCastExpr

   -- | C++'s dynamic_cast<> expression.
  | CXCursor_CXXDynamicCastExpr

   -- | C++'s reinterpret_cast<> expression.
  | CXCursor_CXXReinterpretCastExpr

   -- | C++'s const_cast<> expression.
  | CXCursor_CXXConstCastExpr

    -- | Represents an explicit C++ type conversion that uses "functional"
    -- notion (C++ [expr.type.conv]).
    --
    -- Example:
    --
    -- > x = int(0.5);
  | CXCursor_CXXFunctionalCastExpr

   -- | A C++ typeid expression (C++ [expr.typeid]).
  | CXCursor_CXXTypeidExpr

   -- | [C++ 2.13.5] C++ Boolean Literal.
  | CXCursor_CXXBoolLiteralExpr

   -- | [C++0x 2.14.7] C++ Pointer Literal.
  | CXCursor_CXXNullPtrLiteralExpr

   -- | Represents the "this" expression in C++
  | CXCursor_CXXThisExpr

    -- | [C++ 15] C++ Throw Expression.
    --
    -- This handles @throw@ and @throw@ assignment-expression. When
    -- assignment-expression isn't present, Op will be null.
  | CXCursor_CXXThrowExpr

    -- | A new expression for memory allocation and constructor calls, e.g:
    -- @"new CXXNewExpr(foo)"@.
  | CXCursor_CXXNewExpr

   -- | A delete expression for memory deallocation and destructor calls, e.g.
   -- @"delete[] pArray"@.
  | CXCursor_CXXDeleteExpr

   -- | A unary expression. (noexcept, sizeof, or other traits)
  | CXCursor_UnaryExpr

   -- | An Objective-C string literal i.e. @"foo".
  | CXCursor_ObjCStringLiteral

   -- | An Objective-C \@encode expression.
  | CXCursor_ObjCEncodeExpr

   -- | An Objective-C \@selector expression.
  | CXCursor_ObjCSelectorExpr

   -- | An Objective-C \@protocol expression.
  | CXCursor_ObjCProtocolExpr

    -- | An Objective-C "bridged" cast expression, which casts between
    -- Objective-C pointers and C pointers, transferring ownership in the
    -- process.
    --
    -- > NSString *str = (__bridge_transfer NSString *)CFCreateString();
  | CXCursor_ObjCBridgedCastExpr

    -- | Represents a C++0x pack expansion that produces a sequence of
    -- expressions.
    --
    -- A pack expansion expression contains a pattern (which itself is an
    -- expression) followed by an ellipsis. For example:
    --
    -- > template<typename F, typename ...Types>
    -- > void forward(F f, Types &&...args) {
    -- >  f(static_cast<Types&&>(args)...);
    -- > }
  | CXCursor_PackExpansionExpr

    -- | Represents an expression that computes the length of a parameter pack.
    --
    -- > template<typename ...Types>
    -- > struct count {
    -- >   static const unsigned value = sizeof...(Types);
    -- > };
  | CXCursor_SizeOfPackExpr

    -- | Represents a C++ lambda expression that produces a local function
    -- object.
    --
    -- > void abssort(float *x, unsigned N) {
    -- >   std::sort(x, x + N,
    -- >             [](float a, float b) {
    -- >               return std::abs(a) < std::abs(b);
    -- >             });
    -- > }
  | CXCursor_LambdaExpr

   -- | Objective-c Boolean Literal.
  | CXCursor_ObjCBoolLiteralExpr

   -- | Represents the "self" expression in an Objective-C method.
  | CXCursor_ObjCSelfExpr

    -- | Represents an @available(...) check.
  | CXCursor_ObjCAvailabilityCheckExpr

    -- | Fixed point literal
  | CXCursor_FixedPointLiteral

    -- | OpenMP 5.0 [2.1.4, Array Shaping].
  | CXCursor_OMPArrayShapingExpr

    -- | OpenMP 5.0 [2.1.6 Iterators]
  | CXCursor_OMPIteratorExpr

    -- | OpenCL's addrspace_cast<> expression.
  | CXCursor_CXXAddrspaceCastExpr

    --
    -- Statements
    --

    -- | A statement whose specific kind is not exposed via this interface.
    --
    -- Unexposed statements have the same operations as any other kind of
    -- statement; one can extract their location information, spelling,
    -- children, etc. However, the specific kind of the statement is not
    -- reported.
  | CXCursor_UnexposedStmt

    -- | A labelled statement in a function.
    --
    -- This cursor kind is used to describe the "start_over:" label statement in
    -- the following example:
    --
    -- > start_over:
    -- >   ++counter;
  | CXCursor_LabelStmt

    -- | A group of statements like @{ stmt stmt }@.
    --
    -- This cursor kind is used to describe compound statements, e.g. function
    -- bodies.
  | CXCursor_CompoundStmt

    -- | A case statement.
  | CXCursor_CaseStmt

    -- | A default statement.
  | CXCursor_DefaultStmt

    -- | An if statement
  | CXCursor_IfStmt

    -- | A switch statement.
  | CXCursor_SwitchStmt

    -- | A while statement.
  | CXCursor_WhileStmt

    -- | A do statement.
  | CXCursor_DoStmt

    -- | A for statement.
  | CXCursor_ForStmt

    -- | A goto statement.
  | CXCursor_GotoStmt

    -- | An indirect goto statement.
  | CXCursor_IndirectGotoStmt

    -- | A continue statement.
  | CXCursor_ContinueStmt

    -- | A break statement.
  | CXCursor_BreakStmt

    -- | A return statement.
  | CXCursor_ReturnStmt

    -- | A GCC inline assembly statement extension.
  | CXCursor_GCCAsmStmt

    -- | Objective-C's overall \@try-\@catch-\@finally statement.
  | CXCursor_ObjCAtTryStmt

    -- | Objective-C's \@catch statement.
  | CXCursor_ObjCAtCatchStmt

    -- | Objective-C's \@finally statement.
  | CXCursor_ObjCAtFinallyStmt

    -- | Objective-C's \@throw statement.
  | CXCursor_ObjCAtThrowStmt

    -- | Objective-C's \@synchronized statement.
  | CXCursor_ObjCAtSynchronizedStmt

    -- | Objective-C's autorelease pool statement.
  | CXCursor_ObjCAutoreleasePoolStmt

    -- | Objective-C's collection statement.
  | CXCursor_ObjCForCollectionStmt

    -- | C++'s catch statement.
  | CXCursor_CXXCatchStmt

    -- | C++'s try statement.
  | CXCursor_CXXTryStmt

    -- | C++'s for (* : *) statement.
  | CXCursor_CXXForRangeStmt

    -- | Windows Structured Exception Handling's try statement.
  | CXCursor_SEHTryStmt

    -- | Windows Structured Exception Handling's except statement.
  | CXCursor_SEHExceptStmt

    -- | Windows Structured Exception Handling's finally statement.
  | CXCursor_SEHFinallyStmt

    -- | A MS inline assembly statement extension.
  | CXCursor_MSAsmStmt

    -- | The null statement ";": C99 6.8.3p3.
    --
    -- This cursor kind is used to describe the null statement.
  | CXCursor_NullStmt

   -- | Adaptor class for mixing declarations with statements and expressions.
  | CXCursor_DeclStmt

   -- | OpenMP parallel directive.
  | CXCursor_OMPParallelDirective

   -- | OpenMP SIMD directive.
  | CXCursor_OMPSimdDirective

   -- | OpenMP for directive.
  | CXCursor_OMPForDirective

   -- | OpenMP sections directive.
  | CXCursor_OMPSectionsDirective

   -- | OpenMP section directive.
  | CXCursor_OMPSectionDirective

   -- | OpenMP single directive.
  | CXCursor_OMPSingleDirective

   -- | OpenMP parallel for directive.
  | CXCursor_OMPParallelForDirective

   -- | OpenMP parallel sections directive.
  | CXCursor_OMPParallelSectionsDirective

   -- | OpenMP task directive.
  | CXCursor_OMPTaskDirective

   -- | OpenMP master directive.
  | CXCursor_OMPMasterDirective

   -- | OpenMP critical directive.
  | CXCursor_OMPCriticalDirective

   -- | OpenMP taskyield directive.
  | CXCursor_OMPTaskyieldDirective

   -- | OpenMP barrier directive.
  | CXCursor_OMPBarrierDirective

   -- | OpenMP taskwait directive.
  | CXCursor_OMPTaskwaitDirective

   -- | OpenMP flush directive.
  | CXCursor_OMPFlushDirective

   -- | Windows Structured Exception Handling's leave statement.
  | CXCursor_SEHLeaveStmt

   -- | OpenMP ordered directive.
  | CXCursor_OMPOrderedDirective

   -- | OpenMP atomic directive.
  | CXCursor_OMPAtomicDirective

   -- | OpenMP for SIMD directive.
  | CXCursor_OMPForSimdDirective

   -- | OpenMP parallel for SIMD directive.
  | CXCursor_OMPParallelForSimdDirective

   -- | OpenMP target directive.
  | CXCursor_OMPTargetDirective

   -- | OpenMP teams directive.
  | CXCursor_OMPTeamsDirective

   -- | OpenMP taskgroup directive.
  | CXCursor_OMPTaskgroupDirective

   -- | OpenMP cancellation point directive.
  | CXCursor_OMPCancellationPointDirective

   -- | OpenMP cancel directive.
  | CXCursor_OMPCancelDirective

   -- | OpenMP target data directive.
  | CXCursor_OMPTargetDataDirective

   -- | OpenMP taskloop directive.
  | CXCursor_OMPTaskLoopDirective

   -- | OpenMP taskloop simd directive.
  | CXCursor_OMPTaskLoopSimdDirective

   -- | OpenMP distribute directive.
  | CXCursor_OMPDistributeDirective

   -- | OpenMP target enter data directive.
  | CXCursor_OMPTargetEnterDataDirective

   -- | OpenMP target exit data directive.
  | CXCursor_OMPTargetExitDataDirective

   -- | OpenMP target parallel directive.
  | CXCursor_OMPTargetParallelDirective

   -- | OpenMP target parallel for directive.
  | CXCursor_OMPTargetParallelForDirective

   -- | OpenMP target update directive.
  | CXCursor_OMPTargetUpdateDirective

   -- | OpenMP distribute parallel for directive.
  | CXCursor_OMPDistributeParallelForDirective

   -- | OpenMP distribute parallel for simd directive.
  | CXCursor_OMPDistributeParallelForSimdDirective

   -- | OpenMP distribute simd directive.
  | CXCursor_OMPDistributeSimdDirective

   -- | OpenMP target parallel for simd directive.
  | CXCursor_OMPTargetParallelForSimdDirective

   -- | OpenMP target simd directive.
  | CXCursor_OMPTargetSimdDirective

   -- | OpenMP teams distribute directive.
  | CXCursor_OMPTeamsDistributeDirective

   -- | OpenMP teams distribute simd directive.
  | CXCursor_OMPTeamsDistributeSimdDirective

   -- | OpenMP teams distribute parallel for simd directive.
  | CXCursor_OMPTeamsDistributeParallelForSimdDirective

   -- | OpenMP teams distribute parallel for directive.
  | CXCursor_OMPTeamsDistributeParallelForDirective

   -- | OpenMP target teams directive.
  | CXCursor_OMPTargetTeamsDirective

   -- | OpenMP target teams distribute directive.
  | CXCursor_OMPTargetTeamsDistributeDirective

   -- | OpenMP target teams distribute parallel for directive.
  | CXCursor_OMPTargetTeamsDistributeParallelForDirective

   -- | OpenMP target teams distribute parallel for simd directive.
  | CXCursor_OMPTargetTeamsDistributeParallelForSimdDirective

   -- | OpenMP target teams distribute simd directive.
  | CXCursor_OMPTargetTeamsDistributeSimdDirective

   -- | C++2a std::bit_cast expression.
  | CXCursor_BuiltinBitCastExpr

   -- | OpenMP master taskloop directive.
  | CXCursor_OMPMasterTaskLoopDirective

   -- | OpenMP parallel master taskloop directive.
  | CXCursor_OMPParallelMasterTaskLoopDirective

   -- | OpenMP master taskloop simd directive.
  | CXCursor_OMPMasterTaskLoopSimdDirective

   -- | OpenMP parallel master taskloop simd directive.
  | CXCursor_OMPParallelMasterTaskLoopSimdDirective

   -- | OpenMP parallel master directive.
  | CXCursor_OMPParallelMasterDirective

   -- | OpenMP depobj directive.
  | CXCursor_OMPDepobjDirective

   -- | OpenMP scan directive.
  | CXCursor_OMPScanDirective

   -- | OpenMP tile directive.
  | CXCursor_OMPTileDirective

   -- | OpenMP canonical loop.
  | CXCursor_OMPCanonicalLoop

   -- | OpenMP interop directive.
  | CXCursor_OMPInteropDirective

   -- | OpenMP dispatch directive.
  | CXCursor_OMPDispatchDirective

   -- | OpenMP masked directive.
  | CXCursor_OMPMaskedDirective

   -- | OpenMP unroll directive.
  | CXCursor_OMPUnrollDirective

   -- | OpenMP metadirective directive.
  | CXCursor_OMPMetaDirective

   -- | OpenMP loop directive.
  | CXCursor_OMPGenericLoopDirective

    -- | Cursor that represents the translation unit itself.
    --
    -- The translation unit cursor exists primarily to act as the root cursor
    -- for traversing the contents of a translation unit.
  | CXCursor_TranslationUnit

    --
    -- Attributes
    --

    -- | An attribute whose specific kind is not exposed via this interface.
  | CXCursor_UnexposedAttr

  | CXCursor_IBActionAttr
  | CXCursor_IBOutletAttr
  | CXCursor_IBOutletCollectionAttr
  | CXCursor_CXXFinalAttr
  | CXCursor_CXXOverrideAttr
  | CXCursor_AnnotateAttr
  | CXCursor_AsmLabelAttr
  | CXCursor_PackedAttr
  | CXCursor_PureAttr
  | CXCursor_ConstAttr
  | CXCursor_NoDuplicateAttr
  | CXCursor_CUDAConstantAttr
  | CXCursor_CUDADeviceAttr
  | CXCursor_CUDAGlobalAttr
  | CXCursor_CUDAHostAttr
  | CXCursor_CUDASharedAttr
  | CXCursor_VisibilityAttr
  | CXCursor_DLLExport
  | CXCursor_DLLImport
  | CXCursor_NSReturnsRetained
  | CXCursor_NSReturnsNotRetained
  | CXCursor_NSReturnsAutoreleased
  | CXCursor_NSConsumesSelf
  | CXCursor_NSConsumed
  | CXCursor_ObjCException
  | CXCursor_ObjCNSObject
  | CXCursor_ObjCIndependentClass
  | CXCursor_ObjCPreciseLifetime
  | CXCursor_ObjCReturnsInnerPointer
  | CXCursor_ObjCRequiresSuper
  | CXCursor_ObjCRootClass
  | CXCursor_ObjCSubclassingRestricted
  | CXCursor_ObjCExplicitProtocolImpl
  | CXCursor_ObjCDesignatedInitializer
  | CXCursor_ObjCRuntimeVisible
  | CXCursor_ObjCBoxable
  | CXCursor_FlagEnum
  | CXCursor_ConvergentAttr
  | CXCursor_WarnUnusedAttr
  | CXCursor_WarnUnusedResultAttr
  | CXCursor_AlignedAttr

    --
    -- Preprocessing
    --

  | CXCursor_PreprocessingDirective
  | CXCursor_MacroDefinition
  | CXCursor_MacroExpansion
  | CXCursor_InclusionDirective

    --
    -- Extra Declarations
    --

    -- | A module import declaration.
  | CXCursor_ModuleImportDecl

  | CXCursor_TypeAliasTemplateDecl

   -- | A static_assert or _Static_assert node
  | CXCursor_StaticAssert

   -- | a friend declaration.
  | CXCursor_FriendDecl

    -- | A code completion overload candidate.
  | CXCursor_OverloadCandidate
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

{-------------------------------------------------------------------------------
  CXDiagnosticDisplayOptions
-------------------------------------------------------------------------------}

-- | Options to control the display of diagnostics.
--
-- The values in this enum are meant to be combined to customize the behavior of
-- 'clang_formatDiagnostic'.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga0545c7c3ef36a397c44d142b0385b8d1>
data CXDiagnosticDisplayOptions =
    -- | Display the source-location information where the diagnostic was
    -- located.
    --
    -- When set, diagnostics will be prefixed by the file, line, and
    -- (optionally) column to which the diagnostic refers. For example,
    --
    -- > test.c:28: warning: extra tokens at end of #endif directive
    --
    -- This option corresponds to the clang flag @-fshow-source-location@.
    CXDiagnostic_DisplaySourceLocation

    -- | If displaying the source-location information of the diagnostic, also
    -- include the column number.
    --
    -- | This option corresponds to the clang flag @-fshow-column@.
  | CXDiagnostic_DisplayColumn

    -- | If displaying the source-location information of the diagnostic, also
    -- include information about source ranges in a machine-parsable format.
    --
    -- This option corresponds to the clang flag
    -- @-fdiagnostics-print-source-range-info@.
  | CXDiagnostic_DisplaySourceRanges

    -- | Display the option name associated with this diagnostic, if any.
    --
    -- The option name displayed (e.g., @-Wconversion@) will be placed in
    -- brackets after the diagnostic text. This option corresponds to the clang
    -- flag @-fdiagnostics-show-option@.
  | CXDiagnostic_DisplayOption

    -- | Display the category number associated with this diagnostic, if any.
    --
    -- The category number is displayed within brackets after the diagnostic
    -- text. This option corresponds to the clang flag
    -- @-fdiagnostics-show-category=id@.
  | CXDiagnostic_DisplayCategoryId

    -- | Display the category name associated with this diagnostic, if any.
    --
    -- The category name is displayed within brackets after the diagnostic text.
    -- This option corresponds to the clang flag
    -- @-fdiagnostics-show-category=name@.
  | CXDiagnostic_DisplayCategoryName
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

{-------------------------------------------------------------------------------
  CXDiagnosticSeverity
-------------------------------------------------------------------------------}

-- | Describes the severity of a particular diagnostic.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#gabff210a02d448bf64e8aee79b2241370>
data CXDiagnosticSeverity =
    -- | A diagnostic that has been suppressed, e.g., by a command-line option.
    CXDiagnostic_Ignored

    -- | This diagnostic is a note that should be attached to the previous
    -- (non-note) diagnostic.
  | CXDiagnostic_Note

    -- | This diagnostic indicates suspicious code that may not be wrong.
  | CXDiagnostic_Warning

    -- | This diagnostic indicates that the code is ill-formed.
  | CXDiagnostic_Error

    -- | This diagnostic indicates that the code is ill-formed such that future
    -- parser recovery is unlikely to produce useful results.
  | CXDiagnostic_Fatal
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

{-------------------------------------------------------------------------------
  CX_StorageClass
-------------------------------------------------------------------------------}

-- | Represents the storage classes as declared in the source.
--
-- NOTE: We omit @CX_SC_Invalid@ (zero) for the case that the passed cursor in
-- not a declaration (and throw an error instead).
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga03a15eaa53465d7f3ce7d88743241d7e>
data CX_StorageClass =
    CX_SC_None
  | CX_SC_Extern
  | CX_SC_Static
  | CX_SC_PrivateExtern
  | CX_SC_OpenCLWorkGroupLocal
  | CX_SC_Auto
  | CX_SC_Register
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

{-------------------------------------------------------------------------------
  CXTLSKind
-------------------------------------------------------------------------------}

-- | Describe the \"thread-local storage (TLS) kind\" of the declaration
-- referred to by a cursor.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#ga4e9aabb46d683642ef49f542be4f1257>
data CXTLSKind =
    CXTLS_None
  | CXTLS_Dynamic
  | CXTLS_Static
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
