// Type information for CXCursors https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html
CXType clang_getCursorType (CXCursor C);
CXString clang_getTypeSpelling (CXType CT);
CXType clang_getTypedefDeclUnderlyingType (CXCursor C);
CXType clang_getEnumDeclIntegerType (CXCursor C);
long long clang_getEnumConstantDeclValue (CXCursor C);
unsigned long long clang_getEnumConstantDeclUnsignedValue (CXCursor C);
unsigned clang_Cursor_isBitField (CXCursor C);
int clang_getFieldDeclBitWidth (CXCursor C);
int clang_Cursor_getNumArguments (CXCursor C);
CXCursor clang_Cursor_getArgument (CXCursor C, unsigned i);
// int clang_Cursor_getNumTemplateArguments (CXCursor C); // C++
// enum CXTemplateArgumentKind clang_Cursor_getTemplateArgumentKind (CXCursor C, unsigned I); // C++
// CXType clang_Cursor_getTemplateArgumentType (CXCursor C, unsigned I); // C++
// long long clang_Cursor_getTemplateArgumentValue (CXCursor C, unsigned I); // C++
// unsigned long long clang_Cursor_getTemplateArgumentUnsignedValue (CXCursor C, unsigned I); // C++
unsigned clang_equalTypes (CXType A, CXType B);
CXType clang_getCanonicalType (CXType T);
unsigned clang_isConstQualifiedType (CXType T);
unsigned clang_Cursor_isMacroFunctionLike (CXCursor C);
unsigned clang_Cursor_isMacroBuiltin (CXCursor C);
unsigned clang_Cursor_isFunctionInlined (CXCursor C);
unsigned clang_isVolatileQualifiedType (CXType T);
unsigned clang_isRestrictQualifiedType (CXType T);
unsigned clang_getAddressSpace (CXType T);
CXString clang_getTypedefName (CXType CT);
CXType clang_getPointeeType (CXType T);
// CXType clang_getUnqualifiedType (CXType CT); // this is special case
// CXType clang_getNonReferenceType (CXType CT); // this is apparently special case as well (not available in older libclang)
CXCursor clang_getTypeDeclaration (CXType T);
// CXString clang_getDeclObjCTypeEncoding (CXCursor C); // Objective C
// CXString clang_Type_getObjCEncoding (CXType type); // Objective C
CXString clang_getTypeKindSpelling (enum CXTypeKind K);
// enum CXCallingConv clang_getFunctionTypeCallingConv (CXType T); // no enum
CXType clang_getResultType (CXType T);
// int clang_getExceptionSpecificationType (CXType T); // C++
int clang_getNumArgTypes (CXType T);
CXType clang_getArgType (CXType T, unsigned i);
CXType clang_Type_getObjCObjectBaseType (CXType T);
unsigned clang_Type_getNumObjCProtocolRefs (CXType T);
CXCursor clang_Type_getObjCProtocolDecl (CXType T, unsigned i);
unsigned clang_Type_getNumObjCTypeArgs (CXType T);
CXType clang_Type_getObjCTypeArg (CXType T, unsigned i);
unsigned clang_isFunctionTypeVariadic (CXType T);
CXType clang_getCursorResultType (CXCursor C);
int clang_getCursorExceptionSpecificationType (CXCursor C);
unsigned clang_isPODType (CXType T);
CXType clang_getElementType (CXType T);
long long clang_getNumElements (CXType T);
CXType clang_getArrayElementType (CXType T);
long long clang_getArraySize (CXType T);
CXType clang_Type_getNamedType (CXType T);
unsigned clang_Type_isTransparentTagTypedef (CXType T);
// enum CXTypeNullabilityKind clang_Type_getNullability (CXType T); // no enum
long long clang_Type_getAlignOf (CXType T);
// CXType clang_Type_getClassType (CXType T); // C++
long long clang_Type_getSizeOf (CXType T);
// long long clang_Type_getOffsetOf (CXType T, const char *S) // TODO: generator doesn't know * yet
CXType clang_Type_getModifiedType (CXType T);
CXType clang_Type_getValueType (CXType CT);
long long clang_Cursor_getOffsetOfField (CXCursor C);
unsigned clang_Cursor_isAnonymous (CXCursor C);
unsigned clang_Cursor_isAnonymousRecordDecl (CXCursor C);
// unsigned clang_Cursor_isInlineNamespace (CXCursor C); // C++
// int clang_Type_getNumTemplateArguments (CXType T); // C++
// CXType clang_Type_getTemplateArgumentAsType (CXType T, unsigned i); // C++
// enum CXRefQualifierKind clang_Type_getCXXRefQualifier (CXType T); // C++
// unsigned clang_isVirtualBase (CXCursor); // C++
// enum CX_CXXAccessSpecifier clang_getCXXAccessSpecifier (CXCursor); // C++
// enum CX_BinaryOperatorKind clang_Cursor_getBinaryOpcode (CXCursor C); // C++
// CXString clang_Cursor_getBinaryOpcodeStr (enum CX_BinaryOperatorKind Op); // C++
// enum CX_StorageClass clang_Cursor_getStorageClass (CXCursor); // no enum
// unsigned clang_getNumOverloadedDecls (CXCursor cursor); // C++
// CXCursor clang_getOverloadedDecl (CXCursor cursor, unsigned index); // C++

// Cursor manipulations https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html
CXCursor clang_getNullCursor ();
// CXCursor clang_getTranslationUnitCursor (CXTranslationUnit unit); CXTranslationUnit is defined in LowLevel.Core
unsigned clang_equalCursors (CXCursor A, CXCursor B);
int clang_Cursor_isNull (CXCursor cursor);
unsigned clang_hashCursor (CXCursor cursor);
enum CXCursorKind clang_getCursorKind (CXCursor cursor);
unsigned clang_isDeclaration (enum CXCursorKind k);
unsigned clang_isInvalidDeclaration (CXCursor cursor);
unsigned clang_isReference (enum CXCursorKind k);
unsigned clang_isExpression (enum CXCursorKind k);
unsigned clang_isStatement (enum CXCursorKind k);
unsigned clang_isAttribute (enum CXCursorKind k);
unsigned clang_Cursor_hasAttrs (CXCursor C);
unsigned clang_isInvalid (enum CXCursorKind k);
unsigned clang_isTranslationUnit (enum CXCursorKind k);
unsigned clang_isPreprocessing (enum CXCursorKind k);
unsigned clang_isUnexposed (enum CXCursorKind k);
// enum CXLinkageKind clang_getCursorLinkage (CXCursor cursor); // no enum
// enum CXVisibilityKind clang_getCursorVisibility (CXCursor cursor); // no enum
// enum CXAvailabilityKind clang_getCursorAvailability (CXCursor cursor); // no enum
// int clang_getCursorPlatformAvailability (CXCursor cursor, int *always_deprecated, CXString *deprecated_message, int *always_unavailable, CXString *unavailable_message, CXPlatformAvailability *availability, int availability_size);
// void clang_disposeCXPlatformAvailability (CXPlatformAvailability *availability);
CXCursor clang_Cursor_getVarDeclInitializer (CXCursor cursor);
int clang_Cursor_hasVarDeclGlobalStorage (CXCursor cursor);
 int clang_Cursor_hasVarDeclExternalStorage (CXCursor cursor);
// enum CXLanguageKind clang_getCursorLanguage (CXCursor cursor); // no enum
// enum CXTLSKind clang_getCursorTLSKind (CXCursor cursor); // no enum
// CXTranslationUnit clang_Cursor_getTranslationUnit (CXCursor cursor); CXTranslationUnit is defined in LowLevel.Core
// CXCursorSet clang_createCXCursorSet (void); // no cursor set
// void clang_disposeCXCursorSet (CXCursorSet cset); // no cursor set
// unsigned clang_CXCursorSet_contains (CXCursorSet cset, CXCursor cursor); // no cursor set
// unsigned clang_CXCursorSet_insert (CXCursorSet cset, CXCursor cursor); // no cursor set
CXCursor clang_getCursorSemanticParent (CXCursor cursor);
CXCursor clang_getCursorLexicalParent (CXCursor cursor);
// void clang_getOverriddenCursors (CXCursor cursor, CXCursor **overridden, unsigned *num_overridden); // C++?
// void clang_disposeOverriddenCursors (CXCursor *overridden); // C++?
CXFile clang_getIncludedFile (CXCursor cursor);

// File manipulation routines https://clang.llvm.org/doxygen/group__CINDEX__FILES.html
CXString clang_getFileName(CXFile SFile);

// Debugging facilities https://clang.llvm.org/doxygen/group__CINDEX__DEBUG.html
CXString clang_getCursorKindSpelling (enum CXCursorKind Kind);
// void clang_getDefinitionSpellingAndExtent (CXCursor, const char **startBuf, const char **endBuf, unsigned *startLine, unsigned *startColumn, unsigned *endLine, unsigned *endColumn);
// void clang_enableStackTraces (void);
// void clang_executeOnThread (void(*fn)(void *), void *user_data, unsigned stack_size);
