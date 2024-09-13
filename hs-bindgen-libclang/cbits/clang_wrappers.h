#ifndef CLANG_WRAPPERS_H
#define CLANG_WRAPPERS_H

/**
 * Wrappers for clang functions that take structs, or return them, by value.
 *
 * For functions that return structs by value, we instead expect a buffer to be
 * preallocated Haskell-side.
 */

#include <clang-c/Index.h>

/**
 * Translation unit manipulation
 */

static inline void wrap_TargetInfo_getTriple(CXTargetInfo Info, CXString* result) {
    *result = clang_TargetInfo_getTriple(Info);
}

/**
 * Cursor manipulations
 */

static inline void wrap_getTranslationUnitCursor (CXTranslationUnit unit, CXCursor* result) {
    *result = clang_getTranslationUnitCursor(unit);
}

static inline unsigned wrap_equalCursors(const CXCursor* a, const CXCursor* b) {
    return clang_equalCursors(*a, *b);
}

static inline void wrap_getCursorSemanticParent(const CXCursor* cursor, CXCursor* result) {
    *result = clang_getCursorSemanticParent(*cursor);
}

static inline void wrap_getCursorLexicalParent(const CXCursor* cursor, CXCursor* result) {
    *result = clang_getCursorLexicalParent(*cursor);
}

static inline enum CXCursorKind wrap_getCursorKind(const CXCursor* cursor) {
    return clang_getCursorKind(*cursor);
}

static inline void wrap_getCursorKindSpelling(enum CXCursorKind Kind, CXString* result) {
    *result = clang_getCursorKindSpelling(Kind);
}

/**
 * Traversing the AST with cursors
 *
 * NOTE: The visitor is passed the two cursors as pointers, but those pointers
 * are pointers to the /stack/. If these pointers can outlive their scope, then
 * the visitor should copy them to the heap.
 */

typedef enum CXChildVisitResult(*WrapCXCursorVisitor)(CXCursor* cursor, CXCursor* parent);

enum CXChildVisitResult wrap_visitor(CXCursor cursor, CXCursor parent, CXClientData client_data);

static inline unsigned wrap_visitChildren(const CXCursor* parent, WrapCXCursorVisitor visitor) {
    return clang_visitChildren(*parent, &wrap_visitor, visitor);
}

/**
 * Cross-referencing in the AST
 */

static inline void wrap_getCursorDisplayName(const CXCursor* C, CXString* result) {
    *result = clang_getCursorDisplayName(*C);
}

static inline void wrap_getCursorSpelling(const CXCursor* C, CXString*  result) {
    *result = clang_getCursorSpelling(*C);
}

static inline void wrap_getCursorReferenced(const CXCursor* C, CXCursor* result) {
    *result = clang_getCursorReferenced(*C);
}

static inline void wrap_getCursorDefinition(const CXCursor* C, CXCursor* result) {
    *result = clang_getCursorDefinition(*C);
}

static inline void wrap_getCanonicalCursor(const CXCursor* C, CXCursor* result) {
    *result = clang_getCanonicalCursor(*C);
}

static inline void wrap_Cursor_getRawCommentText(const CXCursor* C, CXString* result) {
    *result = clang_Cursor_getRawCommentText(*C);
}

static inline void wrap_Cursor_getBriefCommentText(const CXCursor* C, CXString* result) {
    *result = clang_Cursor_getBriefCommentText(*C);
}

static inline unsigned wrap_isCursorDefinition(const CXCursor *C) {
    return clang_isCursorDefinition(*C);
}

static inline void wrap_Cursor_getSpellingNameRange(const CXCursor *C, unsigned pieceIndex, unsigned options, CXSourceRange* result) {
    *result = clang_Cursor_getSpellingNameRange(*C, pieceIndex, options);
}

/**
 * Type information for CXCursors
 */

static inline enum CXTypeKind wrap_cxtKind(const CXType* type) {
    return type->kind;
}

static inline void wrap_getCursorType(const CXCursor* C, CXType* result) {
    *result = clang_getCursorType(*C);
}

static inline void wrap_getTypeKindSpelling(enum CXTypeKind K, CXString* result) {
    *result = clang_getTypeKindSpelling(K);
}

static inline void wrap_getTypeSpelling(const CXType* CT, CXString* result) {
    *result = clang_getTypeSpelling(*CT);
}

static inline void wrap_getTypedefDeclUnderlyingType(const CXCursor* C, CXType* result) {
    *result = clang_getTypedefDeclUnderlyingType(*C);
}

static inline void wrap_getPointeeType(const CXType* T, CXType* result) {
    *result = clang_getPointeeType(*T);
}

static inline long long wrap_Type_getSizeOf(const CXType* T) {
    return clang_Type_getSizeOf(*T);
}

static inline long long wrap_Type_getAlignOf(const CXType* T) {
    return clang_Type_getAlignOf(*T);
}

static inline unsigned wrap_Type_isTransparentTagTypedef(const CXType *T) {
    return clang_Type_isTransparentTagTypedef(*T);
}

static inline long long wrap_Cursor_getOffsetOfField(const CXCursor* C) {
    return clang_Cursor_getOffsetOfField(*C);
}

static inline unsigned wrap_Cursor_isAnonymous(const CXCursor* C) {
    return clang_Cursor_isAnonymous(*C);
}

static inline long long wrap_getEnumConstantDeclValue(const CXCursor *C) {
    return clang_getEnumConstantDeclValue(*C);
}

static inline void wrap_getCanonicalType(const CXType* T, CXType* result) {
    *result = clang_getCanonicalType(*T);
}

static inline void wrap_getTypedefName(const CXType* CT, CXString* result) {
    *result = clang_getTypedefName(*CT);
}

static inline void wrap_getUnqualifiedType(const CXType* CT, CXType* result) {
#if CINDEX_VERSION_MINOR >= 63
    *result = clang_getUnqualifiedType(*CT);
#else
    result->kind = CXType_Invalid;
#endif
}

static inline void wrap_getTypeDeclaration(const CXType* T, CXCursor* result) {
    *result = clang_getTypeDeclaration(*T);
}

static inline void wrap_Type_getNamedType(const CXType* T, CXType* result) {
    *result = clang_Type_getNamedType(*T);
}

static inline void wrap_Type_getModifiedType(const CXType* T, CXType* result) {
    *result = clang_Type_getModifiedType(*T);
}

static inline void wrap_Type_getValueType(const CXType* CT, CXType* result) {
    *result = clang_Type_getValueType(*CT);
}

/**
 * Mapping between cursors and source code
 */

static inline void wrap_getCursorLocation(const CXCursor* C, CXSourceLocation* result) {
    *result = clang_getCursorLocation(*C);
}

static inline void wrap_getCursorExtent(const CXCursor* C, CXSourceRange* result) {
    *result = clang_getCursorExtent(*C);
}

/**
 * Token extraction and manipulation
 */

static inline CXToken* wrap_getToken(CXTranslationUnit TU, const CXSourceLocation* Location) {
    return clang_getToken(TU, *Location);
}

static inline CXTokenKind wrap_getTokenKind(CXToken* Token) {
    return clang_getTokenKind(*Token);
}

static inline void wrap_getTokenSpelling(CXTranslationUnit TU, CXToken* Token, CXString* result) {
    *result = clang_getTokenSpelling(TU, *Token);
}

static inline void wrap_getTokenLocation(CXTranslationUnit TU, CXToken* Token, CXSourceLocation* result) {
    *result = clang_getTokenLocation(TU, *Token);
}

static inline void wrap_getTokenExtent(CXTranslationUnit TU, CXToken* Token, CXSourceRange* result) {
    *result = clang_getTokenExtent(TU, *Token);
}

static inline void wrap_tokenize(CXTranslationUnit TU, const CXSourceRange* Range, CXToken** Tokens, unsigned* NumTokens) {
    clang_tokenize(TU, *Range, Tokens, NumTokens);
}

/**
 * Physical source locations
 */

static inline void wrap_getRangeStart(const CXSourceRange* range, CXSourceLocation* result) {
    *result = clang_getRangeStart(*range);
}

static inline void wrap_getRangeEnd(const CXSourceRange* range, CXSourceLocation* result) {
    *result = clang_getRangeEnd(*range);
}

static inline void wrap_getExpansionLocation(const CXSourceLocation* location, CXFile* file, unsigned* line, unsigned* column, unsigned* offset) {
    clang_getExpansionLocation(*location, file, line, column, offset);
}

static inline void wrap_getSpellingLocation(const CXSourceLocation* location, CXFile* file, unsigned* line, unsigned* column, unsigned* offset) {
    clang_getSpellingLocation(*location, file, line, column, offset);
}

static inline int wrap_Location_isFromMainFile(const CXSourceLocation* location) {
    return clang_Location_isFromMainFile(*location);
}

/**
 * File manipulation routines
 */

static inline void wrap_getFileName(CXFile SFile, CXString* result) {
    *result = clang_getFileName(SFile);
}

/**
 * String manipulation routines
 */

static inline char* wrap_getCString(const CXString* string) {
    // returning `const char*` confuses ghc
    return (char*) clang_getCString(*string);
}

static inline void wrap_disposeString(const CXString* string) {
    clang_disposeString(*string);
}

#endif
