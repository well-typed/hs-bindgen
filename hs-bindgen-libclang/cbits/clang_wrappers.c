#include <stdlib.h>

#include "clang_wrappers.h"

/**
 * Cursor manipulations
 */

CXCursor* wrap_malloc_getTranslationUnitCursor (CXTranslationUnit unit) {
    CXCursor *result = malloc(sizeof(CXCursor));
    *result = clang_getTranslationUnitCursor(unit);
    return result;
}

unsigned wrap_equalCursors(CXCursor* a, CXCursor* b) {
    return clang_equalCursors(*a, *b);
}

/**
 * Traversing the AST with cursors
 */

enum CXChildVisitResult wrap_HsCXCursorVisitor(CXCursor cursor, CXCursor parent, CXClientData client_data) {
    HsCXCursorVisitor visitor = client_data;
    CXCursor* cursor_ = malloc(sizeof(CXCursor));
    CXCursor* parent_ = malloc(sizeof(CXCursor));
    *cursor_ = cursor;
    *parent_ = parent;
    return visitor(cursor_, parent_);
}

unsigned wrap_malloc_visitChildren(CXCursor* parent, HsCXCursorVisitor visitor) {
    return clang_visitChildren(*parent, &wrap_HsCXCursorVisitor, visitor);
}

/**
 * Cross-referencing in the AST
 */

CXString* wrap_malloc_getCursorDisplayName(CXCursor* C) {
    CXString* result = malloc(sizeof(CXString));
    *result = clang_getCursorDisplayName(*C);
    return result;
}

CXString* wrap_malloc_getCursorSpelling(CXCursor* C) {
    CXString* result = malloc(sizeof(CXString));
    *result = clang_getCursorSpelling(*C);
    return result;
}

CXCursor* wrap_malloc_getCursorReferenced(CXCursor* C) {
    CXCursor* result = malloc(sizeof(CXCursor));
    *result = clang_getCursorReferenced(*C);
    return result;
}

CXCursor* wrap_malloc_getCursorDefinition(CXCursor* C) {
    CXCursor* result = malloc(sizeof(CXCursor));
    *result = clang_getCursorDefinition(*C);
    return result;
}

CXCursor* wrap_malloc_getCanonicalCursor(CXCursor* C) {
    CXCursor* result = malloc(sizeof(CXCursor));
    *result = clang_getCanonicalCursor(*C);
    return result;
}


CXString* wrap_malloc_Cursor_getRawCommentText(CXCursor* C) {
    CXString* result = malloc(sizeof(CXString));
    *result = clang_Cursor_getRawCommentText(*C);
    return result;
}

CXString* wrap_malloc_Cursor_getBriefCommentText(CXCursor* C) {
    CXString* result = malloc(sizeof(CXString));
    *result = clang_Cursor_getBriefCommentText(*C);
    return result;
}

unsigned wrap_isCursorDefinition(CXCursor *C) {
    return clang_isCursorDefinition(*C);
}

CXSourceRange* wrap_malloc_Cursor_getSpellingNameRange(CXCursor *C, unsigned pieceIndex, unsigned options) {
    CXSourceRange* result = malloc(sizeof(CXSourceRange));
    *result = clang_Cursor_getSpellingNameRange(*C, pieceIndex, options);
    return result;
}

/**
 * Type information for CXCursors
 */

enum CXTypeKind wrap_cxtKind(CXType* type) {
    return type->kind;
}

CXType* wrap_malloc_getCursorType(CXCursor* C) {
    CXType* result = malloc(sizeof(CXType));
    *result = clang_getCursorType(*C);
    return result;
}

CXString* wrap_malloc_getTypeKindSpelling(enum CXTypeKind K) {
    CXString* result = malloc(sizeof(CXString));
    *result = clang_getTypeKindSpelling(K);
    return result;
}

CXString* wrap_malloc_getTypeSpelling(CXType* CT) {
    CXString* result = malloc(sizeof(CXString));
    *result = clang_getTypeSpelling(*CT);
    return result;
}

CXType* wrap_malloc_getPointeeType(CXType* T) {
    CXType* result = malloc(sizeof(CXType));
    *result = clang_getPointeeType(*T);
    return result;
}

long long wrap_Type_getSizeOf(CXType* T) {
    return clang_Type_getSizeOf(*T);
}

long long wrap_Type_getAlignOf(CXType* T) {
    return clang_Type_getAlignOf(*T);
}

unsigned wrap_Type_isTransparentTagTypedef(CXType *T) {
    return clang_Type_isTransparentTagTypedef(*T);
}

unsigned wrap_Cursor_isAnonymous(CXCursor* C) {
    return clang_Cursor_isAnonymous(*C);
}

/**
 * Mapping between cursors and source code
 */

CXSourceRange* wrap_malloc_getCursorExtent(CXCursor* C) {
    CXSourceRange* result = malloc(sizeof(CXSourceRange));
    *result = clang_getCursorExtent(*C);
    return result;
}

/**
 * Token extraction and manipulation
 */

CXToken* wrap_getToken(CXTranslationUnit TU, CXSourceLocation* Location) {
    return clang_getToken(TU, *Location);
}

CXTokenKind wrap_getTokenKind(CXToken* Token) {
    return clang_getTokenKind(*Token);
}

CXString* wrap_malloc_getTokenSpelling(CXTranslationUnit TU, CXToken* Token) {
    CXString* result = malloc(sizeof(CXString));
    *result = clang_getTokenSpelling(TU, *Token);
    return result;
}

CXSourceLocation* wrap_malloc_getTokenLocation(CXTranslationUnit TU, CXToken* Token) {
    CXSourceLocation* result = malloc(sizeof(CXSourceLocation));
    *result = clang_getTokenLocation(TU, *Token);
    return result;
}

CXSourceRange* wrap_malloc_getTokenExtent(CXTranslationUnit TU, CXToken* Token) {
    CXSourceRange* result = malloc(sizeof(CXSourceRange));
    *result = clang_getTokenExtent(TU, *Token);
    return result;
}

/**
 * Physical source locations
 */

CXSourceLocation* wrap_malloc_getRangeStart(CXSourceRange* range) {
    CXSourceLocation* result = malloc(sizeof(CXSourceLocation));
    *result = clang_getRangeStart(*range);
    return result;
}

CXSourceLocation* wrap_malloc_getRangeEnd(CXSourceRange* range) {
    CXSourceLocation* result = malloc(sizeof(CXSourceLocation));
    *result = clang_getRangeEnd(*range);
    return result;
}

void wrap_getExpansionLocation(CXSourceLocation* location, CXFile* file, unsigned* line, unsigned* column, unsigned* offset) {
    clang_getExpansionLocation(*location, file, line, column, offset);
}

void wrap_getSpellingLocation(CXSourceLocation* location, CXFile* file, unsigned* line, unsigned* column, unsigned* offset) {
    clang_getSpellingLocation(*location, file, line, column, offset);
}

/**
 * File manipulation routines
 */

CXString* wrap_malloc_getFileName(CXFile SFile) {
    CXString* result = malloc(sizeof(CXString));
    *result = clang_getFileName(SFile);
    return result;
}

/**
 * String manipulation routines
 */

const char * wrap_getCString (CXString* string) {
    return clang_getCString(*string);
}

void wrap_disposeString(CXString* string) {
    clang_disposeString(*string);
}
