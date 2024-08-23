#include <stdlib.h>
#include <string.h>

#include <clang-c/Index.h>

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

CXString* wrap_malloc_getCursorDisplayName(CXCursor* cursor) {
    CXString* result = malloc(sizeof(CXString));
    *result = clang_getCursorDisplayName(*cursor);
    return result;
}

CXString* wrap_malloc_getCursorSpelling(CXCursor* cursor) {
    CXString* result = malloc(sizeof(CXString));
    *result = clang_getCursorSpelling(*cursor);
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

/**
 * Mapping between cursors and source code
 */

CXSourceRange* wrap_malloc_getCursorExtent(CXCursor * cursor) {
    CXSourceRange* result = malloc(sizeof(CXSourceRange));
    *result = clang_getCursorExtent(*cursor);
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

/**
 * String manipulation routines
 */

const char * wrap_getCString (CXString* string) {
    return clang_getCString(*string);
}

void wrap_disposeString(CXString* string) {
    clang_disposeString(*string);
}

