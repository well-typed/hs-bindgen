#ifndef CLANG_WRAPPERS_H
#define CLANG_WRAPPERS_H

/**
 * Wrappers for clang functions that take structs, or return them, by value.
 *
 * For functions that return structs by value, we instead allocate memory and
 * return a pointer. It is the responsibility of the caller to attach a
 * finalizer to these pointers; to remind the caller to do so, these functions
 * are prefixed with @wrap_malloc_@; the other functions are prefixed with
 * @wrap_@ (in both cases this prefixes replaces the @clang_@ prefix).
 */

#include <clang-c/Index.h>

/**
 * Translation unit manipulation
 */

CXString* wrap_malloc_TargetInfo_getTriple(CXTargetInfo Info);


/**
 * Cursor manipulations
 */

CXCursor* wrap_malloc_getTranslationUnitCursor(CXTranslationUnit unit);
unsigned wrap_equalCursors(CXCursor* a, CXCursor* b);
CXCursor* wrap_malloc_getCursorSemanticParent(CXCursor* cursor);
CXCursor* wrap_malloc_getCursorLexicalParent(CXCursor* cursor);
enum CXCursorKind wrap_getCursorKind(CXCursor* cursor);
CXString* wrap_malloc_getCursorKindSpelling(enum CXCursorKind Kind);


/**
 * Traversing the AST with cursors
 */

typedef enum CXChildVisitResult(* HsCXCursorVisitor) (CXCursor* cursor, CXCursor* parent);

unsigned wrap_malloc_visitChildren(CXCursor* parent, HsCXCursorVisitor visitor);

/**
 * Cross-referencing in the AST
 */

CXString* wrap_malloc_getCursorDisplayName(CXCursor* C);
CXString* wrap_malloc_getCursorSpelling(CXCursor* C);
CXCursor* wrap_malloc_getCursorReferenced(CXCursor* C);
CXCursor* wrap_malloc_getCursorDefinition(CXCursor* C);
CXCursor* wrap_malloc_getCanonicalCursor(CXCursor* C);
CXString* wrap_malloc_Cursor_getRawCommentText(CXCursor* C);
CXString* wrap_malloc_Cursor_getBriefCommentText(CXCursor* C);
unsigned wrap_isCursorDefinition(CXCursor *C);
CXSourceRange* wrap_malloc_Cursor_getSpellingNameRange(CXCursor *C, unsigned pieceIndex, unsigned options);

/**
 * Type information for CXCursors
 */

enum CXTypeKind wrap_cxtKind(CXType* type);
CXType* wrap_malloc_getCursorType(CXCursor* C);
CXString* wrap_malloc_getTypeKindSpelling(enum CXTypeKind K);
CXString* wrap_malloc_getTypeSpelling(CXType* CT);
CXType* wrap_malloc_getPointeeType(CXType* T);
long long wrap_Type_getSizeOf(CXType* T);
long long wrap_Type_getAlignOf(CXType* T);
unsigned wrap_Type_isTransparentTagTypedef(CXType *T);
unsigned wrap_Cursor_isAnonymous(CXCursor* C);

static inline long long wrap_getEnumConstantDeclValue(CXCursor *C) {
    return clang_getEnumConstantDeclValue(*C);
}

/**
 * Mapping between cursors and source code
 */

CXSourceLocation* wrap_malloc_getCursorLocation(CXCursor* C);
CXSourceRange* wrap_malloc_getCursorExtent(CXCursor* C);

/**
 * Token extraction and manipulation
 */

CXToken* wrap_getToken(CXTranslationUnit TU, CXSourceLocation* Location);
CXTokenKind wrap_getTokenKind(CXToken* Token);
CXString* wrap_malloc_getTokenSpelling(CXTranslationUnit TU, CXToken* Token);
CXSourceLocation* wrap_malloc_getTokenLocation(CXTranslationUnit TU, CXToken* Token);
CXSourceRange* wrap_malloc_getTokenExtent(CXTranslationUnit TU, CXToken* Token);

/**
 * Physical source locations
 */

CXSourceLocation* wrap_malloc_getRangeStart(CXSourceRange* range);
CXSourceLocation* wrap_malloc_getRangeEnd(CXSourceRange* range);
void wrap_getExpansionLocation(CXSourceLocation* location, CXFile* file, unsigned* line, unsigned* column, unsigned* offset);
void wrap_getSpellingLocation(CXSourceLocation* location, CXFile* file, unsigned* line, unsigned* column, unsigned* offset);
int wrap_Location_isFromMainFile(CXSourceLocation* location);

/**
 * File manipulation routines
 */

CXString* wrap_malloc_getFileName(CXFile SFile);

/**
 * String manipulation routines
 */

const char * wrap_getCString(CXString* string);
void wrap_disposeString(CXString* string);

#endif
