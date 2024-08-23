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
 * Cursor manipulations
 */

CXCursor* wrap_malloc_getTranslationUnitCursor(CXTranslationUnit unit);
unsigned wrap_equalCursors(CXCursor* a, CXCursor* b);

/**
 * Traversing the AST with cursors
 */

typedef enum CXChildVisitResult(* HsCXCursorVisitor) (CXCursor* cursor, CXCursor* parent);

unsigned wrap_malloc_visitChildren(CXCursor* parent, HsCXCursorVisitor visitor);

/**
 * Cross-referencing in the AST
 */

CXString* wrap_malloc_getCursorDisplayName(CXCursor* cursor);
CXString* wrap_malloc_getCursorSpelling(CXCursor* cursor);
CXString* wrap_malloc_Cursor_getRawCommentText(CXCursor* C);
CXString* wrap_malloc_Cursor_getBriefCommentText(CXCursor* C);

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


/**
 * Mapping between cursors and source code
 */

CXSourceRange* wrap_malloc_getCursorExtent(CXCursor *);

/**
 * Physical source locations
 */

CXSourceLocation* wrap_malloc_getRangeStart(CXSourceRange* range);
CXSourceLocation* wrap_malloc_getRangeEnd(CXSourceRange* range);
void wrap_getExpansionLocation(CXSourceLocation* location, CXFile* file, unsigned* line, unsigned* column, unsigned* offset);

/**
 * String manipulation routines
 */

const char * wrap_getCString(CXString* string);
void wrap_disposeString(CXString* string);

#endif