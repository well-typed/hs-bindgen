#ifndef CLANG_WRAPPERS_H
#define CLANG_WRAPPERS_H

#include <clang-c/Index.h>
#include <stdio.h>
#include "clang_wrappers_ffi.h"

/**
 * Wrappers for clang functions that take structs, or return them, by value.
 *
 * For functions that return structs by value, we instead expect a buffer to be
 * preallocated Haskell-side.
 */

/**
 * Diagnostic reporting
 */

static inline void wrap_formatDiagnostic(CXDiagnostic Diagnostic, unsigned Options, CXString* result) {
    *result = clang_formatDiagnostic(Diagnostic, Options);
}

static inline void wrap_getDiagnosticLocation(CXDiagnostic Diagnostic, CXSourceLocation* result) {
    *result = clang_getDiagnosticLocation(Diagnostic);
}

static inline void wrap_getDiagnosticSpelling(CXDiagnostic Diagnostic, CXString* result) {
    *result = clang_getDiagnosticSpelling(Diagnostic);
}

static inline void wrap_getDiagnosticOption(CXDiagnostic Diag, CXString* Disable, CXString* result) {
    *result = clang_getDiagnosticOption(Diag, Disable);
}

static inline void wrap_getDiagnosticCategoryText(CXDiagnostic Diag, CXString* result) {
    *result = clang_getDiagnosticCategoryText(Diag);
}

static inline void wrap_getDiagnosticRange(CXDiagnostic Diagnostic, unsigned Range, CXSourceRange* result) {
    *result = clang_getDiagnosticRange(Diagnostic, Range);
}

static inline void wrap_getDiagnosticFixIt(CXDiagnostic Diagnostic, unsigned FixIt, CXSourceRange* ReplacementRange, CXString* result) {
    *result = clang_getDiagnosticFixIt(Diagnostic, FixIt, ReplacementRange);
}

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

static inline CXTranslationUnit wrap_Cursor_getTranslationUnit(const CXCursor* cursor) {
    return clang_Cursor_getTranslationUnit(*cursor);
}

static inline enum CXTLSKind wrap_getCursorTLSKind(const CXCursor* cursor) {
    return clang_getCursorTLSKind(*cursor);
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

static inline signed int wrap_compareTypes(const CXType *A, const CXType *B) {
    if (A->data[0] < B->data[0]) {
        return -1;
    } else if (A->data[0] > B->data[0]) {
        return +1;
    } else {
        if (A->data[1] < B->data[1]) {
            return -1;
        } else if(A->data[1] > B->data[1]) {
            return +1;
        } else {
            return 0;
        }
    }
}

/**
 * Call `clang_getUnqualifiedType`
 *
 * This function does not exist in versions before Clang 16.  This function acts
 * as a no-op in that case, and `result` should not be used.
 *
 * Calling this function with an invalid CT results in a segfault.
 */
static inline void wrap_getUnqualifiedType(const CXType* CT, CXType* result) {
    #if CINDEX_VERSION_MINOR >= 63
        *result = clang_getUnqualifiedType(*CT);
    #endif
}

static inline enum CX_StorageClass wrap_Cursor_getStorageClass(const CXCursor* C) {
    return clang_Cursor_getStorageClass(*C);
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

static inline int wrap_Range_isNull(const CXSourceRange* range) {
    return clang_Range_isNull(*range);
}

static inline void wrap_getExpansionLocation(const CXSourceLocation* location, CXFile* file, unsigned* line, unsigned* column, unsigned* offset) {
    clang_getExpansionLocation(*location, file, line, column, offset);
}

static inline void wrap_getPresumedLocation(const CXSourceLocation* location, CXString* filename, unsigned* line, unsigned* column) {
    clang_getPresumedLocation(*location, filename, line, column);
}

static inline void wrap_getSpellingLocation(const CXSourceLocation* location, CXFile* file, unsigned* line, unsigned* column, unsigned* offset) {
    clang_getSpellingLocation(*location, file, line, column, offset);
}

static inline void wrap_getFileLocation(const CXSourceLocation* location, CXFile* file, unsigned* line, unsigned* column, unsigned* offset) {
    clang_getFileLocation(*location, file, line, column, offset);
}

static inline void wrap_getLocation(CXTranslationUnit tu, CXFile file, unsigned line, unsigned column, CXSourceLocation* result) {
    *result = clang_getLocation(tu, file, line, column);
}

static inline void wrap_getRange(const CXSourceLocation* begin, const CXSourceLocation* end, CXSourceRange* result) {
    *result = clang_getRange(*begin, *end);
}

static inline int wrap_Location_isFromMainFile(const CXSourceLocation* location) {
    return clang_Location_isFromMainFile(*location);
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

/**
 * Miscellaneous utility functions
 */

static inline void wrap_getClangVersion(CXString* result) {
    *result = clang_getClangVersion();
}

/**
 * Debugging
 */

void clang_breakpoint(void);

#endif
