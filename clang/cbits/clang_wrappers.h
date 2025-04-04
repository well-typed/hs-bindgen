#ifndef CLANG_WRAPPERS_H
#define CLANG_WRAPPERS_H

#include <clang-c/Index.h>
#include <stdio.h>
#include "clang_wrappers_ffi.h"

/**
 * Versioning
 */

/**
 * Clang version
 *
 * See `getClangVersion` for discussion.
 */
enum ClangVersion {
  ClangOlderThan3_2   =   0,      /**< `clang  <  3.2`           */
  Clang3              =  32,      /**< `clang >=  3.2 && <  4.0` */
  Clang4              =  40,      /**< `clang >=  4.0 && <  5.0` */
  Clang5              =  50,      /**< `clang >=  5.0 && <  6.0` */
  Clang6              =  60,      /**< `clang >=  6.0 && <  7.0` */
  Clang7              =  70,      /**< `clang >=  7.0 && <  8.0` */
  Clang8              =  80,      /**< `clang >=  8.0 && <  9.0` */
  Clang9_or_10        =  90,      /**< `clang >=  9.0 && < 11.0` */
  Clang11             = 110,      /**< `clang >= 11.0 && < 11.1` */
  Clang11_or_12       = 111,      /**< `clang >= 11.1 && < 13.0` */
  Clang13_or_14_or_15 = 130,      /**< `clang >= 13.0 && < 16.0` */
  Clang16             = 160,      /**< `clang >= 16.0 && < 17.0` */
  Clang17_or_18_or_19 = 170,      /**< `clang >= 17.0 && < 20.0` */
  ClangNewerThan19    = 1000000,  /**< `clang >= 20.0`           */
};

/**
 * Get `clang` version
 *
 * This is not a standard `libclang` function; `libclang` offers only
 * `clang_getClangVersion`, but this is described as
 *
 * ```
 * Return a version string, suitable for showing to a user, but not intended
 * to be parsed (the format is not guaranteed to be stable).
 * ```
 *
 * For this reason we provide `getClangVersion`, which _is_ intended to be
 * stable. We base this on `CINDEX_VERSION_MINOR` (and `CINDEX_VERSION_MAJOR`,
 * which is always expected to be `0`). Unfortunately the mapping from
 * `CINDEX_VERSION_MINOR` to `clang` version is not one-to-one:
 *
 * - Some `clang` versions share the same `CINDEX_VERSION_MINOR` version;
 *   e.g. `clang-9` and `clang-10` both have a `CINDEX_VERSION_MINOR` of `59`.
 * - Some `CINDEX_VERSION_MINOR` numbers don't correspond to any (official)
 *   `clang` release; for example, the last 8.x release is 8.0.1, with a
 *   `CINDEX_VERSION_MINOR` of `50` , and the first 9.x release, 9.0.0, has
 *   `CINDEX_VERSION_MINOR` set to `59`.
 *
 * On the assumption that `getClangVersion` is called to check if the clang
 * version is _at least_ some minimum bound, we take the conservative approach
 * in both these cases, returning `Clang9_or_10` also for `clang-10`, and
 * returning `Clang8` for a `CINDEX_VERSION_MINOR` of `51..58`.
 *
 * Returns `-1` if `CINDEX_VERSION_MINOR` or `CINDEX_VERSION_MAJOR` are not
 * defined, or if `CINDEX_VERSION_MAJOR` is not equal to zero.
 */
static inline enum ClangVersion getClangVersion(void) {
    // Implementation note: this mapping comes from git tags versus the
    // values in `c-lang-c/Index.h`; we ignore tags of the shape `..-init`,
    // as they still use the values from the _previous_ version.

    #if !defined(CINDEX_VERSION_MINOR) || !defined(CINDEX_VERSION_MAJOR)
    return (-1);
    #endif

    if (CINDEX_VERSION_MAJOR > 0)
        return (-1);
    else if (CINDEX_VERSION_MINOR < 6)
        return ClangOlderThan3_2;
    else if (CINDEX_VERSION_MINOR >=  6 && CINDEX_VERSION_MINOR < 37)
        return Clang3;
    else if (CINDEX_VERSION_MINOR >= 37 && CINDEX_VERSION_MINOR < 43)
        return Clang4;
    else if (CINDEX_VERSION_MINOR >= 43 && CINDEX_VERSION_MINOR < 45)
        return Clang5;
    else if (CINDEX_VERSION_MINOR >= 45 && CINDEX_VERSION_MINOR < 49)
        return Clang6;
    else if (CINDEX_VERSION_MINOR == 49)
        return Clang7;
    else if (CINDEX_VERSION_MINOR >= 50 && CINDEX_VERSION_MINOR < 59)
        return Clang8;
    else if (CINDEX_VERSION_MINOR == 59)
        return Clang9_or_10;
    else if (CINDEX_VERSION_MINOR == 60)
        return Clang11;
    else if (CINDEX_VERSION_MINOR == 61)
        return Clang11_or_12;
    else if (CINDEX_VERSION_MINOR == 62)
        return Clang13_or_14_or_15;
    else if (CINDEX_VERSION_MINOR == 63)
        return Clang16;
    else if (CINDEX_VERSION_MINOR == 64)
        return Clang17_or_18_or_19;
    else
        return ClangNewerThan19;
}

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
 * Debugging
 */

void clang_breakpoint(void);

#endif
