#ifndef DOXYGEN_WRAPPERS_H
#define DOXYGEN_WRAPPERS_H

/**
 * Wrappers for the Doxygen API
 */

#include <clang-c/Documentation.h>

/**
 * Top-level
 */

static inline void wrap_Cursor_getParsedComment(const CXCursor* C, CXComment* result) {
    *result = clang_Cursor_getParsedComment(*C);
}

static inline enum CXCommentKind wrap_Comment_getKind(const CXComment* Comment) {
    return clang_Comment_getKind(*Comment);
}

/**
 * Comment type 'CXComment_InlineCommand'
 */

static inline void wrap_InlineCommandComment_getCommandName(const CXComment* Comment, CXString*  result) {
    *result = clang_InlineCommandComment_getCommandName(*Comment);
}

static inline enum CXCommentInlineCommandRenderKind wrap_InlineCommandComment_getRenderKind(const CXComment* Comment) {
    return clang_InlineCommandComment_getRenderKind(*Comment);
}

static inline unsigned wrap_InlineCommandComment_getNumArgs(const CXComment* Comment) {
    return clang_InlineCommandComment_getNumArgs(*Comment);
}

static inline void wrap_InlineCommandComment_getArgText(const CXComment* Comment, unsigned argIdx, CXString* result) {
    *result = clang_InlineCommandComment_getArgText(*Comment, argIdx);
}

/**
 * Comment type 'CXComment_FullComment'
 */

static inline void wrap_FullComment_getAsHTML(const CXComment* Comment, CXString*  result) {
    *result = clang_FullComment_getAsHTML(*Comment);
}

static inline void wrap_FullComment_getAsXML(const CXComment* Comment, CXString*  result) {
    *result = clang_FullComment_getAsXML(*Comment);
}

#endif