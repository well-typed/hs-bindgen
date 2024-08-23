#ifndef DOXYGEN_WRAPPERS_H
#define DOXYGEN_WRAPPERS_H

/**
 * Wrappers for the Doxygen API
 */

#include <clang-c/Documentation.h>

/**
 * Top-level
 */

CXComment* wrap_malloc_Cursor_getParsedComment(CXCursor* C);
enum CXCommentKind wrap_Comment_getKind(CXComment* Comment);

/**
 * Comment type 'CXComment_FullComment'
 */

CXString* wrap_malloc_FullComment_getAsHTML(CXComment* Comment);
CXString* wrap_malloc_FullComment_getAsXML(CXComment* Comment);

#endif