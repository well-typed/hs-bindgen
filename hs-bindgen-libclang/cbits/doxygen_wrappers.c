#include <stdlib.h>

#include "doxygen_wrappers.h"

/**
 * Top-level
 */

CXComment* wrap_malloc_Cursor_getParsedComment(CXCursor* C) {
    CXComment* result = malloc(sizeof(CXComment));
    *result = clang_Cursor_getParsedComment(*C);
    return result;
}

enum CXCommentKind wrap_Comment_getKind(CXComment* Comment) {
    return clang_Comment_getKind(*Comment);
}

/**
 * Comment type 'CXComment_FullComment'
 */

CXString* wrap_malloc_FullComment_getAsHTML(CXComment* Comment) {
    CXString* result = malloc(sizeof(CXString));
    *result = clang_FullComment_getAsHTML(*Comment);
    return result;
}

CXString* wrap_malloc_FullComment_getAsXML(CXComment* Comment) {
    CXString* result = malloc(sizeof(CXString));
    *result = clang_FullComment_getAsXML(*Comment);
    return result;
}

