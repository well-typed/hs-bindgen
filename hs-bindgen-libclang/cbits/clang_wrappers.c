#include <stdlib.h>

#include "clang_wrappers.h"

/**
 * Traversing the AST with cursors
 *
 * We need a function pointer to `wrap_visitor`, so this cannot be defined as
 * `static inline` in the header file.
 */

enum CXChildVisitResult wrap_visitor(CXCursor cursor, CXCursor parent, CXClientData client_data) {
    WrapCXCursorVisitor visitor = client_data;
    return visitor(&cursor, &parent);
}
