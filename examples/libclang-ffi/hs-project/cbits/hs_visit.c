#include "hs_visit.h"

/* By-value -> pointer trampoline: libclang calls this with the cursors by value,
 * and we hand their addresses to the Haskell visitor (passed through
 * client_data). */
static enum CXChildVisitResult hs_trampoline(CXCursor cursor, CXCursor parent,
                                             CXClientData client_data) {
    HsCursorVisitor visitor = (HsCursorVisitor) client_data;
    return (enum CXChildVisitResult) visitor(&cursor, &parent);
}

unsigned hs_visitChildren(const CXCursor *parent, HsCursorVisitor visitor) {
    return clang_visitChildren(*parent, hs_trampoline, (CXClientData) visitor);
}
