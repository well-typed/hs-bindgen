#ifndef HS_VISIT_H
#define HS_VISIT_H

#include <clang-c/Index.h>

/* The Haskell-facing visitor. It receives the two cursors as pointers, not by
 * value, because GHC cannot build a FunPtr for a function that takes a struct by
 * value. The pointers are void* so the Haskell side can use the runtime's
 * generated ToFunPtr (Ptr Void -> Ptr Void -> IO CInt); the result is a
 * CXChildVisitResult returned as a plain int. */
typedef int (*HsCursorVisitor)(void *cursor, void *parent);

/* clang_visitChildren takes the parent cursor by value, which the Haskell FFI
 * cannot express; this shim takes it by pointer and dereferences, and passes the
 * Haskell visitor through client_data. */
unsigned hs_visitChildren(const CXCursor *parent, HsCursorVisitor visitor);

#endif
