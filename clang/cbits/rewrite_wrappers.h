#ifndef REWRITE_WRAPPERS_H
#define REWRITE_WRAPPERS_H

/**
 * Wrappers for the Rewrite API
 */

#include <clang-c/Rewrite.h>

static inline void wrap_CXRewriter_insertTextBefore(CXRewriter Rew, const CXSourceLocation *Loc, const char *Insert) {
    clang_CXRewriter_insertTextBefore(Rew, *Loc, Insert);
}

#endif
