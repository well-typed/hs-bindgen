#include "macro_fallback_inner.h"

/* The macro language handles this macro; no fallback */
#define HANDLED 42

/**
 * @brief Fails to parse
 *
 * A fallback binding keeps its Doxygen comment.
 */
#define PARSE_FAILURE __attribute__((unused))

/* Fails to resolve: refers to an undeclared name */
#define RESOLVE_FAILURE (undeclared + 1)

/* Fails to typecheck */
#define TYPECHECK_FAILURE (HANDLED + "x")

/* A type expression over a value macro: fails to typecheck */
#define TYPE_LIKE const HANDLED

/* Cascade: refers to a macro that falls back */
#define CASCADE (PARSE_FAILURE + 1)

/* Resolves to a typedef outside the selection, then fails to typecheck. With
 * program slicing, the typedef must not be selected: a fallback macro has no
 * dependencies. */
#define OUTSIDE_DEP (outside_t + 1)

/* Function-like, variadic and GNU named-variadic */
#define CONCAT(x, y) x ## y
#define LOG(fmt, ...) printf(fmt, __VA_ARGS__)
#define LOG_NAMED(fmt, args...) printf(fmt, args)

/* Empty replacement list: reaches the macro language under
 * --parse-empty-macros only */
#define EMPTY
