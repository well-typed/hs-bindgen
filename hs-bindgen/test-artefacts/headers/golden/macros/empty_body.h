// A macro body may be empty: `#define FOO` defines FOO as the empty token
// sequence (C23 6.10.5), the form every include guard uses. See issue 2246.
//
// Whether an empty body can be translated is up to the macro language: the
// `CExpr` language has no expression to translate and drops these macros, while
// `Raw` reproduces them verbatim.

#define EMPTY_OBJECT
#define EMPTY_FUNCTION()
#define EMPTY_FUNCTION_PARAMS(x, y)
#define EMPTY_FUNCTION_VARIADIC(x, ...)
