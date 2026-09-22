// A macro body may be empty: `#define FOO` defines FOO as the empty token
// sequence (C23 6.10.5), the form every include guard uses. See issue 2246.
//
// Such a macro only reaches the macro language with `--parse-empty-macros`;
// it is then up to the language whether it can be translated: the `CExpr`
// language has no expression to translate and declines these macros, while
// `Raw` reproduces them verbatim.

#define EMPTY_OBJECT
#define EMPTY_FUNCTION()
#define EMPTY_FUNCTION_PARAMS(x, y)
#define EMPTY_FUNCTION_VARIADIC(x, ...)
