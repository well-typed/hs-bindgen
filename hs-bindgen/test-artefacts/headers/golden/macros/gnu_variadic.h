// A variadic parameter list comes in two forms, and they are different macros:
// in the C99 form the trailing arguments are __VA_ARGS__, in the GNU form the
// name before the ... stands for them. Rendering GNU_VARIADIC as
// `#define GNU_VARIADIC(fmt, ...) args` would pass only fmt on.
//
// `c-expr` parses neither form, so only the `Raw` macro language produces
// bindings here.

#define C99_VARIADIC(fmt, ...) fmt

#define GNU_VARIADIC(fmt, args...) args

#define GNU_VARIADIC_ONLY(args...) args
