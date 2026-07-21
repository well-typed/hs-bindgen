// See https://github.com/well-typed/hs-bindgen/issues/2147.
//
// A macro naming an undeclared tagged type fails name resolution. Such failures
// are common in real headers (e.g. preprocessor-only builtin operators), so
// they are reported at Info level by default; EnableMacroWarnings raises them
// to Warning.
#define UNRESOLVED_MACRO struct DoesNotExist
