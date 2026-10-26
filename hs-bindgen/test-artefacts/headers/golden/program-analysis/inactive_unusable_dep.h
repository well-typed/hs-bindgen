// A selected inactive declaration reports its own reason only, not its
// unusable dependencies, and is not reported as selected

// Unusable: long double is not supported.
struct unsupported { long double x; };

// Unusable, and deprecated.
struct __attribute__ ((deprecated)) deprecated_unsupported { long double x; };

// Both declarations mangle to @Clash@, so neither is usable.
struct clash { struct unsupported *p; };
typedef int clash;

// Omitted by the prescriptive binding specification, so its user fails to
// mangle; program slicing reaches it only through that user.
struct omitted { struct unsupported *p; };
struct uses_omitted { struct omitted *p; };
