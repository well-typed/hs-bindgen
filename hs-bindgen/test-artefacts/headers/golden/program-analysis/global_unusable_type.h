// A global whose type is an unusable unnamed struct: long double is not
// supported. The global is unusable because its type is.
struct { long double field1; } badGlobal;
