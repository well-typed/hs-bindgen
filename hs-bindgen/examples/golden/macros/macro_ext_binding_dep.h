// A typedef that is replaced by an external binding spec
typedef int A;

// A type-macro whose body references A. Its var slot should resolve to
// Left ext after ResolveBindingSpecs runs.
#define B A

void use_b(B x);
