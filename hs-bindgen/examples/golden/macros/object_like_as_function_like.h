// F is a function-like macro definition while G is an object-like macro
// definition. The difference is in the whitespace between the identifier and
// the opening parenthesis. hs-bindgen treats F and G both as function-like
// macro definitions, which is wrong.
//
// TODO <https://github.com/well-typed/hs-bindgen/issues/1903>
//
// <https://en.cppreference.com/w/cpp/preprocessor/replace.html>

#define F(x, y) x + y
#define G (x, y) x + y

int a = F(3, 5);
// int b = G(4, 2); // does not work!