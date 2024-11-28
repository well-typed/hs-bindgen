/* note: we cannot also have struct foo, clang and gcc say:

 - use of 'foo' with tag type that does not match previous declaration
 - ‘foo’ defined as wrong kind of tag

 respectively.

struct foo {
	int x;
	int y;
};
*/

enum foo {
	FOO1,
	FOO2
};

typedef double foo;

/* we use enum foo as an example, as here Rust bindgen defines the same type twice:

pub type foo = ::std::os::raw::c_uint;
pub type foo = f64;

*/
