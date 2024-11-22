/* automatically generated by rust-bindgen 0.70.1 */

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct foo {
    pub i: ::std::os::raw::c_int,
    pub c: ::std::os::raw::c_char,
}
#[allow(clippy::unnecessary_operation, clippy::identity_op)]
const _: () = {
    ["Size of foo"][::std::mem::size_of::<foo>() - 8usize];
    ["Alignment of foo"][::std::mem::align_of::<foo>() - 4usize];
    ["Offset of field: foo::i"][::std::mem::offset_of!(foo, i) - 0usize];
    ["Offset of field: foo::c"][::std::mem::offset_of!(foo, c) - 4usize];
};
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct bar {
    pub foo1: foo,
    pub foo2: foo,
}
#[allow(clippy::unnecessary_operation, clippy::identity_op)]
const _: () = {
    ["Size of bar"][::std::mem::size_of::<bar>() - 16usize];
    ["Alignment of bar"][::std::mem::align_of::<bar>() - 4usize];
    ["Offset of field: bar::foo1"][::std::mem::offset_of!(bar, foo1) - 0usize];
    ["Offset of field: bar::foo2"][::std::mem::offset_of!(bar, foo2) - 8usize];
};
