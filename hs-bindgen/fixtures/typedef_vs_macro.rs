/* automatically generated by rust-bindgen 0.70.1 */

pub type T1 = ::std::os::raw::c_int;
pub type T2 = ::std::os::raw::c_char;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct ExampleStruct {
    pub t1: T1,
    pub t2: T2,
    pub m1: ::std::os::raw::c_int,
    pub m2: ::std::os::raw::c_char,
}
#[allow(clippy::unnecessary_operation, clippy::identity_op)]
const _: () = {
    ["Size of ExampleStruct"][::std::mem::size_of::<ExampleStruct>() - 16usize];
    ["Alignment of ExampleStruct"][::std::mem::align_of::<ExampleStruct>() - 4usize];
    [
        "Offset of field: ExampleStruct::t1",
    ][::std::mem::offset_of!(ExampleStruct, t1) - 0usize];
    [
        "Offset of field: ExampleStruct::t2",
    ][::std::mem::offset_of!(ExampleStruct, t2) - 4usize];
    [
        "Offset of field: ExampleStruct::m1",
    ][::std::mem::offset_of!(ExampleStruct, m1) - 8usize];
    [
        "Offset of field: ExampleStruct::m2",
    ][::std::mem::offset_of!(ExampleStruct, m2) - 12usize];
};
