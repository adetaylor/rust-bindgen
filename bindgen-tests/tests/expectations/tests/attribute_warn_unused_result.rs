#![allow(dead_code, non_snake_case, non_camel_case_types, non_upper_case_globals)]
#[repr(C)]
#[derive(Debug, Default, Copy, Clone)]
pub struct Foo {
    pub _address: u8,
}
#[test]
fn bindgen_test_layout_Foo() {
    assert_eq!(::std::mem::size_of::<Foo>(), 1usize, "Size of Foo");
    assert_eq!(::std::mem::align_of::<Foo>(), 1usize, "Alignment of Foo");
}
extern "C" {
    #[must_use]
    #[bindgen_original_name("foo")]
    #[link_name = "\u{1}_ZN3Foo3fooEi"]
    pub fn Foo_foo(this: *mut Foo, arg1: ::std::os::raw::c_int) -> ::std::os::raw::c_int;
}
impl Foo {
    #[inline]
    #[must_use]
    pub unsafe fn foo(&mut self, arg1: ::std::os::raw::c_int) -> ::std::os::raw::c_int {
        Foo_foo(self, arg1)
    }
}
extern "C" {
    #[must_use]
    #[link_name = "\u{1}_Z3fooi"]
    pub fn foo(arg1: ::std::os::raw::c_int) -> ::std::os::raw::c_int;
}
