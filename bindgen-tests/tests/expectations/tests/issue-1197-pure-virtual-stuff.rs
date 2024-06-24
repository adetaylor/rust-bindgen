#![allow(dead_code, non_snake_case, non_camel_case_types, non_upper_case_globals)]
#[repr(C)]
pub struct Foo__bindgen_vtable(::std::os::raw::c_void);
#[repr(C)]
#[derive(Debug)]
pub struct Foo {
    pub vtable_: *const Foo__bindgen_vtable,
}
const _: () = {
    ["Size of Foo"][::std::mem::size_of::<Foo>() - 8usize];
    ["Alignment of Foo"][::std::mem::align_of::<Foo>() - 8usize];
};
impl Default for Foo {
    fn default() -> Self {
        let mut s = ::std::mem::MaybeUninit::<Self>::uninit();
        unsafe {
            ::std::ptr::write_bytes(s.as_mut_ptr(), 0, 1);
            s.assume_init()
        }
    }
}
extern "C" {
    #[bindgen_pure_virtual]
    #[bindgen_original_name("Bar")]
    #[link_name = "\u{1}_ZN3Foo3BarEv"]
    pub fn Foo_Bar(this: *mut ::std::os::raw::c_void);
}
extern "C" {
    #[bindgen_pure_virtual]
    #[bindgen_original_name("Foo_destructor")]
    #[bindgen_special_member("dtor")]
    #[link_name = "\u{1}_ZN3FooD1Ev"]
    pub fn Foo_Foo_destructor(this: *mut Foo);
}
