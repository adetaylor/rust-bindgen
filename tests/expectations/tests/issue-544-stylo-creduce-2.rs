#![allow(
    dead_code,
    non_snake_case,
    non_camel_case_types,
    non_upper_case_globals
)]

#[repr(C)]
pub struct Foo {
    pub member: Foo_SecondAlias,
}
#[bindgen_original_name("Foo::FirstAlias")]
pub type Foo_FirstAlias = [u8; 0usize];
#[bindgen_original_name("Foo::SecondAlias")]
pub type Foo_SecondAlias = [u8; 0usize];
impl Default for Foo {
    fn default() -> Self {
        let mut s = ::std::mem::MaybeUninit::<Self>::uninit();
        unsafe {
            ::std::ptr::write_bytes(s.as_mut_ptr(), 0, 1);
            s.assume_init()
        }
    }
}
