#![allow(dead_code, non_snake_case, non_camel_case_types, non_upper_case_globals)]
pub type foo = ::std::option::Option<unsafe extern "C" fn(arg1: ::std::os::raw::c_int)>;
#[repr(C)]
#[derive(Debug, Default, Copy, Clone)]
pub struct Foo {
    pub _address: u8,
}
#[bindgen_original_name("Foo::Char")]
pub type Foo_Char<T> = T;
#[bindgen_original_name("Foo::FooPtrTypedef")]
pub type Foo_FooPtrTypedef<T> = *mut Foo_Char<T>;
#[bindgen_original_name("Foo::nsCOMArrayEnumFunc")]
pub type Foo_nsCOMArrayEnumFunc<T> = ::std::option::Option<
    unsafe extern "C" fn(aElement: *mut T, aData: *mut ::std::os::raw::c_void) -> bool,
>;
