#![allow(
    dead_code,
    non_snake_case,
    non_camel_case_types,
    non_upper_case_globals
)]

#[repr(C)]
#[derive(Debug, Default, Copy, Clone)]
#[bindgen_original_name("RootedBase")]
pub struct js_RootedBase {
    pub _address: u8,
}
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct Rooted {
    pub _address: u8,
}
impl Default for Rooted {
    fn default() -> Self {
        unsafe { ::std::mem::zeroed() }
    }
}
