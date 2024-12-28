#![allow(dead_code, non_snake_case, non_camel_case_types, non_upper_case_globals)]
#[repr(C)]
pub struct nsTArray {
    pub hdr: *const (),
}
unsafe extern "C" {
    #[bindgen_unused_template_param_in_arg_or_return]
    pub fn func() -> *mut nsTArray;
}
