#![allow(dead_code, non_snake_case, non_camel_case_types, non_upper_case_globals)]
#[repr(C)]
#[derive(Debug, Default, Copy, Clone)]
#[bindgen_original_name("allocator_traits")]
pub struct std_allocator_traits {
    pub _address: u8,
}
#[bindgen_original_name("allocator_traits::__size_type")]
pub type std_allocator_traits___size_type<_Alloc> = _Alloc;
