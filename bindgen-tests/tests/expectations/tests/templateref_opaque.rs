#![allow(dead_code, non_snake_case, non_camel_case_types, non_upper_case_globals)]
#[repr(C)]
#[derive(Debug, Default, Copy, Clone, Hash, PartialEq, Eq)]
#[bindgen_original_name("PointerType")]
pub struct detail_PointerType {
    pub _address: u8,
}
#[bindgen_original_name("PointerType::Type")]
pub type detail_PointerType_Type<T> = *mut T;
#[repr(C)]
#[derive(Debug, Default, Copy, Clone, Hash, PartialEq, Eq)]
pub struct UniquePtr {
    pub _address: u8,
}
#[bindgen_unused_template_param]
#[bindgen_original_name("UniquePtr::Pointer")]
pub type UniquePtr_Pointer = detail_PointerType;
