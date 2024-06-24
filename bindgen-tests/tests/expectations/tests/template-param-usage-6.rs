#![allow(dead_code, non_snake_case, non_camel_case_types, non_upper_case_globals)]
#[repr(C)]
#[derive(Debug, Default, Copy, Clone)]
pub struct DoesNotUseTemplateParameter {
    pub x: ::std::os::raw::c_int,
}
#[bindgen_original_name("DoesNotUseTemplateParameter::ButAliasDoesUseIt")]
pub type DoesNotUseTemplateParameter_ButAliasDoesUseIt<T> = T;
