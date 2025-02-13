#![allow(dead_code, non_snake_case, non_camel_case_types, non_upper_case_globals)]
#[repr(C)]
pub struct nsSlots__bindgen_vtable(::std::os::raw::c_void);
#[repr(C)]
#[derive(Debug)]
pub struct nsSlots {
    pub vtable_: *const nsSlots__bindgen_vtable,
}
#[allow(clippy::unnecessary_operation, clippy::identity_op)]
const _: () = {
    ["Size of nsSlots"][::std::mem::size_of::<nsSlots>() - 8usize];
    ["Alignment of nsSlots"][::std::mem::align_of::<nsSlots>() - 8usize];
};
impl Default for nsSlots {
    fn default() -> Self {
        let mut s = ::std::mem::MaybeUninit::<Self>::uninit();
        unsafe {
            ::std::ptr::write_bytes(s.as_mut_ptr(), 0, 1);
            s.assume_init()
        }
    }
}
unsafe extern "C" {
    #[bindgen_original_name("nsSlots_destructor")]
    #[bindgen_special_member("dtor")]
    #[link_name = "\u{1}_ZN7nsSlotsD1Ev"]
    pub fn nsSlots_nsSlots_destructor(this: *mut nsSlots);
}
