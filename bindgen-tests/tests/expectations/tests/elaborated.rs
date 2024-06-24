#[bindgen_original_name("whatever_t")]
pub type whatever_whatever_t = ::std::os::raw::c_int;
extern "C" {
    #[link_name = "\u{1}_Z9somethingPKi"]
    pub fn something(wat: *const whatever_whatever_t);
}
