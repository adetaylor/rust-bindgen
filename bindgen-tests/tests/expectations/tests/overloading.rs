#![allow(dead_code, non_snake_case, non_camel_case_types, non_upper_case_globals)]
extern "C" {
    #[link_name = "\u{1}_Z8Evaluatec"]
    pub fn Evaluate(r: ::std::os::raw::c_char) -> bool;
}
extern "C" {
    #[bindgen_original_name("Evaluate")]
    #[link_name = "\u{1}_Z8Evaluateii"]
    pub fn Evaluate1(x: ::std::os::raw::c_int, y: ::std::os::raw::c_int) -> bool;
}
extern "C" {
    #[bindgen_original_name("MyFunction")]
    #[link_name = "\u{1}_ZN3foo10MyFunctionEv"]
    pub fn foo_MyFunction();
}
extern "C" {
    #[bindgen_original_name("MyFunction")]
    #[link_name = "\u{1}_ZN3bar10MyFunctionEv"]
    pub fn bar_MyFunction();
}
