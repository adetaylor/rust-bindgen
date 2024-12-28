//! Helpers for code generation that don't need macro expansion.

use proc_macro2::{Ident, Span, TokenStream};

use crate::ir::comp::SpecialMemberKind;
use crate::ir::function::Visibility;
use crate::ir::context::BindgenContext;
use crate::ir::layout::Layout;
use crate::BindgenOptions;

pub(crate) mod attributes {
    use proc_macro2::{Ident, Span, TokenStream};
    use std::{borrow::Cow, str::FromStr};

    pub(crate) fn repr(which: &str) -> TokenStream {
        let which = Ident::new(which, Span::call_site());
        quote! {
            #[repr( #which )]
        }
    }

    pub(crate) fn repr_list(which_ones: &[&str]) -> TokenStream {
        let which_ones = which_ones
            .iter()
            .map(|one| TokenStream::from_str(one).expect("repr to be valid"));
        quote! {
            #[repr( #( #which_ones ),* )]
        }
    }

    pub(crate) fn derives(which_ones: &[&str]) -> TokenStream {
        let which_ones = which_ones
            .iter()
            .map(|one| TokenStream::from_str(one).expect("derive to be valid"));
        quote! {
            #[derive( #( #which_ones ),* )]
        }
    }

    pub(crate) fn inline() -> TokenStream {
        quote! {
            #[inline]
        }
    }

    pub(crate) fn must_use() -> TokenStream {
        quote! {
            #[must_use]
        }
    }

    pub(crate) fn non_exhaustive() -> TokenStream {
        quote! {
            #[non_exhaustive]
        }
    }

    pub(crate) fn doc(comment: String) -> TokenStream {
        if comment.is_empty() {
            quote!()
        } else {
            quote!(#[doc = #comment])
        }
    }

    pub(crate) fn link_name<const MANGLE: bool>(name: &str) -> TokenStream {
        // LLVM mangles the name by default but it's already mangled.
        // Prefixing the name with \u{1} should tell LLVM to not mangle it.
        let name: Cow<'_, str> = if MANGLE {
            name.into()
        } else {
            format!("\u{1}{name}").into()
        };

        quote! {
            #[link_name = #name]
        }
    }
}

pub(crate) trait CppSemanticAttributeCreator {
    fn do_add(&mut self, ts: TokenStream);
    fn is_enabled(&self) -> bool;

    fn add(&mut self, tokens: TokenStream) {
        if self.is_enabled() {
            self.do_add(quote! {
                #[cpp_semantics(#tokens)]
            })
        }
    }

    fn add_ident(&mut self, desc: &str) {
        if self.is_enabled() {
            let id = Ident::new(desc, Span::call_site());
            self.add(quote! { #id })
        }
    }

    fn special_member(&mut self, kind: SpecialMemberKind) {
        let kind_str = match kind {
            SpecialMemberKind::DefaultConstructor => "default_ctor",
            SpecialMemberKind::CopyConstructor => "copy_ctor",
            SpecialMemberKind::MoveConstructor => "move_ctor",
            SpecialMemberKind::Destructor => "dtor",
            SpecialMemberKind::AssignmentOperator => "assignment_operator",
        };
        self.add(quote! {
            special_member(#kind_str)
        })
    }

    fn original_name(&mut self, name: &str) {
        self.add(quote! {
            original_name(#name)
        })
    }

    fn ret_type_reference(&mut self) {
        self.add_ident("ret_type_reference")
    }

    fn ret_type_rvalue_reference(&mut self) {
        self.add_ident("ret_type_rvalue_reference")
    }

    fn arg_type_reference(&mut self, arg_name: &Ident) {
        self.add(quote! {
            arg_type_reference(#arg_name)
        })
    }

    fn field_type_reference(&mut self) {
        self.add_ident("reference")
    }

    fn field_type_rvalue_reference(&mut self) {
        self.add_ident("rvalue_reference")
    }

    fn is_virtual(&mut self) {
        self.add_ident("bindgen_virtual")
    }

    fn arg_type_rvalue_reference(&mut self, arg_name: &Ident) {
        self.add(quote! {
            arg_type_rvalue_reference(#arg_name)
        })
    }

    fn is_pure_virtual(&mut self) {
        self.add_ident("pure_virtual")
    }

    fn visibility(&mut self, visibility: Visibility) {
        match visibility {
            Visibility::Protected => self.add_ident("visibility_protected"),
            Visibility::Private => self.add_ident("visibility_private"),
            _ => {}
        }
    }

    fn incomprehensible_param_in_arg_or_return(&mut self) {
        self.add_ident("incomprehensible_param_in_arg_or_return")
    }

    fn discards_template_param(&mut self) {
        self.add_ident("unused_template_param")
    }

    fn deleted_fn(&mut self) {
        self.add_ident("deleted")
    }

    fn defaulted_fn(&mut self) {
        self.add_ident("defaulted")
    }

    fn layout(&mut self, layout: &Layout) {
        let sz = ast_ty::int_expr(layout.size as i64);
        let align = ast_ty::int_expr(layout.align as i64);
        let packed = if layout.packed {
            quote! { true }
        } else {
            quote! { false }
        };
        self.add(quote! {
            layout(#sz, #align, #packed)
        })
    }

    fn location(&mut self,
        location: Option<&crate::clang::SourceLocation>,
    ) {
        if let Some(location) = location {
            let (file, line, col, byte_offset) = location.location();
            let line = ast_ty::int_expr(line as i64);
            let col = ast_ty::int_expr(col as i64);
            let byte_offset = ast_ty::int_expr(byte_offset as i64);
            let file = file.name();
            if let Some(filename) = file {
                self.add(quote! {
                    source_location(#filename, #line, #col, #byte_offset)
                });
            }
        }
    }
}

pub struct CppSemanticAttributeAdder<'a> {
    enabled: bool,
    attrs: &'a mut Vec<TokenStream>,
}

impl<'a> CppSemanticAttributeAdder<'a> {
    pub(crate) fn new(
        opts: &BindgenOptions,
        attrs: &'a mut Vec<TokenStream>,
    ) -> Self {
        Self {
            enabled: opts.cpp_semantic_attributes,
            attrs,
        }
    }
}

impl<'a> CppSemanticAttributeCreator for CppSemanticAttributeAdder<'a> {
    fn do_add(&mut self, ts: TokenStream) {
        self.attrs.push(ts)
    }

    fn is_enabled(&self) -> bool {
        self.enabled
    }
}

pub struct CppSemanticAttributeSingle {
    enabled: bool,
    attr: TokenStream,
}

impl CppSemanticAttributeSingle {
    pub(crate) fn new(opts: &BindgenOptions) -> Self {
        Self {
            enabled: opts.cpp_semantic_attributes,
            attr: quote! {},
        }
    }

    pub(crate) fn result(self) -> TokenStream {
        self.attr
    }
}

impl CppSemanticAttributeCreator for CppSemanticAttributeSingle {
    fn do_add(&mut self, ts: TokenStream) {
        self.attr.extend(ts);
    }

    fn is_enabled(&self) -> bool {
        self.enabled
    }
}

/// The `ffi_safe` argument should be true if this is a type that the user might
/// reasonably use, e.g. not struct padding, where the `__BindgenOpaqueArray` is
/// just noise.
/// TODO: Should this be `MaybeUninit`, since padding bytes are effectively
/// uninitialized?
pub(crate) fn blob(
    ctx: &BindgenContext,
    layout: Layout,
    ffi_safe: bool,
) -> syn::Type {
    let opaque = layout.opaque();

    // FIXME(emilio, #412): We fall back to byte alignment, but there are
    // some things that legitimately are more than 8-byte aligned.
    //
    // Eventually we should be able to `unwrap` here, but...
    let ty = opaque.known_rust_type_for_array().unwrap_or_else(|| {
        warn!("Found unknown alignment on code generation!");
        syn::parse_quote! { u8 }
    });

    let data_len = opaque.array_size().unwrap_or(layout.size);

    if data_len == 1 {
        ty
    } else if ffi_safe && ctx.options().rust_features().min_const_generics {
        ctx.generated_opaque_array();
        if ctx.options().enable_cxx_namespaces {
            syn::parse_quote! { root::__BindgenOpaqueArray<#ty, #data_len> }
        } else {
            syn::parse_quote! { __BindgenOpaqueArray<#ty, #data_len> }
        }
    } else {
        // This is not FFI safe as an argument; the struct above is
        // preferable.
        syn::parse_quote! { [ #ty ; #data_len ] }
    }
}

/// Integer type of the same size as the given `Layout`.
pub(crate) fn integer_type(layout: Layout) -> Option<syn::Type> {
    Layout::known_type_for_size(layout.size)
}

pub(crate) const BITFIELD_UNIT: &str = "__BindgenBitfieldUnit";

/// Generates a bitfield allocation unit type for a type with the given `Layout`.
pub(crate) fn bitfield_unit(ctx: &BindgenContext, layout: Layout) -> syn::Type {
    let size = layout.size;
    let bitfield_unit_name = Ident::new(BITFIELD_UNIT, Span::call_site());
    let ty = syn::parse_quote! { #bitfield_unit_name<[u8; #size]> };

    if ctx.options().enable_cxx_namespaces {
        return syn::parse_quote! { root::#ty };
    }

    ty
}

pub(crate) mod ast_ty {
    use crate::ir::context::BindgenContext;
    use crate::ir::function::FunctionSig;
    use crate::ir::layout::Layout;
    use crate::ir::ty::{FloatKind, IntKind};
    use crate::RustTarget;
    use proc_macro2::TokenStream;
    use std::str::FromStr;

    pub(crate) fn c_void(ctx: &BindgenContext) -> syn::Type {
        // ctypes_prefix takes precedence
        match ctx.options().ctypes_prefix {
            Some(ref prefix) => {
                let prefix = TokenStream::from_str(prefix.as_str()).unwrap();
                syn::parse_quote! { #prefix::c_void }
            }
            None => {
                if ctx.options().use_core {
                    syn::parse_quote! { ::core::ffi::c_void }
                } else {
                    syn::parse_quote! { ::std::os::raw::c_void }
                }
            }
        }
    }

    pub(crate) fn raw_type(ctx: &BindgenContext, name: &str) -> syn::Type {
        let ident = ctx.rust_ident_raw(name);
        match ctx.options().ctypes_prefix {
            Some(ref prefix) => {
                let prefix = TokenStream::from_str(prefix.as_str()).unwrap();
                syn::parse_quote! { #prefix::#ident }
            }
            None => {
                if ctx.options().use_core &&
                    ctx.options().rust_features().core_ffi_c
                {
                    syn::parse_quote! { ::core::ffi::#ident }
                } else {
                    syn::parse_quote! { ::std::os::raw::#ident }
                }
            }
        }
    }

    pub(crate) fn int_kind_rust_type(
        ctx: &BindgenContext,
        ik: IntKind,
        layout: Option<Layout>,
    ) -> syn::Type {
        match ik {
            IntKind::Bool => syn::parse_quote! { bool },
            IntKind::Char { .. } => raw_type(ctx, "c_char"),
            IntKind::Char16 => raw_type(ctx, "c_char16_t"),
            IntKind::SChar => raw_type(ctx, "c_schar"),
            IntKind::UChar => raw_type(ctx, "c_uchar"),
            IntKind::Short => raw_type(ctx, "c_short"),
            IntKind::UShort => raw_type(ctx, "c_ushort"),
            IntKind::Int => raw_type(ctx, "c_int"),
            IntKind::UInt => raw_type(ctx, "c_uint"),
            IntKind::Long => raw_type(ctx, "c_long"),
            IntKind::ULong => raw_type(ctx, "c_ulong"),
            IntKind::LongLong => raw_type(ctx, "c_longlong"),
            IntKind::ULongLong => raw_type(ctx, "c_ulonglong"),
            IntKind::WChar => {
                let layout =
                    layout.expect("Couldn't compute wchar_t's layout?");
                Layout::known_type_for_size(layout.size)
                    .expect("Non-representable wchar_t?")
            }

            IntKind::I8 => syn::parse_quote! { i8 },
            IntKind::U8 => syn::parse_quote! { u8 },
            IntKind::I16 => syn::parse_quote! { i16 },
            IntKind::U16 => syn::parse_quote! { u16 },
            IntKind::I32 => syn::parse_quote! { i32 },
            IntKind::U32 => syn::parse_quote! { u32 },
            IntKind::I64 => syn::parse_quote! { i64 },
            IntKind::U64 => syn::parse_quote! { u64 },
            IntKind::Custom { name, .. } => {
                syn::parse_str(name).expect("Invalid integer type.")
            }
            IntKind::U128 => {
                if true {
                    syn::parse_quote! { u128 }
                } else {
                    // Best effort thing, but wrong alignment
                    // unfortunately.
                    syn::parse_quote! { [u64; 2] }
                }
            }
            IntKind::I128 => {
                if true {
                    syn::parse_quote! { i128 }
                } else {
                    syn::parse_quote! { [u64; 2] }
                }
            }
        }
    }

    pub(crate) fn float_kind_rust_type(
        ctx: &BindgenContext,
        fk: FloatKind,
        layout: Option<Layout>,
    ) -> syn::Type {
        // TODO: we probably should take the type layout into account more
        // often?
        //
        // Also, maybe this one shouldn't be the default?
        match (fk, ctx.options().convert_floats) {
            (FloatKind::Float16, _) => {
                // TODO: do f16 when rust lands it
                ctx.generated_bindgen_float16();
                if ctx.options().enable_cxx_namespaces {
                    syn::parse_quote! { root::__BindgenFloat16 }
                } else {
                    syn::parse_quote! { __BindgenFloat16 }
                }
            }
            (FloatKind::Float, true) => syn::parse_quote! { f32 },
            (FloatKind::Double, true) => syn::parse_quote! { f64 },
            (FloatKind::Float, false) => raw_type(ctx, "c_float"),
            (FloatKind::Double, false) => raw_type(ctx, "c_double"),
            (FloatKind::LongDouble, _) => {
                if let Some(layout) = layout {
                    match layout.size {
                        4 => syn::parse_quote! { f32 },
                        8 => syn::parse_quote! { f64 },
                        // TODO(emilio): If rust ever gains f128 we should
                        // use it here and below.
                        _ => super::integer_type(layout)
                            .unwrap_or(syn::parse_quote! { f64 }),
                    }
                } else {
                    debug_assert!(
                        false,
                        "How didn't we know the layout for a primitive type?"
                    );
                    syn::parse_quote! { f64 }
                }
            }
            (FloatKind::Float128, _) => {
                if true {
                    syn::parse_quote! { u128 }
                } else {
                    syn::parse_quote! { [u64; 2] }
                }
            }
        }
    }

    pub(crate) fn int_expr(val: i64) -> TokenStream {
        // Don't use quote! { #val } because that adds the type suffix.
        let val = proc_macro2::Literal::i64_unsuffixed(val);
        quote!(#val)
    }

    pub(crate) fn uint_expr(val: u64) -> TokenStream {
        // Don't use quote! { #val } because that adds the type suffix.
        let val = proc_macro2::Literal::u64_unsuffixed(val);
        quote!(#val)
    }

    pub(crate) fn cstr_expr(mut string: String) -> TokenStream {
        string.push('\0');
        let b = proc_macro2::Literal::byte_string(string.as_bytes());
        quote! {
            #b
        }
    }

    pub(crate) fn float_expr(
        ctx: &BindgenContext,
        f: f64,
    ) -> Result<TokenStream, ()> {
        if f.is_finite() {
            let val = proc_macro2::Literal::f64_unsuffixed(f);

            return Ok(quote!(#val));
        }

        let prefix = ctx.trait_prefix();
        let rust_target = ctx.options().rust_target;

        if f.is_nan() {
            // FIXME: This should be done behind a `RustFeature` instead
            #[allow(deprecated)]
            let tokens = if rust_target >= RustTarget::Stable_1_43 {
                quote! {
                    f64::NAN
                }
            } else {
                quote! {
                    ::#prefix::f64::NAN
                }
            };
            return Ok(tokens);
        }

        if f.is_infinite() {
            let tokens = if f.is_sign_positive() {
                // FIXME: This should be done behind a `RustFeature` instead
                #[allow(deprecated)]
                if rust_target >= RustTarget::Stable_1_43 {
                    quote! {
                        f64::INFINITY
                    }
                } else {
                    quote! {
                        ::#prefix::f64::INFINITY
                    }
                }
            } else {
                // FIXME: This should be done behind a `RustFeature` instead
                #[allow(deprecated)]
                // Negative infinity
                if rust_target >= RustTarget::Stable_1_43 {
                    quote! {
                        f64::NEG_INFINITY
                    }
                } else {
                    quote! {
                        ::#prefix::f64::NEG_INFINITY
                    }
                }
            };
            return Ok(tokens);
        }

        warn!("Unknown non-finite float number: {f:?}");
        Err(())
    }

    pub(crate) fn arguments_from_signature(
        signature: &FunctionSig,
        ctx: &BindgenContext,
    ) -> Vec<TokenStream> {
        let mut unnamed_arguments = 0;
        signature
            .argument_types()
            .iter()
            .map(|&(ref name, _ty)| {
                let name = if let Some(ref name) = *name {
                    ctx.rust_ident(name)
                } else {
                    unnamed_arguments += 1;
                    ctx.rust_ident(format!("arg{unnamed_arguments}"))
                };
                quote! { #name }
            })
            .collect()
    }
}
