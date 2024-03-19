use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::Comma;
use syn::Expr;
use syn::Expr::Lit;
use syn::Field;
use syn::Meta::List;
use syn::Type::Path;
use syn::{parse_macro_input, Data::Struct, DeriveInput};
#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let st_name = input.ident;
    let data = input.data;
    let fields = match data {
        Struct(value) => {
            let fields = value.fields;
            match fields {
                syn::Fields::Named(value) => value.named,
                _ => panic!("not common fields"),
            }
        }
        _ => panic!("not DataStruct"),
    };
    let field_name = fields.clone().into_iter().map(|field| field.ident);
    let field_name_2 = field_name.clone();
    let setters = call_setters(fields.clone());
    let build_fn_field = build_fn_field(fields.clone());
    let builder_fields = builder_fields(fields.clone());
    let repeated_field = repeated_field(fields.clone());
    let expanded = quote!(
        #[derive(Debug)]
        pub struct CommandBuilder {
            #(#builder_fields),*
        }
        impl #st_name {
            pub fn builder() -> CommandBuilder {
                CommandBuilder {
                    #(#field_name_2: None),*
                }
            }

        }
        impl CommandBuilder {
            #(#setters)*
            #repeated_field
        }
        impl CommandBuilder {
            pub fn build(&mut self) -> std::result::Result<#st_name, std::boxed::Box<dyn std::error::Error>>{
                let rt = #st_name{
                    #(#build_fn_field),*
                };
                Ok(rt)
            }
        }
    );
    TokenStream::from(expanded)
}
fn call_setters(
    fields: Punctuated<Field, Comma>,
) -> impl Iterator<Item = proc_macro2::TokenStream> {
    fields.clone().into_iter().map(|field| {
        let fn_name = field.ident;
        let field_ty = field.ty;
        match field_ty.clone() {
            Path(path) => {
                if let Some(segment) = path.path.segments.last() {
                    if segment.ident == "Option" {
                        if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                            if let Some(syn::GenericArgument::Type(inner_type)) = args.args.first()
                            {
                                return quote!(
                                    pub fn #fn_name(&mut self,value: #inner_type)-> &mut Self {
                                        self.#fn_name = std::option::Option::Some(value);
                                        self
                                    }
                                );
                            };
                        };
                    }
                }
                quote!(
                    pub fn #fn_name(&mut self,value: #field_ty)-> &mut Self {
                        self.#fn_name = std::option::Option::Some(value);
                        self
                    }
                )
            }
            _ => {
                quote!(
                    pub fn #fn_name(&mut self,value: #field_ty)-> &mut Self {
                        self.#fn_name = std::option::Option::Some(value);
                        self
                    }
                )
            }
        }
    })
}

fn repeated_field(fields: Punctuated<Field, Comma>) -> proc_macro2::TokenStream {
    let all_fn = fields.into_iter().map(|field| {
        let all_fn = one_fn(field.clone());
        quote!(
            #(#all_fn)*
        )
    });
    quote!(
        #(#all_fn)*
    )
}

fn one_fn(field: Field) -> impl Iterator<Item = proc_macro2::TokenStream> {
    let _type = field.ty;
    let value_name = field.ident;
    let mut field_ty = _type.clone();
    if let Path(path) = _type.clone() {
        if let Some(segment) = path.path.segments.last() {
            if segment.ident == "Vec" {
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    if let Some(syn::GenericArgument::Type(inner_type)) = args.args.first() {
                        field_ty = inner_type.clone();
                    };
                };
            }
        }
    }
    field.attrs.into_iter().map(move |attr| {
        let mut ts = quote!();
        if let List(mete) = attr.meta.clone() {
            if !mete.path.is_ident("builder") {
                return quote!();
            }
            let expr: Expr = attr.parse_args().unwrap();
            if let Expr::Assign(exp) = expr {
                if let syn::Expr::Path(right) = *exp.left{
                    if !right.path.is_ident("each"){
                        let err_span = right.span();
                        let error_message = "error: expected `builder(each = \"...\")'";
                        return quote_spanned!(err_span => compile_error!(#error_message));
                    }
                }else {
                    let err_span = attr.span();
                    let error_message = "error: expected `builder(each = \"...\")'";
                    return quote_spanned!(err_span => compile_error!(#error_message));
                }
                if let Lit(right) = *exp.right {
                    if let syn::Lit::Str(right) = right.lit {
                        let fn_name = right.value();
                        let fn_ident =
                            proc_macro2::Ident::new(&fn_name, proc_macro2::Span::call_site());
                        ts = quote!(
                            pub fn #fn_ident(&mut self,value: #field_ty)-> &mut Self {
                                if let std::option::Option::Some(ref mut ref_value) = self.#value_name {
                                    ref_value.push(value);
                                }else{
                                    let mut new_vec:Vec<#field_ty> = Vec::new();
                                    new_vec.push(value);
                                    self.#value_name = std::option::Option::Some(new_vec);
                                };
                                self
                            }
                        );
                    }
                }
            }
        }
        ts
    })
}

fn build_fn_field(
    fields: Punctuated<Field, Comma>,
) -> impl Iterator<Item = proc_macro2::TokenStream> {
    fields.into_iter().map(|field| {
        let st_field_name = field.ident;
        let st_field_ty = field.ty;
        match st_field_ty {
            Path(path) => {
                if let Some(segment) = path.path.segments.last() {
                    if segment.ident == "Option" {
                        return quote!(
                            #st_field_name: self.#st_field_name.clone()
                        );
                    }
                }
                quote!(
                    #st_field_name: self.#st_field_name.clone().unwrap_or_default()
                )
            }
            _ => {
                quote!(
                    #st_field_name: self.#st_field_name.clone().unwrap_or_default()
                )
            }
        }
    })
}

fn builder_fields(
    fields: Punctuated<Field, Comma>,
) -> impl Iterator<Item = proc_macro2::TokenStream> {
    fields.into_iter().map(|field| {
        let st_field_name = field.ident;
        let st_field_ty = field.ty;
        let st_field_ty_1 = st_field_ty.clone();
        match st_field_ty {
            Path(path) => {
                if let Some(segment) = path.path.segments.last() {
                    if segment.ident == "Option" {
                        return quote!(
                            #st_field_name: #st_field_ty_1
                        );
                    }
                }
                quote!(
                    #st_field_name: std::option::Option<#st_field_ty_1>
                )
            }
            _ => {
                quote!(
                    #st_field_name: std::option::Option<#st_field_ty_1>
                )
            }
        }
    })
}
