use proc_macro::TokenStream;
use quote::quote;
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{
    parse_macro_input, parse_quote, Attribute, DeriveInput, Expr, Field, GenericParam, Generics,
    Ident, Lit, LitStr, Meta, PathSegment,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let st_name = input.ident;
    let st_attr = input.attrs;
    let attr_ident = attr_idnet(st_attr);
    let debug_name = st_name.clone().to_string();
    let data = input.data;
    let fields = match data {
        syn::Data::Struct(value) => {
            let fields = value.fields;
            match fields {
                syn::Fields::Named(value) => value.named,
                _ => panic!("not common fields"),
            }
        }
        _ => panic!("not DataStruct"),
    };

    let generics = input.generics;
    let ignore_generics = find_tpye_boxed_by_marker(fields.clone());
    let generics_value = find_tpye_genrics_value(fields.clone(), &generics);
    let mut where_bound = where_bound(generics_value.clone(), attr_ident.clone());
    let mut generics = add_trait_bounds(generics, ignore_generics);
    if have_where(&generics) {
        where_bound = quote!();
        generics = add_where_bounds(generics, generics_value.clone(), attr_ident);
    }
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let field_debug = fields_debug(fields.clone());

    quote!(
        impl #impl_generics std::fmt::Debug for #st_name #ty_generics #where_clause
        #where_bound
        {
            fn fmt(&self,f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result{
                f.debug_struct(#debug_name)
                    #field_debug
                    .finish()
            }
        }
    )
    .into()

    //quote!().into()
}

fn fields_debug(fields: Punctuated<Field, Comma>) -> proc_macro2::TokenStream {
    let field = fields
        .into_iter()
        .map(|field| each_field_attr(field.clone()));
    quote!(
        #(#field)*
    )
}
fn each_field_attr(field: Field) -> proc_macro2::TokenStream {
    let field_debug = field.ident.clone().expect("no value").to_string();
    let field_ty = field.ty;
    let field_name = field.ident;
    let expand = field.attrs.iter().map(|attr| {
        let mut expand = quote!();
        if let syn::Meta::NameValue(value) = attr.meta.clone() {
            if value.path.is_ident("debug") {
                if let Expr::Lit(right) = value.value {
                    if let Lit::Str(value) = right.lit {
                        let fmt_arg = value.value();
                        // let s = format_args!(#fmt_arg,self.#field_name);
                        // expand = quote!(
                        //     .field_with(#field_debug,|x|{
                        //         x.write_fmt(format_args!(#fmt_arg,self.#field_name))
                        //     })
                        // );
                        // expand = quote!(
                        //     .field(#field_debug, &(std::format!(#fmt_arg,self.#field_name)))
                        // );
                        expand = quote!(
                            .field(#field_debug,&{
                                struct A(#field_ty);
                                impl std::fmt::Debug for A {
                                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                                        f.write_fmt(format_args!(#fmt_arg,self.0))
                                    }
                                }
                                A(self.#field_name)
                            })
                        )
                    }
                }
            }
        }
        expand
    });

    if field.attrs.is_empty() {
        quote!(
            .field(#field_debug,&self.#field_name)
        )
    } else {
        quote!(
            #(#expand)*
        )
    }
}

fn add_trait_bounds(mut generics: Generics, exceptions: Vec<Ident>) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            let finded = exceptions.iter().any(|except| type_param.ident == *except);
            if !finded {
                type_param.bounds.push(parse_quote!(std::fmt::Debug));
            }
        };
    }
    generics
}

fn where_bound(
    new_bounds: Vec<Punctuated<PathSegment, syn::token::PathSep>>,
    attr_bound: Vec<LitStr>,
) -> proc_macro2::TokenStream {
    let new_bounds = new_bounds.into_iter().map(|x| quote!(#x: std::fmt::Debug));
    let attr_bounds = attr_bound.into_iter().map(|x| {
        let value = x.value();
        let _token = ':';
        let expr = value.rsplit_once(_token).expect("not with T: Trait");
        let left_expr: Expr = syn::parse_str(expr.0).expect("left not expr");
        let right_expr: Expr = syn::parse_str(expr.1).expect("right not expr");
        quote!(#left_expr: #right_expr)
    });
    quote!(
        where #(#new_bounds),*
            #(#attr_bounds),*
    )
}

fn add_where_bounds(
    mut generics: Generics,
    new_bounds: Vec<Punctuated<PathSegment, syn::token::PathSep>>,
    attr_bound: Vec<LitStr>,
) -> Generics {
    let new_bounds = new_bounds
        .into_iter()
        .map(|x| quote!(#x: std::fmt::Debug))
        .collect::<Vec<_>>();

    let attr_bound = attr_bound
        .into_iter()
        .map(|x| quote!(#x))
        .collect::<Vec<_>>();
    if let Some(mut where_clause) = generics.where_clause.clone() {
        new_bounds.into_iter().for_each(|ts| {
            where_clause.predicates.push(parse_quote!(
                #ts
            ))
        });
        generics.where_clause.replace(where_clause);
    };

    if let Some(mut where_clause) = generics.where_clause.clone() {
        attr_bound.into_iter().for_each(|ts| {
            where_clause.predicates.push(parse_quote!(
                #ts
            ))
        });
        generics.where_clause.replace(where_clause);
    };
    generics
}

fn have_where(generics: &Generics) -> bool {
    generics.where_clause.is_some()
}

fn find_tpye_boxed_by_marker(fields: Punctuated<Field, Comma>) -> Vec<Ident> {
    let mut vec: Vec<Ident> = vec![];
    fields.iter().for_each(|field| {
        if let syn::Type::Path(path) = field.ty.clone() {
            if let Some(segment) = path.path.segments.last() {
                if segment.ident == "PhantomData" {
                    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                        if let Some(syn::GenericArgument::Type(syn::Type::Path(type_path))) =
                            args.args.first()
                        {
                            if let Some(ident_name) = type_path.path.segments.last() {
                                vec.push(ident_name.ident.clone());
                            }
                        }
                    }
                }
            };
        }
    });
    vec
}
#[allow(unused)]
fn test(fields: Punctuated<Field, Comma>) -> Vec<Ident> {
    let mut vec: Vec<Ident> = vec![];
    fields.iter().for_each(|field| {
        let syn::Type::Path(path) = field.ty.clone() else { return;};
        let Some(segment) = path.path.segments.last() else {return;};
        if segment.ident != "PhantomData" {return;};
        let syn::PathArguments::AngleBracketed(args) = &segment.arguments else {return;};
        let Some(syn::GenericArgument::Type(syn::Type::Path(type_path))) = args.args.first() else {return;};
        if let Some(ident_name) = type_path.path.segments.last() {
            vec.push(ident_name.ident.clone());
        };
    });
    vec
}

fn find_tpye_genrics_value(
    fields: Punctuated<Field, Comma>,
    generics: &Generics,
) -> Vec<Punctuated<PathSegment, syn::token::PathSep>> {
    let mut res = vec![];
    fields.iter().for_each(|field| {
        if let syn::Type::Path(path) = field.ty.clone() {
            if let Some(last_path) = path.path.segments.last() {
                if let syn::PathArguments::AngleBracketed(args) = &last_path.arguments {
                    if let Some(syn::GenericArgument::Type(syn::Type::Path(type_path))) =
                        args.args.first()
                    {
                        if type_path.path.segments.len() >= 2 {
                            let first = type_path.path.segments.first().expect("err");
                            let is_generic = generics.params.iter().any(|param| {
                                if let GenericParam::Type(ref type_param) = *param {
                                    first.ident == type_param.ident
                                } else {
                                    false
                                }
                            });
                            if is_generic {
                                res.push(path.path.segments.clone());
                            }
                        }
                    }
                }
            }

            if path.path.segments.len() >= 2 {
                let first = path.path.segments.first().expect("err");
                let is_generic = generics.params.iter().any(|param| {
                    if let GenericParam::Type(ref type_param) = *param {
                        first.ident == type_param.ident
                    } else {
                        false
                    }
                });
                if is_generic {
                    res.push(path.path.segments.clone());
                }
            }
        }
    });
    res
}

fn attr_idnet(attrs: Vec<Attribute>) -> Vec<LitStr> {
    let mut vec = Vec::new();
    attrs.iter().for_each(|attr| {
        if let Meta::List(ref meta_value) = attr.meta {
            if meta_value.path.is_ident("debug") {
                if let syn::Expr::Assign(expr) = attr.parse_args::<Expr>().expect("error not expr")
                {
                    if let syn::Expr::Path(ref left_path) = *expr.left {
                        if left_path.path.is_ident("bound") {
                            if let syn::Expr::Lit(ref right_path) = *expr.right {
                                if let syn::Lit::Str(ref should_bound) = right_path.lit {
                                    vec.push(should_bound.clone());
                                }
                            }
                        }
                    }
                }
            }
        }
    });
    vec
}
