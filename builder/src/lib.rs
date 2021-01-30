use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;

    let bname = format!("{}Builder", name);
    let bident = syn::Ident::new(&bname, name.span());

    let fields = match ast.data {
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
            ..
        }) => named,
        _ => panic!("can only derive Builder for structs"),
    };

    let builder_fields = fields.iter().map(|prop| {
        let name = &prop.ident;
        let ty = &prop.ty;

        if let std::option::Option::Some((_, group)) = attr_group(&prop.attrs, "builder") {
            let mut tokens = group.stream().into_iter();

            check_builder_attribute(&mut tokens);

            return quote! {
                #name:  #ty
            };
        }

        match inner_type(ty, "Option").is_some() {
            true => quote! {
                #name: #ty
            },
            false => quote! {
                #name: std::option::Option<#ty>
            },
        }
    });

    let builder_methods = fields.iter().map(|prop| {
        let name = &prop.ident;
        let ty = &prop.ty;

        if let std::option::Option::Some((attr, group)) = attr_group(&prop.attrs, "builder") {
            let mut tokens = group.stream().into_iter();

            if !check_builder_attribute(&mut tokens) {
                return syn::Error::new_spanned(
                    attr.parse_meta().unwrap(),
                    "expected `builder(each = \"...\")`",
                )
                .to_compile_error();
            }

            let arg = match tokens.next().unwrap() {
                TokenTree::Literal(l) => l,
                tt => panic!("expected string, found {:?}", tt),
            };

            let inner = inner_type(ty, "Vec");

            match syn::Lit::new(arg) {
                syn::Lit::Str(s) => {
                    let arg = syn::Ident::new(&s.value(), s.span());
                    return quote! {
                        fn #arg(&mut self, #arg: #inner) -> &mut Self {
                            self.#name.push(#arg);
                            self
                        }
                    };
                }
                lit => panic!("expected string, found {:?}", lit),
            }
        }

        match inner_type(ty, "Option") {
            std::option::Option::Some(inner) => quote! {
                pub fn #name(&mut self, #name: #inner) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            },
            std::option::Option::None => quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            },
        }
    });

    let set_fields = fields.iter().map(|prop| {
        let name = &prop.ident;

        if attr_group(&prop.attrs, "builder").is_some() {
            return quote! {
                #name: self.#name.clone()
            };
        }

        match inner_type(&prop.ty, "Option").is_some() {
            true => quote! {
                #name: self.#name.clone()
            },
            false => quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            },
        }
    });

    let default_fields = fields.iter().map(|prop| {
        let name = &prop.ident;

        match attr_group(&prop.attrs, "builder") {
            std::option::Option::Some(_) => quote! {
                #name: std::vec::Vec::new()
            },
            _ => quote! {
                #name: std::option::Option::None
            },
        }
    });

    let expanded = quote! {
        pub struct #bident {
            #(#builder_fields,)*
        }

        impl #bident {
            #(#builder_methods)*

            pub fn build(&self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                std::result::Result::Ok(#name {
                    #(#set_fields,)*
                })
            }
        }

        impl #name {
            pub fn builder() -> #bident {
                #bident {
                    #(#default_fields,)*
                }
            }
        }
    };

    expanded.into()
}

fn inner_type<'a>(ty: &'a syn::Type, name: &str) -> std::option::Option<&'a syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if !p.path.segments.len() == 1 || p.path.segments[0].ident != name {
            return std::option::Option::None;
        }

        if let syn::PathArguments::AngleBracketed(ref inner) = p.path.segments[0].arguments {
            if inner.args.len() != 1 {
                return std::option::Option::None;
            }

            let inner = inner.args.first().unwrap();
            if let syn::GenericArgument::Type(ref t) = inner {
                return std::option::Option::Some(t);
            }
        }
    }

    std::option::Option::None
}

fn attr_group<'a>(
    attrs: &'a Vec<syn::Attribute>,
    name: &str,
) -> std::option::Option<(&'a syn::Attribute, proc_macro2::Group)> {
    for attr in attrs {
        if attr.path.segments.len() == 1 && attr.path.segments[0].ident == name {
            if let std::option::Option::Some(proc_macro2::TokenTree::Group(group)) =
                attr.tokens.clone().into_iter().next()
            {
                return std::option::Option::Some((attr, group));
            }
        }
    }
    None
}

fn check_builder_attribute(tokens: &mut proc_macro2::token_stream::IntoIter) -> bool {
    if let std::option::Option::Some(TokenTree::Ident(ident)) = tokens.next() {
        if ident != "each" {
            return false;
        }
    }

    if let std::option::Option::Some(TokenTree::Punct(punct)) = tokens.next() {
        if punct.as_char() != '=' {
            return false;
        }
    }

    true
}
