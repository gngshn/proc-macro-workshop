use proc_macro2::{Ident, TokenStream};
use quote::quote;
use syn::{
    punctuated::Punctuated, AngleBracketedGenericArguments, Data::Struct, DataStruct, DeriveInput,
    Field, Fields::Named, FieldsNamed, GenericArgument, PathArguments, Token, Type, TypePath,
};

#[proc_macro_derive(Builder)]
pub fn derive(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(tokens as DeriveInput);
    match expand(input) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn expand(input: DeriveInput) -> syn::Result<TokenStream> {
    let struct_ident = &input.ident;
    let struct_literal = struct_ident.to_string();
    let builder_literal = format!("{}Builder", struct_literal);
    let builder_ident = syn::Ident::new(&builder_literal, struct_ident.span());

    let fields = get_fields_name(&input)?;
    let fields_idents = fields.iter().map(|field| &field.ident).collect::<Vec<_>>();
    let fields_types = fields.iter().map(|field| &field.ty).collect::<Vec<_>>();

    let builder_structure_setter_methods =
        expand_builder_setter_methods(&fields_idents, &fields_types)?;

    let tokens = quote!(
        pub struct #builder_ident {
            #(#fields_idents: ::std::option::Option<#fields_types>,)*
        }

        impl #struct_ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#fields_idents: ::std::option::Option::None,)*
                }
            }
        }

        impl #builder_ident {
            #builder_structure_setter_methods

            pub fn build(&self) -> ::std::option::Option<#struct_ident> {
                ::std::option::Option::Some(#struct_ident {
                    #(#fields_idents: self.#fields_idents.clone().unwrap_or_default(),)*
                })
            }
        }
    );

    Ok(tokens)
}

fn get_fields_name(input: &DeriveInput) -> syn::Result<&Punctuated<Field, Token!(,)>> {
    if let Struct(DataStruct {
        fields: Named(FieldsNamed { ref named, .. }),
        ..
    }) = input.data
    {
        return Ok(named);
    }
    Err(syn::Error::new_spanned(
        input,
        "only can be use on structure",
    ))
}

fn expand_builder_setter_methods(
    fields_idents: &Vec<&Option<Ident>>,
    fields_types: &Vec<&Type>,
) -> syn::Result<TokenStream> {
    let mut tokens = TokenStream::new();
    fields_types
        .clone()
        .into_iter()
        .zip(fields_idents.clone().into_iter())
        .for_each(|(type_, ident)| {
            if let Type::Path(TypePath { path, .. }) = type_ {
                if let Some(segment) = path.segments.last() {
                    if segment.ident == "Option" {
                        if let PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                            ref args,
                            ..
                        }) = segment.arguments {
                            if let Some(GenericArgument::Type(unwrap_type)) = args.first() {
                                tokens.extend(quote!(
                                    pub fn #ident(&mut self, #ident: #unwrap_type) ->&mut Self {
                                        self.#ident = ::std::option::Option::Some(::std::option::Option::Some(#ident));
                                        self
                                    }));
                            }
                        }
                    } else {
                        tokens.extend(quote!(
                            pub fn #ident(&mut self, #ident: #type_) ->&mut Self {
                                self.#ident = ::std::option::Option::Some(#ident);
                                self
                            }));
                    }
                }
            }
        });

    Ok(tokens)
}
