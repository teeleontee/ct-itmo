use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ItemStruct};

#[proc_macro_derive(Scan)]
pub fn derive_scan(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as ItemStruct);
    let name = input.ident;
    let field_indents = input.fields.into_iter().map(|f| f.ident.unwrap());

    if field_indents.len() == 0 {
        quote! {
            impl Scan for #name {
                fn get_pointers(&self) -> Vec<usize> {
                    vec![]
                }
            }
        }.into()
    } else {
        quote! {
            impl Scan for #name {
                fn get_pointers(&self) -> Vec<usize> {
                    let a = [#(self.#field_indents .get_pointers(),)*];
                    a.into_iter().flatten().collect::<Vec<_>>()
                }
            }
        }.into()
    }

} 

