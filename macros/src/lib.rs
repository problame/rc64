extern crate proc_macro;
use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::parse_macro_input;

#[derive(Debug)]
struct Mi {
    variant: syn::Variant,
    addr_modes: Vec<syn::Path>,
}

impl syn::parse::Parse for Mi {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let variant = input.parse()?;
        input.parse::<syn::Token![,]>()?;
        let addr_modes = syn::punctuated::Punctuated::<syn::Path, syn::Token![,]>::parse_terminated(input)?
            .iter()
            .cloned()
            .collect();
        Ok(Mi { variant, addr_modes })
    }
}

#[proc_macro_attribute]
pub fn gen_instr_match(_: TokenStream, item: TokenStream) -> TokenStream {
    let mut f: syn::ItemFn = parse_macro_input!(item);

    struct MiReplace;

    impl syn::visit_mut::VisitMut for MiReplace {
        fn visit_expr_match_mut(&mut self, m: &mut syn::ExprMatch) {
            let mut mods = Vec::new();
            for (idx, arm) in m.arms.iter_mut().enumerate() {
                if let syn::Pat::Macro(syn::PatMacro { mac, .. }) = &arm.pat {
                    let segs =
                        mac.path.segments.iter().map(|seg| seg.ident.to_string()).collect::<Vec<_>>();

                    if segs.len() == 1 && segs[0] == "mi" {
                        // parse mi!
                        let mi: Mi = syn::parse2(mac.tokens.clone()).expect("parse mi! args");
                        let v = &mi.variant;
                        let patterns_expanded =
                            mi.addr_modes.iter().fold(proc_macro2::TokenStream::new(), |mut a, m| {
                                if !a.is_empty() {
                                    a.extend(quote! {|});
                                }
                                a.extend(quote! { Instr(#v, #m(_)) });
                                a
                            });
                        mods.push((idx, patterns_expanded));
                    } else if segs.len() == 1 && segs[0] == "branch" {
                        // parse branch!
                    }
                }
            }
            for (idx, out) in mods {
                // eprintln!("rewrite {} to {:#?}", idx, out);
                let ors: syn::punctuated::Punctuated<syn::Pat, syn::token::Or> = syn::parse_quote!(#out);
                m.arms[idx].pat =
                    syn::Pat::Or(syn::PatOr { attrs: Vec::new(), leading_vert: None, cases: ors });
            }
        }
    }

    use syn::visit_mut::VisitMut;
    MiReplace.visit_item_fn_mut(&mut f);

    f.to_token_stream().into()
}

// #[proc_macro_attribute]
// pub fn gen_instr_match(_: TokenStream, item: TokenStream) -> TokenStream {
//     let mut f: syn::ItemFn = parse_macro_input!(item);

// }
