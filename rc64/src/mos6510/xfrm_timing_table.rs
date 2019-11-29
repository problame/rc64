fn main() {
    use scraper::{Html, Selector};

    let html = include_str!("timing_table.html");

    let fragment = Html::parse_fragment(html);
    let selector =
        Selector::parse("tr:not(:first-child) td:not(:first-child):not([bgcolor]) font").unwrap();
    use std::collections::HashSet;
    let mut opcs = HashSet::new();
    let mut modes = HashSet::new();
    struct Info {
        opc: String,
        mode: Option<String>,
        cycles: usize,
        page_boundary_dependent: bool,
    }
    impl std::fmt::Debug for Info {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "{} {:?} ({:?} cycles)", self.opc, self.mode, self.cycles)
        }
    }
    let mut infos = Vec::new();
    for element in fragment.select(&selector) {
        let cell = element.inner_html();
        let cell = cell.trim();
        // like BRK<br>7
        // like ORA<br>izx 6
        // like BPL<br>rel 2*
        // like KIL
        if cell == "KIL" {
            continue;
        }
        let comps: Vec<_> = cell.split("<br>").collect();
        assert_eq!(comps.len(), 2);
        let opc = comps[0].to_owned();
        let mode = comps[1].to_owned();
        if opc.starts_with("<i>") {
            continue;
        }
        opcs.insert(opc.clone());
        modes.insert(mode.clone());

        let comps: Vec<_> = mode.split_whitespace().collect();
        assert!(!comps.is_empty());
        let (mode, cycles, page_boundary_dependent) = if comps.len() == 1 {
            (None, comps[0].parse().unwrap(), false)
        } else if comps.len() == 2 {
            let mut idx = comps[1].len();
            if let Some(star) = comps[1].find('*') {
                idx = star;
            };
            (
                Some(comps[0].to_owned()),
                comps[1][..idx].parse().unwrap(),
                idx != comps[1].len(),
            )
        } else {
            unimplemented!()
        };

        infos.push(Info {
            opc,
            mode,
            cycles,
            page_boundary_dependent,
        });
    }
    // println!("{:#?}", infos);
    // println!("opcs (count={}) = {:?}", opcs.len(), opcs);
    // println!("modes = {:#?}", modes);

    for info in infos {
        use std::fmt::Write;
        let Info {
            opc,
            mode,
            cycles,
            page_boundary_dependent,
        } = info;
        let modes = match mode.as_ref().map(String::as_str) {
            None => None,
            Some("imm") => Some("Imm(_)"),
            Some("zp") => Some("Zpi(_)"),
            Some("zpx") => Some("ZpX(_)"),
            Some("zpy") => Some("ZpY(_)"),
            Some("rel") => Some("PCr(_)"),
            Some("abs") => Some("Abs(_)"),
            Some("abx") => Some("AbX(_)"),
            Some("aby") => Some("AbY(_)"),
            Some("ind") => Some("Ind(_)"),
            Some("izx") => Some("IzX(_)"),
            Some("izy") => Some("IzY(_)"),
            _ => unimplemented!(),
        }
        .map(|s| vec![s.to_owned()])
        .unwrap_or(vec!["Imp".to_owned(), "Acc".to_owned()]);
        let page_boundary_dependent = if page_boundary_dependent {
            "AddOneCycle"
        } else {
            "NoAdditionalCycle"
        };
        for mode in modes {
            let mut s = String::new();
            write!(
                &mut s,
                r"
                Instr(Op::{}, Addr::{}) => ({}, {}),
            ",
                opc, mode, cycles, page_boundary_dependent
            )
            .unwrap();
            println!("{}", s.trim());
        }
    }
}
