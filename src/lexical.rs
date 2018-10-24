use crate::*;
use std::collections::HashMap;

pub struct Lexical {
    map: HashMap<String, String>,
}

impl Lexical {
    pub fn new() -> Self {
        let mut map = HashMap::new();
        ["+", "-", "tuple-ref"].iter().for_each(|s| {
            map.insert(s.to_string(), s.to_string());
        });
        Self { map }
    }

    pub fn visit(&mut self, expr: Expr) -> Expr {
        use self::Expr::*;
        match expr {
            Symbol(name) => Symbol(self.map[&name].clone()),
            Number(_) | Bool(_) => expr,
            Tuple(tuple) => {
                let tuple = tuple.iter().map(|s| self.visit(s.clone())).collect();
                Tuple(tuple)
            }
            Func(_) => unreachable!("func name shouldn't exist before closure conversion"),
            Let(bindings, body) => {
                let mut out = vec![];
                for (k, v) in bindings {
                    let name = next_name(Named::Let(k.to_string()));
                    self.map.insert(k.clone(), name.clone());
                    out.push((name, self.visit(v)))
                }
                Let(out, Box::new(self.visit(*body)))
            }
            List(mut list) => {
                // TODO do I want to modify this?
                list = list.iter().map(|s| self.visit(s.clone())).collect();
                List(list)
            }
            Define(name, args, val) => {
                let k = next_name(Named::Define(name.to_string()));
                self.map.insert(name, k.clone());

                let mut out = vec![];
                for arg in args {
                    let k = next_name(Named::Arg(arg.to_string()));
                    out.push(k.clone());
                    self.map.insert(arg, k);
                }
                Define(k, out, Box::new(self.visit(*val)))
            }
            Lambda(args, body) => {
                let mut out = vec![];
                for arg in args {
                    let k = next_name(Named::Arg(arg.to_string()));
                    out.push(k.clone());
                    self.map.insert(arg, k);
                }
                Lambda(out, Box::new(self.visit(*body)))
            }
            If(cond, then, else_) => If(
                Box::new(self.visit(*cond)),
                Box::new(self.visit(*then)),
                Box::new(self.visit(*else_)),
            ),
            Comparison(cmp, left, right) => Comparison(
                cmp,
                Box::new(self.visit(*left)),
                Box::new(self.visit(*right)),
            ),
            Apply(f, mut args) => {
                // TODO do I want to modify this?
                args = args.iter().map(|a| self.visit(a.clone())).collect();
                Apply(Box::new(self.visit(*f)), args)
            }
            Program(mut defs, expr) => {
                // TODO do I want to modify this?
                defs = defs.iter().map(|d| self.visit(d.clone())).collect();
                Program(defs, Box::new(self.visit(*expr)))
            }

            EOF => unreachable!(),
        }
    }
}
