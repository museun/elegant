use crate::*;

use std::collections::HashSet;

pub struct Closure {
    global: HashSet<String>,
}

impl Closure {
    pub fn convert(expr: Expr) -> (Expr, Vec<Expr>) {
        let mut this = Self {
            global: HashSet::new(),
        };

        this.visit(&HashSet::new(), expr)
    }

    fn visit(&mut self, env: &HashSet<String>, expr: Expr) -> (Expr, Vec<Expr>) {
        use self::Expr::*;
        match expr.clone() {
            Comparison(_, _, _) | Bool(_) | Symbol(_) | Number(_) | Func(_) => (expr, vec![]),

            Lambda(mut args, body) => {
                let mut new_env = env.clone();
                for arg in &args {
                    new_env.insert(arg.clone());
                }
                let (body, mut defines) = self.visit(&new_env, *body);
                let name = next_name(Named::Lambda);
                let free = self.free_variables(env, &HashSet::new(), expr);
                let mut loaded = body;
                for (i, var) in free.iter().enumerate() {
                    let bindings = vec![(
                        var.to_string(),
                        Apply(
                            Box::new(Symbol("tuple-ref".to_string())),
                            vec![Symbol("clos".to_string()), Number((i + 1) as i64)],
                        ),
                    )];
                    loaded = Let(bindings, Box::new(loaded))
                }
                args.insert(0, "clos".to_string());
                let mut closures = vec![Func(name.clone())];

                let free = free
                    .iter()
                    .map(|s| Symbol(s.to_string()))
                    .collect::<Vec<_>>();

                closures.extend_from_slice(&free);

                let closure = Tuple(closures);
                defines.push(Define(name, args, Box::new(loaded)));
                (closure, defines)
            }
            Define(name, mut args, body) => {
                let mut new_env = env.clone();
                for arg in &args {
                    new_env.insert(arg.clone());
                }

                let (body, body_defs) = self.visit(&new_env, *body);
                args.insert(0, "clos".to_string());
                let closure = Define(name, args, Box::new(body));

                (closure, body_defs)
            }
            If(cond, then, else_) => {
                let (cond, mut cond_defs) = self.visit(env, *cond);
                let (then, then_defs) = self.visit(env, *then);
                let (else_, else_defs) = self.visit(env, *else_);

                let closure = If(Box::new(cond), Box::new(then), Box::new(else_));

                cond_defs.extend_from_slice(&then_defs);
                cond_defs.extend_from_slice(&else_defs);

                (closure, cond_defs)
            }
            Let(bindings, body) => {
                let (mut new_bindings, mut defines) = (vec![], vec![]);
                let mut new_env = env.clone();

                for (k, v) in bindings {
                    let (cv, d) = self.visit(env, v);
                    new_env.insert(k.clone());
                    new_bindings.push((k, cv));
                    defines.extend_from_slice(&d);
                }

                let (body, new_defines) = self.visit(&new_env, *body);
                defines.extend_from_slice(&new_defines);
                let closure = Let(new_bindings, Box::new(body));

                (closure, defines)
            }
            Tuple(tuple) => {
                let (mut tup, mut defines) = (vec![], vec![]);

                for tuple in tuple.clone() {
                    let (cv, d) = self.visit(env, tuple);
                    tup.push(cv);
                    defines.extend_from_slice(&d);
                }

                let closure = Tuple(tup);
                (closure, defines)
            }

            Program(defines, main) => {
                let (mut defs, mut new_defs) = (vec![], vec![]);
                for def in defines {
                    self.global.insert(Self::defined_name(&def));
                    let (cv, d) = self.visit(env, def.clone());
                    defs.push(cv);
                    new_defs.extend_from_slice(&d);
                }

                let (main, main_defs) = self.visit(env, *main);
                defs.extend_from_slice(&new_defs);
                defs.extend_from_slice(&main_defs);

                let closure = Program(defs, Box::new(main));
                (closure, vec![])
            }
            Apply(f, args) => {
                let f = match *f {
                    Symbol(f) => f,
                    _ => unreachable!(),
                };
                if Self::is_primitive(&f) {
                    let (mut new_args, mut defines) = (vec![], vec![]);
                    for arg in args.clone() {
                        let (cv, d) = self.visit(env, arg);
                        new_args.push(cv);
                        defines.extend_from_slice(&d);
                    }
                    let closure = Apply(Box::new(Symbol(f)), new_args);
                    (closure, defines)
                } else {
                    let name = match self.global.get(&f) {
                        Some(_) => Tuple(vec![Func(f.to_string())]),
                        None => Symbol(f.to_string()),
                    };
                    let (c, d) = self.visit(env, name);
                    let tmp = next_name(Named::Apply);

                    let (mut cargs, mut defines) = (vec![Symbol(tmp.to_string())], vec![]);

                    for arg in args.clone() {
                        let (cv, d) = self.visit(env, arg);
                        cargs.push(cv);
                        defines.extend_from_slice(&d);
                    }

                    defines.extend_from_slice(&d);
                    let closure = Let(
                        vec![(tmp.clone(), c)],
                        Box::new(Apply(
                            Box::new(Apply(
                                Box::new(Symbol("tuple-ref".to_string())),
                                vec![Symbol(tmp), Number(0)],
                            )),
                            cargs,
                        )),
                    );
                    (closure, defines)
                }
            }

            // apply
            // apply prim
            e => unimplemented!("{:?}", e),
        }
    }

    fn free_variables(
        &mut self,
        env: &HashSet<String>,
        parent: &HashSet<String>,
        expr: Expr,
    ) -> Vec<String> {
        use self::Expr::*;
        match expr {
            Number(_) | Bool(_) => vec![],
            Symbol(name) => env
                .get(&name)
                .or_else(|| parent.get(&name))
                .and_then(|_| Some(vec![name.clone()]))
                .or_else(|| panic!("no binding found in parent env for free-variable {}", name))
                .unwrap(),

            If(cond, then, else_) => {
                let mut cond_free = self.free_variables(env, parent, *cond);
                let then_free = self.free_variables(env, parent, *then);
                let else_free = self.free_variables(env, parent, *else_);
                cond_free.extend_from_slice(&then_free);
                cond_free.extend_from_slice(&else_free);
                cond_free
            }
            Define(_, args, body) | Lambda(args, body) => {
                let new_parent = env.union(&parent).cloned().collect();
                let new_env = args.into_iter().collect::<HashSet<_>>();
                self.free_variables(&new_env, &new_parent, *body)
            }
            Let(bindings, body) => {
                let mut new_env = HashSet::new();
                let mut free = vec![];
                for (k, v) in bindings {
                    let f = self.free_variables(env, parent, v);
                    new_env.insert(k);
                    free.extend_from_slice(&f);
                }

                let new_env = new_env.union(&env).cloned().collect();
                let f = self.free_variables(&new_env, parent, *body);
                free.extend_from_slice(&f);
                free
            }
            Apply(_, args) => {
                let mut free = vec![];
                for arg in args {
                    let f = self.free_variables(env, parent, arg);
                    free.extend_from_slice(&f);
                }
                free
            }
            e => unimplemented!("{:?}", e),
        }
    }

    fn is_primitive(sym: &str) -> bool {
        match sym {
            "+" | "-" | "tuple-ref" | "tuple" => true,
            _ => false,
        }
    }

    fn defined_name(def: &Expr) -> String {
        match def {
            Expr::Define(ref name, _, _) => name.to_string(),
            e => panic!("not a define: {:?}", e),
        }
    }
}
