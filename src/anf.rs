use crate::parser::Expr;
use crate::*;

use std::io::prelude::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Symbol(String),
    Number(i64),
    Bool(bool),
    Tuple(Vec<Node>),
    Func(String), // this is for converting to a closure

    Return(Box<Node>),
    Assign(String, Box<Node>),
    If(Box<Node>, Vec<Node>, Vec<Node>),
    Comparison(Comparison, Box<Node>, Box<Node>),
    Apply(String, Vec<Node>),
    Primitive(String, Vec<Node>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ANF {
    Program(Vec<ANF>, Vec<Node>, Vec<String>),
    Define(String, Vec<String>, Vec<Node>, Vec<String>),
    Node(Node, Vec<Node>, Vec<String>),
}

impl ANF {
    pub fn print<W: Write>(&self, w: &mut W) {
        Dump::ANF(self).dump(w, 0)
    }

    pub fn flatten(expr: Expr) -> ANF {
        use self::Expr::*;
        match expr {
            Symbol(name) => ANF::Node(Node::Symbol(name.clone()), vec![], vec![name]),
            Number(n) => ANF::Node(Node::Number(n), vec![], vec![]),
            Bool(b) => ANF::Node(Node::Bool(b), vec![], vec![]),
            Tuple(tuple) => {
                let tmp = next_name(Named::Tuple);
                let (mut list, mut assigns, mut vars) = (vec![], vec![], vec![]);

                for el in tuple {
                    let (node, assign, var) = match Self::flatten(el) {
                        ANF::Node(node, assign, vars) => (node, assign, vars),
                        _ => unreachable!(),
                    };
                    list.push(node);
                    assigns.extend_from_slice(&assign);
                    vars.extend_from_slice(&var)
                }

                assigns.push(Node::Assign(tmp.to_string(), Box::new(Node::Tuple(list))));
                vars.push(tmp.to_string());
                ANF::Node(Node::Symbol(tmp), assigns, vars)
            }
            Func(name) => ANF::Node(Node::Func(name.clone()), vec![], vec![name]),
            Define(name, args, body) => {
                let (body, mut assigns, mut vars) = match Self::flatten(*body) {
                    ANF::Node(body, assigns, vars) => (body, assigns, vars),
                    _ => unreachable!(),
                };
                assigns.push(Node::Return(Box::new(body)));
                vars.retain(|a| !args.contains(a));
                ANF::Define(name, args, assigns, vars)
            }
            Let(bindings, body) => {
                let (body, assigns, vars) = match Self::flatten(*body) {
                    ANF::Node(body, assigns, vars) => (body, assigns, vars),
                    _ => unreachable!(),
                };
                let (mut bassigns, mut bvars) = (vec![], vec![]);
                for (k, v) in bindings {
                    let (n, fa, fv) = match Self::flatten(v) {
                        ANF::Node(body, assigns, vars) => (body, assigns, vars),
                        _ => unreachable!(),
                    };
                    if let Node::Symbol(name) = &n {
                        bvars.push(name.clone());
                    }
                    bassigns.extend_from_slice(&fa);
                    bassigns.push(Node::Assign(k.clone(), Box::new(n)));
                    bvars.extend_from_slice(&fv);
                    bvars.push(k);
                }
                bassigns.extend_from_slice(&assigns);
                bvars.extend_from_slice(&vars);
                ANF::Node(body, bassigns, bvars)
            }
            If(cond, then, else_) => {
                let (cond_node, mut cond_assigns, mut cond_vars) = match Self::flatten(*cond) {
                    ANF::Node(node, assigns, vars) => (node, assigns, vars),
                    _ => unreachable!(),
                };
                let (then_node, mut then_assigns, mut then_vars) = match Self::flatten(*then) {
                    ANF::Node(node, assigns, vars) => (node, assigns, vars),
                    _ => unreachable!(),
                };
                let (else_node, mut else_assigns, mut else_vars) = match Self::flatten(*else_) {
                    ANF::Node(node, assigns, vars) => (node, assigns, vars),
                    _ => unreachable!(),
                };

                let if_tmp = next_name(Named::If);
                then_assigns.push(Node::Assign(if_tmp.clone(), Box::new(then_node)));
                else_assigns.push(Node::Assign(if_tmp.clone(), Box::new(else_node)));
                let if_ = Node::If(Box::new(cond_node), then_assigns, else_assigns);
                // TODO look at this ordering
                cond_assigns.push(if_);
                cond_vars.append(&mut then_vars);
                cond_vars.append(&mut else_vars);
                cond_vars.push(if_tmp.clone());

                ANF::Node(Node::Symbol(if_tmp), cond_assigns, cond_vars)
            }
            Comparison(cmp, left, right) => {
                let (left_node, mut left_assigns, mut left_vars) = match Self::flatten(*left) {
                    ANF::Node(node, assigns, vars) => (node, assigns, vars),
                    _ => unreachable!(),
                };
                let (right_node, mut right_assigns, mut right_vars) = match Self::flatten(*right) {
                    ANF::Node(node, assigns, vars) => (node, assigns, vars),
                    _ => unreachable!(),
                };

                let comp_tmp = next_name(Named::Cmp(cmp));
                left_assigns.append(&mut right_assigns);
                left_assigns.push(Node::Assign(
                    comp_tmp.clone(),
                    Box::new(Node::Comparison(
                        cmp,
                        Box::new(left_node),
                        Box::new(right_node),
                    )),
                ));
                left_vars.append(&mut right_vars);
                left_vars.push(comp_tmp.clone());
                ANF::Node(Node::Symbol(comp_tmp), left_assigns, left_vars)
            }

            // this is going to be tedious
            Apply(func, args) => match *func {
                Symbol(name) => match (name.as_str(), args.as_slice()) {
                    ("+", [arg1, arg2]) => {
                        let (arg1_node, mut arg1_assigns, mut arg1_vars) =
                            match Self::flatten(arg1.clone()) {
                                ANF::Node(node, assigns, vars) => (node, assigns, vars),
                                _ => unreachable!(),
                            };

                        let (arg2_node, mut arg2_assigns, mut arg2_vars) =
                            match Self::flatten(arg2.clone()) {
                                ANF::Node(node, assigns, vars) => (node, assigns, vars),
                                _ => unreachable!(),
                            };

                        let plus_tmp = next_name(Named::Plus);
                        let plus_node = Node::Assign(
                            plus_tmp.clone(),
                            Box::new(Node::Primitive("+".to_string(), vec![arg1_node, arg2_node])),
                        );

                        arg1_assigns.append(&mut arg2_assigns);
                        arg1_assigns.push(plus_node);

                        arg1_vars.append(&mut arg2_vars);
                        arg1_vars.push(plus_tmp.clone());

                        ANF::Node(Node::Symbol(plus_tmp), arg1_assigns, arg1_vars)
                    }
                    ("-", [arg]) => {
                        let (expr_node, mut expr_assigns, mut expr_vars) =
                            match Self::flatten(arg.clone()) {
                                ANF::Node(node, assigns, vars) => (node, assigns, vars),
                                _ => unreachable!(),
                            };
                        let neg_tmp = next_name(Named::Neg);
                        let neg_node = Node::Assign(
                            neg_tmp.clone(),
                            Box::new(Node::Primitive("-".to_string(), vec![expr_node])),
                        );
                        expr_assigns.push(neg_node);
                        expr_vars.push(neg_tmp.clone());
                        ANF::Node(Node::Symbol(neg_tmp), expr_assigns, expr_vars)
                    }
                    ("tuple-ref", [tuple, Number(index)]) => {
                        let (tuple_node, mut tuple_assigns, mut tuple_vars) =
                            match Self::flatten(tuple.clone()) {
                                ANF::Node(node, assigns, vars) => (node, assigns, vars),
                                _ => unreachable!(),
                            };

                        let ref_tmp = next_name(Named::Ref);
                        let ref_node = Node::Assign(
                            ref_tmp.clone(),
                            Box::new(Node::Primitive(
                                "tuple-ref".to_string(),
                                vec![tuple_node, Node::Number(*index)],
                            )),
                        );

                        tuple_assigns.push(ref_node);
                        tuple_vars.push(ref_tmp.clone());

                        ANF::Node(Node::Symbol(ref_tmp), tuple_assigns, tuple_vars)
                    }

                    (f, [..]) => Self::flatten(Apply(
                        Box::new(Symbol("tuple-ref".to_string())),
                        vec![Tuple(vec![Func(f.to_string()), Number(0)])],
                    )),
                },
                Apply(_, _) => {
                    let (apply_node, mut apply_assigns, mut apply_vars) = match Self::flatten(*func)
                    {
                        ANF::Node(Node::Symbol(node), assigns, vars) => (node, assigns, vars),
                        _ => unreachable!(),
                    };

                    let apply_tmp = next_name(Named::Apply);
                    let (args, assigns, vars) = Self::flatten_args(&args);

                    let apply =
                        Node::Assign(apply_tmp.clone(), Box::new(Node::Apply(apply_node, args)));

                    apply_assigns.extend_from_slice(&assigns);
                    apply_assigns.push(apply);

                    // name first
                    apply_vars.push(apply_tmp.clone());
                    apply_vars.extend_from_slice(&vars);

                    ANF::Node(Node::Symbol(apply_tmp), apply_assigns, apply_vars)
                }
                e => unimplemented!("not a function: {:?}", e),
            },
            Program(defs, expr) => {
                let (expr_node, mut expr_assigns, mut expr_vars) = match Self::flatten(*expr) {
                    ANF::Node(node, assigns, vars) => (node, assigns, vars),
                    _ => unreachable!(),
                };
                expr_assigns.push(Node::Return(Box::new(expr_node)));
                expr_vars.dedup();
                let mut flat = vec![];
                for def in defs {
                    flat.push(Self::flatten(def));
                }
                ANF::Program(flat, expr_assigns, expr_vars)
            }
            EOF => unreachable!(),
            List(_) => unimplemented!("TODO lists"),
            Lambda(_, _) => unreachable!("closure conversion should happen before Self::flatten"),
        }
    }

    fn flatten_args(inputs: &[Expr]) -> (Vec<Node>, Vec<Node>, Vec<String>) {
        let (mut args, mut assigns, mut vars) = (vec![], vec![], vec![]);

        for arg in inputs {
            let (arg_node, arg_assigns, arg_vars) = match ANF::flatten(arg.clone()) {
                ANF::Node(node, assigns, vars) => (node, assigns, vars),
                _ => unreachable!(),
            };
            args.push(arg_node);
            assigns.extend_from_slice(&arg_assigns);
            vars.extend_from_slice(&arg_vars);
        }

        (args, assigns, vars)
    }
}

#[derive(Debug)]
enum Dump<'a> {
    ANF(&'a ANF),
    Node(&'a Node),
}

impl<'a> Dump<'a> {
    fn dump<W: Write>(&'a self, w: &mut W, depth: usize) {
        match self {
            Dump::ANF(ANF::Program(defines, assigns, vars)) => {
                writeln!(w, "{}anf program:", pad(depth));
                writeln!(w, "{}vars:", pad(depth + 4));
                for var in vars {
                    writeln!(w, "{}{}", pad(depth + 4 + 4), var);
                }
                writeln!(w);

                writeln!(w, "{}assigns:", pad(depth + 4));
                for assign in assigns {
                    Dump::Node(assign).dump(w, depth + 4 + 4);
                }
                writeln!(w);

                writeln!(w, "{}defines:", pad(depth + 4));
                for define in defines {
                    Dump::ANF(define).dump(w, depth + 4 + 4);
                }
                writeln!(w);
            }
            Dump::ANF(ANF::Define(defines, args, assigns, vars)) => {
                write!(w, "{}{}(", pad(depth), defines);
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(w, ", ");
                    }
                    write!(w, "{}", arg);
                }
                writeln!(w, "):");
                writeln!(w, "{}assigns:", pad(depth + 4));
                for assign in assigns {
                    Dump::Node(assign).dump(w, depth + 4 + 4);
                }
                writeln!(w);
                writeln!(w, "{}vars:", pad(depth + 4));
                for var in vars {
                    writeln!(w, "{}{}", pad(depth + 4 + 4), var);
                }
                writeln!(w);
            }
            Dump::Node(Node::Assign(name, node)) => {
                write!(w, "{}{} = ", pad(depth), name);
                Dump::Node(node).dump(w, depth + 4);
                writeln!(w);
            }
            Dump::Node(Node::Return(node)) => {
                write!(w, "{}return ", pad(depth));
                Dump::Node(node).dump(w, depth + 4);
                writeln!(w);
            }
            Dump::Node(Node::Symbol(name)) => {
                write!(w, "#{}", name);
            }
            Dump::Node(Node::Primitive(name, nodes)) => {
                write!(w, "({} ", name);
                for (i, node) in nodes.iter().enumerate() {
                    if i > 0 {
                        write!(w, ", ");
                    }
                    Dump::Node(node).dump(w, depth + 4)
                }
                write!(w, ")");
            }
            Dump::Node(Node::Bool(n)) => {
                write!(w, "{}", n);
            }
            Dump::Node(Node::Number(n)) => {
                write!(w, "{}", n);
            }
            Dump::Node(Node::Comparison(comp, left, right)) => {
                write!(
                    w,
                    "{}",
                    match comp {
                        Comparison::Equal => "=",
                        Comparison::LessThan => "<",
                        Comparison::LessThanEqual => "<=",
                        Comparison::GreaterThan => ">",
                        Comparison::GreaterThanEqual => ">=",
                    }
                );
                write!(w, " ");
                Dump::Node(left).dump(w, depth + 4);
                write!(w, " ");
                Dump::Node(right).dump(w, depth + 4);
            }
            Dump::Node(Node::If(cond, then, else_)) => {
                write!(w, "{}if ", pad(depth));
                Dump::Node(cond).dump(w, depth + 4);
                writeln!(w);
                writeln!(w, "{}then ", pad(depth));
                for then in then {
                    Dump::Node(then).dump(w, depth + 4);
                }
                writeln!(w, "{}else ", pad(depth));
                for else_ in else_ {
                    Dump::Node(else_).dump(w, depth + 4);
                }
            }
            Dump::Node(Node::Func(func)) => {
                write!(w, "\\{}", func);
            }
            Dump::Node(Node::Apply(func, nodes)) => {
                write!(w, "apply {} -> ", func);
                for (i, node) in nodes.iter().enumerate() {
                    if i > 0 {
                        write!(w, ", ");
                    }
                    Dump::Node(node).dump(w, depth + 4);
                }
            }
            Dump::Node(Node::Tuple(tuple)) => {
                write!(w, "(");
                for (i, tup) in tuple.iter().enumerate() {
                    if i > 0 {
                        write!(w, ", ");
                    }
                    Dump::Node(tup).dump(w, depth + 4);
                }
                write!(w, ")");
            }
            d => eprintln!("unknown: {:#?}", d),
        }
    }
}

fn pad(d: usize) -> String {
    ::std::iter::repeat(" ").take(d).collect()
}
