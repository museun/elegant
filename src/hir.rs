use crate::anf::Node;
use crate::*;

use std::fmt;
use std::io::prelude::*;

#[rustfmt::skip]
#[derive(Clone, Copy, Debug)]
pub enum Reg {
    AL,

    RAX, RBX, RCX, RDX, 
    RDI, RSI, R8,  R9,

    R10, R11, R12, R13, R14, R15
}

// linux first

// also need to do some pointer tagging to save the primitive type
// probably just put it in the msb
// so something like 0xFFFF_FFFF_FFFF_FFFF and 0x7FFF_FFFF_FFFF_FFFF could work
// as masks on the pointers

// probably need a register to "point to the heap"

// need to have a list of callee saved registers
// need to have a list of caller saved registers
// some alignment construct to ensure 16-bit (stack) alignment
// stack-ordered registers
// available registers for allocation

// 6 at first and then reuse them?
// or reuse them from the start

// simple at first

// R11 is going to be used to point to the heap
const CALLEE_SAVED: [Reg; 5] = [Reg::RBX, Reg::R12, Reg::R13, Reg::R14, Reg::R15];
const CALLER_SAVED: [Reg; 7] = [
    Reg::RDX,
    Reg::RCX,
    Reg::RSI,
    Reg::RDI,
    Reg::R8,
    Reg::R9,
    Reg::R10,
];

// only 6 arguments allowed at this point
const REGS: [Reg; 6] = [Reg::RBX, Reg::R12, Reg::R13, Reg::R14, Reg::R15, Reg::R10];

const TRUE: u64 = 0xFFFF_FFFF_FFFF_FFFF;
const FALSE: u64 = 0x7FFF_FFFF_FFFF_FFFF;

const FREE_PTR: &str = "free_ptr";

#[derive(Debug)]
pub enum Arg {
    Reg(Reg),
    Imm(u64),
    Offset(Reg, i64), // *p rel
    Global(String),
    Func(String), // closure name
    Var(String),  // a define?
}

impl fmt::Display for Arg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Arg::*;
        match self {
            Reg(reg) => write!(f, "%{:?}", reg),
            Imm(imm) => write!(f, "#{:#0X}", imm),
            Offset(reg, off) => write!(f, "[{:?}-{:#0X}]", reg, off),
            Global(s) => write!(f, "@{}", s),
            Func(s) => write!(f, "\\{}", s),
            Var(s) => write!(f, "${}", s),
        }
    }
}

#[derive(Debug)]
pub enum HIR {
    Mov(Arg, Arg),
    Add(Arg, Arg),
    Sub(Arg, Arg),
    Neg(Arg),
    Cmp(Arg, Arg),
    And(Arg, Arg),
    Push(Reg),
    Pop(Reg),

    Set(Arg, Comparison),
    Movxz(Arg, Arg), // TODO less-intel-name

    If {
        cond: Box<HIR>,
        then: Vec<HIR>,
        else_: Vec<HIR>,
    },

    Eq(Arg, Arg),

    Label(String), // label
    Je(String),    // label
    Jne(String),   // label
    Jmp(String),   // label
    //
    Call(Arg),
    Define {
        binding: String,
        vars: Vec<String>,
        insts: Vec<HIR>,
    },
    Program {
        defines: Vec<HIR>,
        vars: Vec<String>,
        insts: Vec<HIR>,
    },
}

impl HIR {
    pub fn print<W: Write>(&self, w: &mut W) {
        self.dump(w, 0)
    }

    fn pad(d: usize) -> String {
        ::std::iter::repeat(" ").take(d).collect()
    }

    fn dump<W: Write>(&self, w: &mut W, depth: usize) {
        use self::HIR::*;
        match self {
            Mov(dst, src) => {
                writeln!(w, "{}mov {}, {}", Self::pad(depth), dst, src);
            }
            Add(dst, src) => {
                writeln!(w, "{}add {}, {}", Self::pad(depth), dst, src);
            }
            Sub(dst, src) => {
                writeln!(w, "{}sub {}, {}", Self::pad(depth), dst, src);
            }
            Neg(arg) => {
                writeln!(w, "{}neg {}", Self::pad(depth), arg);
            }
            Cmp(dst, src) => {
                writeln!(w, "{}cmp {}, {}", Self::pad(depth), dst, src);
            }
            And(dst, src) => {
                writeln!(w, "{}and {}, {}", Self::pad(depth), dst, src);
            }
            Push(reg) => {
                writeln!(w, "{}push %{:?}", Self::pad(depth), reg);
            }
            Pop(reg) => {
                writeln!(w, "{}pop %{:?}", Self::pad(depth), reg);
            }
            Set(dst, cmp) => {
                writeln!(w, "{}set {}, {}", Self::pad(depth), dst, cmp);
            }
            Movxz(dst, src) => {
                writeln!(w, "{}movzx {}, {}", Self::pad(depth), dst, src);
            }
            If { cond, then, else_ } => {
                writeln!(w, "{}if ", Self::pad(depth));
                (*cond).dump(w, depth + 4);
                writeln!(w, "{}then ", Self::pad(depth));
                for then in then {
                    then.dump(w, depth + 4);
                }
                writeln!(w, "{}else ", Self::pad(depth));
                for else_ in else_ {
                    else_.dump(w, depth + 4);
                }
            }
            Eq(dst, src) => {
                writeln!(w, "{}eq {}, {}", Self::pad(depth), dst, src);
            }
            Label(label) => {
                writeln!(w, "{}label: {}", Self::pad(depth), label);
            }
            Je(label) => {
                writeln!(w, "{}jn: {}", Self::pad(depth), label);
            }
            Jne(label) => {
                writeln!(w, "{}jne: {}", Self::pad(depth), label);
            }
            Jmp(label) => {
                writeln!(w, "{}jmp: {}", Self::pad(depth), label);
            }
            Call(arg) => {
                writeln!(w, "{}call {}", Self::pad(depth), arg);
            }
            Define {
                binding,
                vars,
                insts,
            } => {
                writeln!(w, "{}define: {}", Self::pad(depth), binding);
                let depth = depth + 4;
                writeln!(w, "{}vars:", Self::pad(depth));
                for var in vars {
                    writeln!(w, "{}{}", Self::pad(depth + 4), var);
                }
                writeln!(w);
                writeln!(w, "{}insts:", Self::pad(depth));
                for inst in insts {
                    inst.dump(w, depth + 4);
                }
                writeln!(w);
            }
            Program {
                defines,
                vars,
                insts,
            } => {
                writeln!(w, "{}program:", Self::pad(depth));
                let depth = depth + 4;
                writeln!(w, "{}defines:", Self::pad(depth));
                for inst in defines {
                    inst.dump(w, depth + 4);
                }
                writeln!(w);
                writeln!(w, "{}vars:", Self::pad(depth));
                for var in vars {
                    writeln!(w, "{}{}", Self::pad(depth + 4), var);
                }
                writeln!(w);
                writeln!(w, "{}insts:", Self::pad(depth));
                for inst in insts {
                    inst.dump(w, depth + 4);
                }
                writeln!(w);
            }
        };
    }
}

fn convert_arg(arg: Node) -> Arg {
    use self::Node::*;
    match arg {
        Symbol(name) => Arg::Var(name.clone()),
        Func(name) => Arg::Func(name.clone()),
        Number(num) => Arg::Imm((num << 1) as u64),
        Bool(true) => Arg::Imm(TRUE),
        Bool(false) => Arg::Imm(FALSE),
        e => unreachable!("{:?} can't be used in the arg position", e),
    }
}

pub fn convert(anf: ANF) -> HIR {
    use {self::Arg::*, self::Reg::*, self::HIR::*};
    match anf {
        ANF::Program(defs, assigns, vars) => {
            let mut defines = vec![];

            for def in defs {
                defines.push(convert(def))
            }

            let mut insts = vec![];
            for assign in assigns {
                insts.append(&mut convert_node(assign))
            }

            Program {
                defines,
                vars,
                insts,
            }
        }

        ANF::Define(name, args, assigns, mut vars) => {
            const ORDER: [self::Reg; 6] = [RDI, RSI, RDX, RCX, R8, R9];
            // TODO stack spillage here?
            let mut insts = vec![];
            for (i, arg) in args.iter().enumerate() {
                insts.push(Mov(Var(arg.clone()), Reg(ORDER[i].clone())));
            }
            for assign in assigns {
                insts.append(&mut convert_node(assign));
            }
            vars.extend_from_slice(&args);
            HIR::Define {
                binding: name,
                vars,
                insts,
            }
        }
        _ => unimplemented!(),
    }
}

fn convert_node(node: Node) -> Vec<HIR> {
    use {self::Arg::*, self::Node::*, self::Reg::*, self::HIR::*};
    match node {
        Assign(dest, expr) => match *expr {
            Bool(true) => vec![Mov(Var(dest), Imm(TRUE))],
            Bool(false) => vec![Mov(Var(dest), Imm(FALSE))],
            Number(num) => vec![Mov(Var(dest), Imm((num << 1) as u64))],
            Symbol(name) => vec![Mov(Var(dest), Var(name))],
            Node::Func(name) => vec![Mov(Var(dest), Var(name))],

            Primitive(name, mut args) => match name.as_str() {
                "+" => {
                    let (arg1, arg2) = (args.shift().unwrap(), args.shift().unwrap());
                    vec![
                        Mov(Var(dest.clone()), convert_arg(arg1)),
                        Add(Var(dest), convert_arg(arg2)),
                    ]
                }
                "-" => {
                    let arg = args.shift().unwrap();
                    vec![Mov(Var(dest.clone()), convert_arg(arg)), Neg(Var(dest))]
                }
                "tuple-ref" => {
                    let (tuple, index) = (args.shift().unwrap(), args.shift().unwrap());
                    let index = match index {
                        Number(n) => n,
                        n => panic!("not a number: {:?}", n),
                    };

                    vec![
                        Mov(Reg(R11), convert_arg(tuple)),
                        Sub(Reg(R11), Imm(1)),
                        Mov(Var(dest), Offset(R11, 8 * (index + 1))),
                    ]
                }
                e => unreachable!("invalid primitive: {}", e),
            },

            Apply(name, args) => {
                // TODO spillage should happen here
                // tbh this should be done in the register allocater

                let len = args.len();
                debug_assert!(len <= 6);
                let mut insts = vec![];

                // push registers onto the stack
                for &r in CALLER_SAVED.iter().take(len) {
                    insts.push(Push(r))
                }

                // this shouldn't exist
                const ORDER: [self::Reg; 6] = [RDI, RSI, RDX, RCX, R8, R9];

                for (i, arg) in args.into_iter().map(convert_arg).enumerate() {
                    insts.push(Mov(Reg(ORDER[i]), arg))
                }

                insts.push(Call(Arg::Func(name.clone())));

                // pop them from the stack
                for &r in CALLER_SAVED.iter().rev().take(len) {
                    insts.push(Pop(r))
                }

                insts.push(Mov(Var(dest), Reg(RAX)));
                insts
            }

            Comparison(cmp, left, right) => {
                // TODO be smarter
                let false_ = next_name(Named::Label("false".into()));
                let done = next_name(Named::Label("done".into()));

                // need to do some ensuring that we have legitimate values

                let left = convert_arg(*left);
                let right = convert_arg(*right);

                vec![
                    Cmp(left, right),
                    Set(Reg(AL), cmp),
                    Movxz(Reg(RAX), Reg(AL)),
                    Cmp(Reg(RAX), Imm(0)),
                    Je(false_.clone()),
                    Mov(Var(dest.clone()), Imm(TRUE)),
                    Jmp(done.clone()),
                    Label(false_),
                    Mov(Var(dest), Imm(FALSE)),
                    Label(done),
                ]
            }

            Tuple(tuple) => {
                // (count placed in the first word)
                let len = tuple.len() + 1;
                let total = 8 * (len + (len % 2)); // TODO: have an alignment utility function
                let mut insts = vec![
                    Mov(Var(dest.clone()), Global(FREE_PTR.to_string())),
                    Add(Global(FREE_PTR.to_string()), Imm(total as u64)),
                    Mov(Reg(R11), Var(dest.clone())),
                ];
                // store count in first word

                insts.push(Mov(Offset(R11, 0), Imm(tuple.len() as u64)));
                // alignment
                for (i, tup) in tuple.iter().enumerate() {
                    insts.push(Mov(
                        Offset(R11, 8 * (i + 1) as i64),
                        convert_arg(tup.clone()),
                    ));
                }

                // just to tag that this is a tuple
                // TODO don't waste this space
                insts.push(Add(Var(dest), Imm(1)));
                insts
            }

            _ => unimplemented!(),
        },

        Return(expr) => {
            // TODO align the stack
            let val = convert_arg(*expr);
            vec![Mov(Reg(RAX), val)]
        }

        Node::If(cond, then, else_) => {
            let (left, right) = match *cond {
                Number(_) | Symbol(_) => (convert_arg(*cond), Imm(TRUE)),
                e => panic!("{:?} does not implement Eq", e),
            };
            let mut thens = vec![];
            for expr in then {
                thens.append(&mut convert_node(expr))
            }

            let mut elses = vec![];
            for expr in else_ {
                elses.append(&mut convert_node(expr))
            }

            vec![HIR::If {
                cond: Box::new(Eq(left, right)),
                then: thens,
                else_: elses,
            }]
        }

        _ => unimplemented!(),
    }
}

trait VecExt<T> {
    fn shift(&mut self) -> Option<T>;
}

impl<T> VecExt<T> for Vec<T> {
    fn shift(&mut self) -> Option<T> {
        if self.is_empty() {
            return None;
        }

        Some(self.remove(0))
    }
}
