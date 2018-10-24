#![feature(slice_patterns)]
//#![allow(dead_code, unused_variables)]

use std::collections::HashMap;

#[derive(Debug)]
pub struct Span {
    pub col: usize,
    pub row: usize,
}

#[derive(Debug)]
pub enum Error {
    UnknownToken(char, Span),
    UnmatchedParen(Span),
    BufferFull(Span),
    InvalidFunctionPrototype,
    InvalidArg,
    NonListInLetBinding,
    LetBindingKeyNotSymbol,
}

mod anf;
mod comparison;
mod lexer;
mod parser;

mod closure;
mod lexical;

mod hir;

use self::anf::*;
use self::closure::*;
use self::comparison::*;
use self::lexer::*;
use self::lexical::*;
use self::parser::*;

fn main() {
    let input = &[
        "(define (cons a b) (tuple a b))",
        "(define (car t) (tuple-ref t 0))",
        "(define (cdr t) (tuple-ref t 1))",
        "(define (sum l) (if (= l #f) 0 (+ (car l) (sum (cdr l)))))",
        "(define (length l) (if (= l #f) 0 (+ 1 (length (cdr l)))))",
        "(let ((my-list (cons 1 (cons 2 (cons 3 #f))))) (sum (my-list)))",
    ];

    let input = input.iter().fold(String::new(), |mut acc, c| {
        acc.push_str(c);
        acc.push('\n');
        acc
    });

    //     let input = r#"
    // (define (fib n)
    //   ;; Get n-th Fibonacci number
    //   (if (< n 2)
    //       1
    //       (+ (fib (+ n (- 2)))
    //          (fib (+ n (- 1))))))
    // (fib 10)
    // "#;

    eprintln!("{}", input);

    let mut env = vec![];
    let mut lexer = Lexer::new(input);
    loop {
        match Parser::read(&mut lexer) {
            Ok(Expr::EOF) => break,
            Ok(expr) => env.push(expr),
            Err(err) => {
                eprintln!("error parsing: {:?}", err);
                ::std::process::exit(1);
            }
        }
    }

    let mut lexical = Lexical::new();
    let program = lexical.visit(Expr::Program(
        env[..env.len() - 1].to_vec(),
        Box::new(env[env.len() - 1].clone()),
    ));

    let (closure, _) = Closure::convert(program);
    let anf = ANF::flatten(closure);
    anf.print(&mut ::std::io::stdout());

    let hir = hir::convert(anf);
    hir.print(&mut ::std::io::stdout());
}

/// Namer only works from 1 thread
struct Namer(HashMap<Named, usize>);

impl Namer {
    pub fn new() -> Self {
        Namer(HashMap::new())
    }

    pub fn next(&mut self, name: Named) -> String {
        let s = name.name();
        let n = self.0.entry(name).and_modify(|e| *e += 1).or_insert(0);
        format!("{}{}", s, n)
    }
}

#[derive(PartialEq, PartialOrd, Ord, Eq, Hash, Clone)]
pub enum Named {
    If,
    Neg,
    Plus,
    Ref,
    Tuple,
    Apply,
    Lambda,
    Cmp(Comparison),
    Let(String),
    Define(String),
    Arg(String),

    // for the IRs
    Label(String),
}

impl Named {
    pub fn name(&self) -> String {
        use self::Named::*;
        match self {
            If => "if".into(),
            Neg => "neg".into(),
            Plus => "plus".into(),
            Ref => "ref".into(),
            Tuple => "tuple".into(),
            Apply => "apply".into(),
            Lambda => "lambda".into(),
            Cmp(comp) => format!("cmp-{}", comp),
            Let(name) => format!("let-{}", name),
            Define(name) => format!("def-{}", name),
            Arg(name) => format!("arg-{}", name),
            Label(label) => format!("L{}", label),
        }
    }
}

use std::cell::RefCell;
thread_local!{
    static CONTEXT: RefCell<Option<Namer>> = RefCell::new(None);
}

pub fn next_name(name: Named) -> String {
    CONTEXT.with(|ctx| {
        let ctx = &mut *ctx.borrow_mut();
        match ctx {
            Some(ctx) => ctx.next(name),
            None => {
                // need to swap the None with a Some to act as a cache
                ::std::mem::replace(ctx, Some(Namer::new()));
                ctx.as_mut().unwrap().next(name)
            }
        }
    })
}

// use std::sync::atomic::{AtomicUsize, Ordering};
// static NAME_COUNTER: AtomicUsize = AtomicUsize::new(0);
// fn next_name(s: &str) -> String {
//     let n = NAME_COUNTER.fetch_add(1, Ordering::Relaxed);
//     format!("{}{}", s, n)
// }
