#![allow(dead_code)]
use crate::lexer::*;
use crate::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Symbol(String),
    Number(i64),
    Bool(bool),
    List(Vec<Expr>),
    Tuple(Vec<Expr>),
    Func(String),

    Define(String, Vec<String>, Box<Expr>),
    Let(Vec<(String, Expr)>, Box<Expr>),
    Lambda(Vec<String>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Comparison(Comparison, Box<Expr>, Box<Expr>),
    Apply(Box<Expr>, Vec<Expr>),
    Program(Vec<Expr>, Box<Expr>),

    EOF,
}

pub struct Parser;

impl Parser {
    pub fn read(lexer: &mut Lexer) -> Result<Expr, Error> {
        Self::visit(&Self::expr(lexer)?)
    }

    fn list(lexer: &mut Lexer) -> Result<Vec<Expr>, Error> {
        let expr = Self::expr(lexer)?;
        match lexer.scan()? {
            Token::RParen => Ok(vec![expr]),
            tok => {
                lexer.unread(tok)?;
                let mut seq = Self::list(lexer)?;
                seq.insert(0, expr);
                Ok(seq)
            }
        }
    }

    fn args(args: &[Expr]) -> Result<Vec<String>, Error> {
        let mut names = vec![];
        for arg in args {
            match arg {
                Expr::Symbol(name) => names.push(name.clone()),
                _ => return Err(Error::InvalidArg),
            }
        }
        Ok(names)
    }

    fn expr(lexer: &mut Lexer) -> Result<Expr, Error> {
        match lexer.scan()? {
            Token::Symbol(s) => Ok(Expr::Symbol(s)),
            Token::Number(n) => Ok(Expr::Number(n)),
            Token::LParen => Ok(Expr::List(Self::list(lexer)?)),
            Token::RParen => Err(Error::UnmatchedParen(lexer.span())),
            Token::EOF => Ok(Expr::EOF),
        }
    }

    fn visit(expr: &Expr) -> Result<Expr, Error> {
        use self::Expr::*;
        match expr {
            Symbol(sym) => match sym.as_str() {
                "#f" | "#F" => Ok(Bool(false)),
                "#t" | "#T" => Ok(Bool(false)),
                _ => Ok(Symbol(sym.clone())),
            },
            List(list) => match list.as_slice() {
                [Symbol(k), List(defs), body] if k == "define" => {
                    let (name, args) = (&defs[0], &defs[1..]);
                    match name {
                        Symbol(name) => Ok(Define(
                            name.clone(),
                            Self::args(&args)?,
                            Box::new(Self::visit(body)?),
                        )),
                        _ => Err(Error::InvalidFunctionPrototype),
                    }
                }
                [Symbol(k), cond, then, else_] if k == "if" => Ok(If(
                    Box::new(Self::visit(cond)?),
                    Box::new(Self::visit(then)?),
                    Box::new(Self::visit(else_)?),
                )),
                [Symbol(k), List(bindings), body] if k == "let" => {
                    let mut nodes = vec![];
                    for pair in bindings {
                        let (key, val) = match pair {
                            // TODO probably should check the length of this
                            List(kv) => (kv[0].clone(), kv[1].clone()),
                            _ => return Err(Error::NonListInLetBinding),
                        };
                        let name = match key {
                            Symbol(k) => k,
                            _ => return Err(Error::LetBindingKeyNotSymbol),
                        };
                        nodes.push((name, Self::visit(&val)?))
                    }
                    Ok(Let(nodes, Box::new(Self::visit(&body)?)))
                }
                [Symbol(k), List(args), body] if k == "lambda" => {
                    Ok(Lambda(Self::args(&args)?, Box::new(Self::visit(body)?)))
                }
                [Symbol(k), ..] if k == "tuple" => {
                    let list = list
                        .iter()
                        .skip(1)
                        .map(Self::visit)
                        .collect::<Result<_, _>>()?;

                    Ok(Tuple(list))
                }
                [Symbol(cmp), left, right] if is_comp(cmp) => {
                    use self::Comparison as Comp;
                    let cmp = match cmp.as_ref() {
                        ">" => Comp::GreaterThan,
                        "<" => Comp::LessThan,
                        ">=" => Comp::GreaterThanEqual,
                        "<=" => Comp::LessThanEqual,
                        "=" => Comp::Equal,
                        e => unimplemented!("cmp '{}'", e),
                    };
                    Ok(Comparison(
                        cmp,
                        Box::new(Self::visit(left)?),
                        Box::new(Self::visit(right)?),
                    ))
                }
                // is this the right syntax?
                [f, ..] => {
                    let mut args = vec![];
                    for arg in &list[1..] {
                        args.push(Self::visit(arg)?);
                    }
                    Ok(Apply(Box::new(f.clone()), args))
                }
                _ => unimplemented!("unknown: {:?}", list),
            },

            expr => Ok(expr.clone()),
        }
    }
}
