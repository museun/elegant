use crate::*;

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    LParen,
    RParen,
    Symbol(String),
    Number(i64),
    EOF,
}

pub struct Lexer {
    data: String,
    pos: usize,
    col: usize,
    row: usize,
    buf: Option<Token>,
}

impl Lexer {
    pub fn new<S: Into<String>>(input: S) -> Self {
        Self {
            data: input.into(),
            pos: 0,
            col: 1,
            row: 1,
            buf: None,
        }
    }

    pub fn unread(&mut self, tok: Token) -> Result<(), Error> {
        if self.buf.is_some() {
            return Err(Error::BufferFull(self.span()));
        }

        ::std::mem::replace(&mut self.buf, Some(tok));
        Ok(())
    }

    pub fn scan(&mut self) -> Result<Token, Error> {
        if let Some(tok) = self.buf.take() {
            return Ok(tok);
        }

        let mut iter = self.data[self.pos..].chars().peekable();

        macro_rules! advance {
            () => {
                iter.next();
                self.pos += 1;
                self.col += 1;
            };
        };

        while let Some(&c) = iter.peek() {
            if c.is_ascii_digit() {
                let (skip, n) = iter
                    .by_ref()
                    .take_while(char::is_ascii_digit)
                    .filter_map(|c| c.to_digit(10))
                    .fold((0usize, 0i64), |(s, a), d| (s + 1, 10 * a + i64::from(d)));
                self.pos += skip;
                self.col += skip;
                return Ok(Token::Number(n));
            }

            if Self::is_valid_in_symbol(c) {
                let mut acc = String::new();
                let mut s = c;
                while s.is_alphanumeric() || Self::is_valid_in_symbol(s) {
                    acc.push(s);
                    advance!();
                    s = match iter.peek() {
                        Some(&x) => x,
                        None => break,
                    }
                }
                return Ok(Token::Symbol(acc));
            }

            match c {
                '\n' => {
                    iter.next();
                    self.pos += 1;
                    self.col = 0;
                    self.row += 1;
                    continue;
                }
                ';' => {
                    advance!();
                    while let Some(c) = iter.next() {
                        self.pos += 1;
                        if c == '\n' {
                            break;
                        }
                    }
                    self.row += 1;
                    self.col = 0;
                }
                ' ' | '\r' => {
                    advance!();
                    continue;
                }
                '(' => {
                    advance!();
                    return Ok(Token::LParen);
                }
                ')' => {
                    advance!();
                    return Ok(Token::RParen);
                }
                _ => {
                    return Err(Error::UnknownToken(
                        c,
                        Span {
                            row: self.row,
                            col: self.col,
                        },
                    ));
                }
            }
        }
        Ok(Token::EOF)
    }

    pub fn span(&self) -> Span {
        Span {
            row: self.row,
            col: self.col,
        }
    }

    fn is_valid_in_symbol(c: char) -> bool {
        c.is_alphabetic() || match c {
            '+' | '-' | '*' | '/' | '#' | '<' | '>' | '=' => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex() {
        let input = "(define (add x)
        ;; a lambda
         (lambda (y) (+ x y)))
         
         (let ((one (add 1)))
            (one 40))
         ";

        let mut lexer = Lexer::new(input);
        loop {
            match lexer.scan() {
                Ok(Token::EOF) => break,
                Err(Error::UnknownToken(c, Span { row, col })) => {
                    eprintln!("unknown token: {} at {}:{}", c, row, col);
                    break;
                }
                tok => eprintln!("{:?}", tok),
            };
        }
    }
}
