use bigdecimal::BigDecimal;
use num::{BigInt, BigRational, Complex};

use std::ops::Range;

pub type SourceSpan = Range<usize>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: SourceSpan,
    pub origin: Option<NodeId>,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: SourceSpan) -> Self {
        Self {
            node,
            span,
            origin: None,
        }
    }

    pub fn with_origin(mut self, origin: NodeId) -> Self {
        self.origin = Some(origin);
        self
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            node: f(self.node),
            span: self.span,
            origin: self.origin,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Identifier(String),
    Integer(BigInt),
    Decimal(BigDecimal),
    Real(BigInt, BigInt),
    ExactComplex(Complex<BigRational>),
    Complex(Complex<BigDecimal>),
    String(String),
    Boolean(bool),
    Character(char),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Datum {
    Atom(Atom),
    List(Vec<Spanned<Datum>>),
    DottedList(Vec<Spanned<Datum>>, Box<Spanned<Datum>>),
    Vector(Vec<Spanned<Datum>>),
    Quote(Box<Spanned<Datum>>),
    Quasiquote(Box<Spanned<Datum>>),
    Unquote(Box<Spanned<Datum>>),
    UnquoteSplicing(Box<Spanned<Datum>>),
}

impl Datum {
    pub fn identifier(name: impl Into<String>) -> Self {
        Self::Atom(Atom::Identifier(name.into()))
    }

    pub fn boolean(value: bool) -> Self {
        Self::Atom(Atom::Boolean(value))
    }
}

#[cfg(test)]
mod tests {
    use super::{Atom, Datum, NodeId, Spanned};

    #[test]
    fn spanned_nodes_preserve_source_metadata_when_mapped() {
        let node = Spanned::new(Datum::boolean(true), 0..2).with_origin(NodeId(7));
        let mapped = node.map(|datum| match datum {
            Datum::Atom(Atom::Boolean(value)) => value,
            _ => false,
        });

        assert!(mapped.node);
        assert_eq!(mapped.span, 0..2);
        assert_eq!(mapped.origin, Some(NodeId(7)));
    }

    #[test]
    fn quoted_lists_remain_data() {
        let symbol = Spanned::new(Datum::identifier("and"), 2..5);
        let list = Spanned::new(Datum::List(vec![symbol]), 1..6);
        let quoted = Spanned::new(Datum::Quote(Box::new(list)), 0..6);

        assert!(matches!(quoted.node, Datum::Quote(_)));
    }
}
