use ethers_core::{types::Address, utils::to_checksum};
use forge_fmt::solang_ext::{Operator, OperatorComponents};
use solang_parser::pt::{
    self, Expression, FunctionAttribute, HexLiteral, Identifier, IdentifierPath, Loc, Parameter,
    StringLiteral, Type,
};
use std::{
    borrow::Cow,
    fmt::{self, Write},
};

/// Implements `fmt::Display` for `DisplayPT`.
#[derive(Debug)]
pub struct ImplDisplay<'a, T: ?Sized>(pub &'a T);

impl<'a, T: DisplayPT> fmt::Display for ImplDisplay<'a, T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        DisplayPT::fmt(self.0, f)
    }
}

/// Trait for formatting parse tree items, since solang doesn't implement `fmt::Display` for all of
/// them.
pub trait DisplayPT {
    /// Formats the parse tree item into the given writer.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;

    /// Size hint used to allocate memory before formatting.
    fn size_hint(&self) -> usize {
        0
    }

    /// Creates a `fmt::Display` object.
    fn display(&self) -> ImplDisplay<'_, Self> {
        ImplDisplay(self)
    }
}

impl<T: DisplayPT> DisplayPT for &T {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (*self).fmt(f)
    }

    fn size_hint(&self) -> usize {
        (*self).size_hint()
    }

    fn display(&self) -> ImplDisplay<'_, Self> {
        ImplDisplay(self)
    }
}

macro_rules! spaced {
    ($out:expr, Some($e:expr)) => {
        $out.write_char(' ').and_then(|_| $e.fmt($out))
    };
    ($out:expr, $opt:expr) => {
        match $opt {
            Some(t) => spaced!($out, Some(t)),
            None => Ok(()),
        }
    };
}

macro_rules! wrapped {
    ($out:expr, $start:literal, do $e:expr, $end:literal) => {{
        $out.write_char($start)?;
        $e;
        $out.write_char($end)
    }};

    ($out:expr, $start:literal, $e:expr, $end:literal) => {{
        $out.write_char($start)?;
        $e.fmt($out)?;
        $out.write_char($end)
    }};
}

impl DisplayPT for Expression {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Type(_, ty) => ty.fmt(out),

            Expression::Variable(ident) => ident.fmt(out),

            Expression::ArrayLiteral(_, exprs) => {
                // [{exprs, }]
                wrapped!(out, '[', do join(exprs.iter(), ", ", out)?, ']')
            }
            Expression::ArraySubscript(_, expr1, expr2) => {
                // {expr1}[{expr2}]
                expr1.fmt(out)?;
                wrapped!(out, '[', expr2, ']')
            }

            Expression::MemberAccess(_, expr, ident) => {
                // {ident}.{expr}
                ident.fmt(out)?;
                out.write_char('.')?;
                expr.fmt(out)
            }

            Expression::Parenthesis(_, expr) => {
                // ({expr})
                wrapped!(out, '(', expr, ')')
            }

            Expression::StringLiteral(vals) => vals.fmt(out),
            Expression::HexLiteral(vals) => vals.fmt(out),
            Expression::BoolLiteral(_, bool) => {
                let s = if *bool { "true" } else { "false" };
                out.write_str(s)
            }
            Expression::HexNumberLiteral(_, val, unit) => {
                // ref: https://docs.soliditylang.org/en/latest/types.html?highlight=address%20literal#address-literals
                if val.len() == 42 {
                    let checksum = to_checksum(&val.parse().unwrap(), None);
                    if checksum == *val {
                        out.write_str(&checksum)
                    } else {
                        out.write_str(val)
                    }
                } else {
                    out.write_str(val)
                }?;
                spaced!(out, unit)
            }
            Expression::NumberLiteral(_, val, exp, unit) => {
                let val = rm_underscores(val);
                out.write_str(&val)?;
                if !exp.is_empty() {
                    out.write_char('e')?;
                    let exp = rm_underscores(exp);
                    out.write_str(&exp)?;
                }
                spaced!(out, unit)
            }
            Expression::RationalNumberLiteral(_, val, fraction, exp, unit) => {
                let val = rm_underscores(val);
                out.write_str(&val)?;

                let mut fraction = fraction.trim_end_matches('0');
                if fraction.is_empty() {
                    fraction = "0"
                }
                out.write_char('.')?;
                out.write_str(fraction)?;

                if !exp.is_empty() {
                    out.write_char('e')?;
                    let exp = rm_underscores(exp);
                    out.write_str(&exp)?;
                }
                spaced!(out, unit)
            }

            Expression::FunctionCall(_, expr, exprs) => {
                // {expr}({exprs, })
                expr.fmt(out)?;
                wrapped!(out, '(', do join(exprs.iter(), ", ", out)?, ')')
            }

            Expression::PreIncrement(..) |
            Expression::PostIncrement(..) |
            Expression::PreDecrement(..) |
            Expression::PostDecrement(..) |
            Expression::Not(..) |
            Expression::Complement(..) |
            Expression::UnaryPlus(..) |
            Expression::Add(..) |
            Expression::Negate(..) |
            Expression::Subtract(..) |
            Expression::Power(..) |
            Expression::Multiply(..) |
            Expression::Divide(..) |
            Expression::Modulo(..) |
            Expression::ShiftLeft(..) |
            Expression::ShiftRight(..) |
            Expression::BitwiseAnd(..) |
            Expression::BitwiseXor(..) |
            Expression::BitwiseOr(..) |
            Expression::Less(..) |
            Expression::More(..) |
            Expression::LessEqual(..) |
            Expression::MoreEqual(..) |
            Expression::And(..) |
            Expression::Or(..) |
            Expression::Equal(..) |
            Expression::NotEqual(..) => {
                let (left, right) = self.components();
                if let Some(left) = left {
                    left.fmt(out)?;
                    if self.has_space_around() {
                        out.write_char(' ')?;
                    }
                }
                out.write_str(self.operator().unwrap())?;
                spaced!(out, right)
            }

            item => {
                todo!("formatting pt expression {item:?}")
            }
        }
    }

    fn size_hint(&self) -> usize {
        match self {
            Expression::Type(_, ty) => ty.size_hint(),
            Expression::Variable(ident) => ident.size_hint(),
            Expression::ArrayLiteral(_, exprs) => exprs.size_hint(),
            Expression::ArraySubscript(_, expr1, expr2) => {
                expr1.size_hint() + expr2.size_hint() + 2
            }
            Expression::MemberAccess(_, expr, ident) => ident.size_hint() + expr.size_hint() + 1,
            Expression::Parenthesis(_, expr) => expr.size_hint() + 2,
            Expression::StringLiteral(vals) => vals.size_hint(),
            Expression::HexLiteral(vals) => vals.size_hint(),
            Expression::BoolLiteral(_, bool) => 4 + (!bool as usize),
            Expression::HexNumberLiteral(_, val, unit) => val.len() + unit.size_hint() + 1,
            Expression::NumberLiteral(_, val, exp, unit) => {
                val.len() + exp.len() + unit.size_hint() + 2
            }
            Expression::RationalNumberLiteral(_, val, fraction, exp, unit) => {
                val.len() + fraction.len() + exp.len() + unit.size_hint() + 3
            }
            Expression::FunctionCall(_, expr, exprs) => expr.size_hint() + exprs.size_hint() + 2,
            Expression::PreIncrement(..) |
            Expression::PostIncrement(..) |
            Expression::PreDecrement(..) |
            Expression::PostDecrement(..) |
            Expression::Not(..) |
            Expression::Complement(..) |
            Expression::UnaryPlus(..) |
            Expression::Add(..) |
            Expression::Negate(..) |
            Expression::Subtract(..) |
            Expression::Power(..) |
            Expression::Multiply(..) |
            Expression::Divide(..) |
            Expression::Modulo(..) |
            Expression::ShiftLeft(..) |
            Expression::ShiftRight(..) |
            Expression::BitwiseAnd(..) |
            Expression::BitwiseXor(..) |
            Expression::BitwiseOr(..) |
            Expression::Less(..) |
            Expression::More(..) |
            Expression::LessEqual(..) |
            Expression::MoreEqual(..) |
            Expression::And(..) |
            Expression::Or(..) |
            Expression::Equal(..) |
            Expression::NotEqual(..) => {
                let (left, right) = self.components();
                left.size_hint() + right.size_hint() + 5
            }

            item => {
                todo!("formatting pt expression {item:?}")
            }
        }
    }
}

impl DisplayPT for Type {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Address => out.write_str("address"),
            Type::AddressPayable => out.write_str("address payable"),
            Type::Payable => out.write_str("payable"),
            Type::Bool => out.write_str("bool"),
            Type::String => out.write_str("string"),
            Type::Rational => out.write_str("rational"),
            Type::DynamicBytes => out.write_str("bytes"),
            Type::Bytes(n) => write!(out, "bytes{n}"),
            Type::Int(n) => write!(out, "int{n}"),
            Type::Uint(n) => write!(out, "uint{n}"),
            Type::Mapping { key, key_name, value, value_name, .. } => {
                // mapping({key} => {value})
                out.write_str("mapping(")?;
                (key, key_name).fmt(out)?;
                out.write_str(" => ")?;
                (value, value_name).fmt(out)?;
                out.write_char(')')
            }
            Type::Function { params, attributes, returns } => {
                // function ({params}){attributes}{returns}
                out.write_str("function (")?;
                params.fmt(out)?;
                out.write_char(')')?;
                if !attributes.is_empty() {
                    spaced!(out, Some(attributes))?;
                }
                if let Some((returns, _attrs)) = returns.as_ref() {
                    if !returns.is_empty() {
                        out.write_str(" returns(")?;
                        returns.fmt(out)?;
                        out.write_char(')')?;
                    }
                }
                Ok(())
            }
        }
    }

    fn size_hint(&self) -> usize {
        todo!()
    }
}

impl DisplayPT for Parameter {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.ty.fmt(out)?;
        spaced!(out, &self.storage)?;
        spaced!(out, &self.name)
    }

    fn size_hint(&self) -> usize {
        self.ty.size_hint() + self.storage.size_hint() + self.name.size_hint() + 2
    }
}

impl DisplayPT for FunctionAttribute {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Mutability(mutability) => mutability.fmt(out),
            Self::Visibility(visibility) => visibility.fmt(out),
            Self::Virtual(_) => out.write_str("virtual"),
            Self::Immutable(_) => out.write_str("immutable"),
            Self::Override(_, idents) => {
                // override({idents, })
                out.write_str("override")?;
                if !idents.is_empty() {
                    wrapped!(out, '(', do join(idents.iter(), ", ", out)?, ')')?;
                }
                Ok(())
            }
            Self::BaseOrModifier(_, base) => base.name.fmt(out),
            Self::Error(_) => unreachable!(),
        }
    }

    fn size_hint(&self) -> usize {
        todo!()
    }
}

impl DisplayPT for IdentifierPath {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        join(self.identifiers.iter(), ".", out)
    }

    fn size_hint(&self) -> usize {
        iter_size_hint(self.identifiers.iter(), 1)
    }
}

impl DisplayPT for Identifier {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        out.write_str(&self.name)
    }

    fn size_hint(&self) -> usize {
        self.name.len()
    }
}

impl DisplayPT for StringLiteral {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.unicode {
            out.write_str("unicode")?;
        }
        out.write_char('"')?;
        out.write_str(&self.string)?;
        out.write_char('"')
    }

    fn size_hint(&self) -> usize {
        self.string.len() + 2 + self.unicode as usize * 7
    }
}

impl DisplayPT for HexLiteral {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        // hex"{self.hex}"
        out.write_str("hex\"")?;
        out.write_str(&self.hex)?;
        out.write_char('"')
    }

    fn size_hint(&self) -> usize {
        self.hex.len() + 5
    }
}

impl DisplayPT for Address {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = to_checksum(self, None);
        out.write_str(&s)
    }

    fn size_hint(&self) -> usize {
        42
    }
}

impl DisplayPT for Vec<(Loc, Option<Parameter>)> {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        join(self.iter().map(|(_, p)| p), ", ", out)
    }

    fn size_hint(&self) -> usize {
        iter_size_hint(self.iter().map(|(_, p)| p), 2)
    }
}

// generic impls
// tuples and Vecs: .join(" ")

impl<T: DisplayPT> DisplayPT for Vec<T> {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        join(self.iter(), " ", out)
    }

    fn size_hint(&self) -> usize {
        iter_size_hint(self.iter(), 1)
    }
}

impl<T: DisplayPT> DisplayPT for Option<T> {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Some(ref t) => t.fmt(out),
            None => Ok(()),
        }
    }

    fn size_hint(&self) -> usize {
        self.as_ref().map_or(0, |t| t.size_hint())
    }
}

impl<T: DisplayPT + ?Sized> DisplayPT for Box<T> {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        (&**self).fmt(out)
    }

    fn size_hint(&self) -> usize {
        (&**self).size_hint()
    }
}

impl<T: DisplayPT + ToOwned + ?Sized> DisplayPT for Cow<'_, T> {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        (&**self).fmt(out)
    }

    fn size_hint(&self) -> usize {
        (&**self).size_hint()
    }
}

impl<A: DisplayPT, B: DisplayPT> DisplayPT for (A, B) {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(out)?;
        out.write_char(' ')?;
        self.1.fmt(out)
    }

    fn size_hint(&self) -> usize {
        self.0.size_hint() + self.1.size_hint() + 1
    }
}

impl<A: DisplayPT, B: DisplayPT, C: DisplayPT> DisplayPT for (A, B, C) {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(out)?;
        out.write_char(' ')?;
        self.1.fmt(out)?;
        out.write_char(' ')?;
        self.2.fmt(out)
    }

    fn size_hint(&self) -> usize {
        self.0.size_hint() + self.1.size_hint() + self.2.size_hint()
    }
}

macro_rules! impl_for_display {
    ($($t:ty : $sz:literal),+ $(,)?) => {
        $(
            impl DisplayPT for $t {
                fn fmt(&self,out: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(out, "{}", self)
                }

                fn size_hint(&self) -> usize {
                    $sz
                }
            }
        )+
    };
}

impl_for_display! {
    pt::StorageLocation: 8,
    pt::Mutability: 7,
    pt::Visibility: 8,
}

fn rm_underscores(s: &String) -> Cow<'_, str> {
    if s.is_empty() {
        "0".into()
    } else if s.contains('_') {
        let mut s = s.clone();
        s.retain(|c| c != '_');
        s.into()
    } else {
        Cow::Borrowed(s)
    }
}

fn join<T, I>(iter: I, s: &str, out: &mut fmt::Formatter<'_>) -> fmt::Result
where
    T: DisplayPT,
    I: Iterator<Item = T> + ExactSizeIterator,
{
    let last = iter.len();
    for (i, item) in iter.enumerate() {
        item.fmt(out)?;
        if i < last {
            out.write_str(s)?;
        }
    }
    Ok(())
}

fn iter_size_hint<T, I>(iter: I, sep_size: usize) -> usize
where
    T: DisplayPT,
    I: Iterator<Item = T>,
{
    iter.map(|item| item.size_hint() + sep_size).sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_fmt(s: &str) {
        let source = format!(
            "\
pragma solidity ^0.8.19;
contract A {{
    function a() {{
        {s};
    }}
}}"
        );
        let (parsed, _) = solang_parser::parse(&source, 0).unwrap();

        let contract = parsed
            .0
            .into_iter()
            .find_map(|x| match x {
                pt::SourceUnitPart::ContractDefinition(c) => Some(c),
                _ => None,
            })
            .unwrap();

        let function = contract
            .parts
            .into_iter()
            .find_map(|x| match x {
                pt::ContractPart::FunctionDefinition(f) => Some(f),
                _ => None,
            })
            .unwrap();

        let body_statement = match function.body.unwrap() {
            pt::Statement::Block { statements, .. } => statements.into_iter().next().unwrap(),
            _ => panic!(),
        };

        let expression = match body_statement {
            pt::Statement::Expression(_, e) => e,
            _ => panic!(),
        };

        assert_eq!(expression.display().to_string(), s);
    }

    #[test]
    fn exprs() {
        static TEST_CASES: &[&str] = &[
            "++a",
            "a++",
            "--a",
            "a--",
            "a += 1",
            "a -= 1",
            "a = a | 1",
            "false",
            "true",
            "\"a\"",
            "unicode\"e\"",
        ];
        for s in TEST_CASES {
            assert_fmt(s);
        }
    }
}
