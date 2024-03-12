#![allow(dead_code, unused)]
#![forbid(unused_must_use)]

use std::collections::HashMap;

use chumsky::prelude::*;

#[derive(Debug, Clone)]
enum Expression<'src> {
    Number(f64),
    Identifier(&'src str),
    Let(&'src str, Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    End,
}

type BinaryOperator<'src> = fn(Expression<'src>, Expression<'src>) -> Expression<'src>;

fn parser<'src>() -> impl Parser<'src, &'src str, Expression<'src>, extra::Err<Simple<'src, char>>>
{
    let fraction = just('.').ignore_then(text::digits(10));
    let number = text::int(10)
        .then(fraction.or_not())
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Expression::Number);
    let identifier = text::ident().map(Expression::Identifier);
    let value = number.or(identifier).padded();
    let unary_operator = choice((just('-').to(-1.0f64), just('+').to(1.0f64)))
        .repeated()
        .foldr(value, |a, b| {
            Expression::Mul(Box::new(Expression::Number(a)), Box::new(b))
        });

    let x = choice((
        just('+').to((|a, b| Expression::Add(Box::new(a), Box::new(b))) as BinaryOperator<'src>),
        just('-').to((|a, b| Expression::Sub(Box::new(a), Box::new(b))) as BinaryOperator<'src>),
    ));
    let sum = unary_operator
        .then(x)
        .then(unary_operator)
        .map(|((lhs, operation), rhs)| operation(lhs, rhs));

    let r#let = recursive(|r#let| {
        text::keyword("let")
            .padded()
            .ignore_then(text::ident())
            .then_ignore(just('=').padded())
            .then(sum)
            .then_ignore(just(';'))
            .then(r#let.or(sum).or_not())
            .map(|((identifier, sum), then)| {
                Expression::Let(
                    identifier,
                    Box::new(sum),
                    Box::new(then.unwrap_or(Expression::End)),
                )
            })
    });
    r#let
}

fn eval<'src>(
    expression: Expression<'src>,
    variables: &mut HashMap<&'src str, Expression<'src>>,
) -> Expression<'src> {
    match expression {
        Expression::Number(number) => Expression::Number(number),
        Expression::Identifier(identifier) => variables.get(identifier).unwrap().clone(),
        Expression::Let(name, value, then) => {
            variables.insert(name, *value).unwrap();
            eval(*then, variables)
        }
        Expression::Mul(_, _) => todo!(),
        Expression::Add(lhs, rhs) => match (*lhs, *rhs) {
            (Expression::Number(lhs), Expression::Number(rhs)) => Expression::Number(lhs * rhs),
            (a, b) => Expression::Add(Box::new(eval(a, variables)), Box::new(eval(b, variables))),
        },
        Expression::Sub(_, _) => todo!(),
        Expression::End => Expression::End,
    }
}

fn main() {
    let source = std::fs::read_to_string("sample.cdtk").unwrap();
    let ast = parser().parse(source.as_str()).unwrap();
    println!("{ast:?}");
}
