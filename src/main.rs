#![allow(dead_code, unused)]
#![forbid(unused_must_use)]

use std::collections::HashMap;

use ariadne::{Label, Report, ReportKind, Source};
use chumsky::prelude::*;

type Args<'src> = Vec<Expression<'src>>;
type Parameters<'src> = Vec<&'src str>;
type Bound<'src> = Option<Box<Expression<'src>>>;

#[derive(Debug, Clone)]
enum Expression<'src> {
    Number(f64),
    Identifier(&'src str),
    Let(&'src str, Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Call(&'src str, Args<'src>),
    Tuple(Args<'src>),
    Closure(Parameters<'src>, Box<Expression<'src>>),
    Range(Bound<'src>, Bound<'src>),
    // Dot(Vec<&'src str>),
    Dot(Box<Expression<'src>>, Box<Expression<'src>>),
    End,
}

type BinaryOperator<'src> = fn(Box<Expression<'src>>, Box<Expression<'src>>) -> Expression<'src>;

fn binary_parser<'src>(
    previous_parser: impl Parser<'src, &'src str, Expression<'src>, extra::Err<Simple<'src, char>>>
        + Clone,
    operator1: (char, BinaryOperator<'src>),
    operator2: (char, BinaryOperator<'src>),
) -> impl Parser<'src, &'src str, Expression<'src>, extra::Err<Simple<'src, char>>> + Clone {
    let binary_operator = choice((
        just(operator1.0).to(operator1.1),
        just(operator2.0).to(operator2.1),
    ));
    previous_parser.clone().foldl(
        binary_operator.then(previous_parser).repeated(),
        |a, (op, b)| op(Box::new(a), Box::new(b)),
    )
}

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

    let expression = recursive(|expression| {
        let tuple = expression
            .clone()
            .separated_by(just(','))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just('('), just(')'));

        let range = expression
            .clone()
            .or_not()
            .separated_by(just(".."))
            .exactly(2)
            .collect_exactly::<[_; 2]>()
            .delimited_by(just('('), just(')'))
            .map(|[min, max]| Expression::Range(min.map(Box::new), max.map(Box::new)));

        let call = text::ident()
            .then(tuple.clone())
            .map(|(name, arguments)| Expression::Call(name, arguments));

        let closure = text::ident()
            .padded()
            .separated_by(just(','))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just('|'), just('|'))
            .then(expression.clone())
            .map(|(parameters, expression)| Expression::Closure(parameters, Box::new(expression)));

        let value = choice((
            range,
            closure,
            call,
            number,
            identifier,
            expression.delimited_by(just('('), just(')')),
            tuple.map(Expression::Tuple),
        ))
        .padded();

        let dot_operator = value
            .clone()
            .foldl(just('.').ignore_then(value.clone()).repeated(), |a, b| {
                Expression::Dot(Box::new(a), Box::new(b))
            });

        // let dot_operator = text::ident()
        //     .separated_by(just('.'))
        //     .collect::<Vec<_>>()
        //     .map(Expression::Dot);

        let unary_operator = choice((just('-').to(-1.0f64), just('+').to(1.0f64)))
            .repeated()
            .foldr(dot_operator, |a, b| {
                Expression::Mul(Box::new(Expression::Number(a)), Box::new(b))
            });

        let product = binary_parser(
            unary_operator,
            ('*', Expression::Mul),
            ('/', Expression::Div),
        );
        let sum = binary_parser(
            product.clone(),
            ('+', Expression::Add),
            ('-', Expression::Sub),
        );
        sum
    });

    let r#let = recursive(|r#let| {
        text::keyword("let")
            .padded()
            .ignore_then(text::ident())
            .then_ignore(just('=').padded())
            .then(expression.clone())
            .then_ignore(just(';'))
            .then(r#let.or(expression).or_not())
            .map(|((identifier, expression), then)| {
                Expression::Let(
                    identifier,
                    Box::new(expression),
                    Box::new(then.unwrap_or(Expression::End)),
                )
            })
    });
    r#let
}

fn main() {
    let source = std::fs::read_to_string("sample.cdtk").unwrap();
    match parser().parse(source.as_str()).into_result() {
        Ok(ast) => println!("{ast:#?}"),
        Err(errors) => errors.into_iter().for_each(|error| {
            Report::build(ReportKind::Error, (), error.span().start)
                .with_message("Parse error")
                .with_label(Label::new(error.span().into_range()).with_message(error.to_string()))
                .finish()
                .print(Source::from(source.clone()))
                .unwrap();
        }),
    };
}
