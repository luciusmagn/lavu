use std::collections::{BTreeMap, BTreeSet, HashMap};

use thiserror::Error;

use crate::stdlib::primitive;
use crate::surface::{Expr, Program, TopLevel};
use crate::syntax::{Atom, SourceSpan, Spanned};
use crate::types::{ProcedureType, Type};

#[derive(Debug, Error, Clone, PartialEq)]
pub enum TypeError {
    #[error("unbound variable: {name}")]
    UnboundVariable { name: String, span: SourceSpan },

    #[error("expected a procedure")]
    ExpectedProcedure { actual: Type, span: SourceSpan },

    #[error("wrong number of arguments")]
    ArityMismatch {
        expected: String,
        actual: usize,
        span: SourceSpan,
    },

    #[error("type mismatch")]
    Mismatch {
        expected: Type,
        actual: Type,
        span: SourceSpan,
    },
}

#[derive(Debug, Clone, Default)]
pub struct TypeEnv {
    bindings: HashMap<String, Type>,
}

impl TypeEnv {
    pub fn new() -> Self {
        let mut env = Self::default();
        for primitive in crate::stdlib::r5rs_primitives() {
            env.bindings
                .insert(primitive.name.to_string(), primitive.signature);
        }
        env
    }

    pub fn define(&mut self, name: impl Into<String>, ty: Type) {
        self.bindings.insert(name.into(), ty);
    }

    pub fn get(&self, name: &str) -> Option<&Type> {
        self.bindings.get(name)
    }
}

#[derive(Debug, Clone, Default)]
pub struct Inferencer {
    substitutions: BTreeMap<String, Type>,
}

impl Inferencer {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn infer_program(
        &mut self,
        program: &Program,
        env: &mut TypeEnv,
    ) -> Result<Vec<Type>, TypeError> {
        program
            .forms
            .iter()
            .map(|form| self.infer_top_level(form, env))
            .collect()
    }

    pub fn infer_top_level(
        &mut self,
        form: &Spanned<TopLevel>,
        env: &mut TypeEnv,
    ) -> Result<Type, TypeError> {
        match &form.node {
            TopLevel::Define { name, value } => {
                let ty = self.infer_expr(value, env)?;
                let ty = self.resolve(ty);
                env.define(name.node.clone(), ty.clone());
                Ok(ty)
            }
            TopLevel::Expr(expr) => self.infer_expr(
                &Spanned {
                    node: expr.clone(),
                    span: form.span.clone(),
                    origin: form.origin,
                },
                env,
            ),
        }
    }

    pub fn infer_expr(&mut self, expr: &Spanned<Expr>, env: &TypeEnv) -> Result<Type, TypeError> {
        match &expr.node {
            Expr::Literal(atom) => Ok(type_of_atom(atom)),
            Expr::Variable(name) => env
                .get(name)
                .cloned()
                .map(|ty| self.resolve(ty))
                .ok_or_else(|| TypeError::UnboundVariable {
                    name: name.clone(),
                    span: expr.span.clone(),
                }),
            Expr::Quote(_) => Ok(Type::Any),
            Expr::Lambda { params, body } => self.infer_lambda(params, body, env),
            Expr::If {
                condition,
                consequent,
                alternate,
            } => self.infer_if(condition, consequent, alternate.as_deref(), env),
            Expr::Begin(exprs) => self.infer_sequence(exprs, env),
            Expr::Set { name, value } => {
                let expected =
                    env.get(&name.node)
                        .cloned()
                        .ok_or_else(|| TypeError::UnboundVariable {
                            name: name.node.clone(),
                            span: name.span.clone(),
                        })?;
                let actual = self.infer_expr(value, env)?;
                self.unify(actual, expected, value.span.clone())?;
                Ok(Type::Unknown)
            }
            Expr::Apply { operator, operands } => {
                let operator_ty = self.infer_expr(operator, env)?;
                let operand_tys = operands
                    .iter()
                    .map(|operand| self.infer_expr(operand, env))
                    .collect::<Result<Vec<_>, _>>()?;

                self.infer_application(operator_ty, operands, operand_tys, expr.span.clone())
            }
        }
    }

    fn infer_lambda(
        &mut self,
        params: &[Spanned<String>],
        body: &[Spanned<Expr>],
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        let mut local = env.clone();
        for param in params {
            local.define(param.node.clone(), Type::Var(param.node.clone()));
        }

        let result = self.infer_sequence(body, &local)?;
        let param_types = params
            .iter()
            .map(|param| self.resolve(Type::Var(param.node.clone())))
            .collect::<Vec<_>>();

        Ok(Type::procedure(param_types, self.resolve(result)))
    }

    fn infer_if(
        &mut self,
        condition: &Spanned<Expr>,
        consequent: &Spanned<Expr>,
        alternate: Option<&Spanned<Expr>>,
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        let predicate = predicate_refinement(condition);
        self.infer_expr(condition, env)?;

        let Some((refined_name, refined_type)) = predicate else {
            let consequent_ty = self.infer_expr(consequent, env)?;
            let alternate_ty = match alternate {
                Some(expr) => self.infer_expr(expr, env)?,
                None => Type::Unknown,
            };
            return Ok(Type::union(vec![
                self.resolve(consequent_ty),
                self.resolve(alternate_ty),
            ]));
        };

        let base = self.clone();

        let mut then_env = env.clone();
        then_env.define(refined_name.clone(), refined_type.clone());
        let mut then_inferencer = base.clone();
        let consequent_ty = then_inferencer.infer_expr(consequent, &then_env)?;

        let mut else_inferencer = base;
        let alternate_ty = match alternate {
            Some(expr) => else_inferencer.infer_expr(expr, env)?,
            None => Type::Unknown,
        };

        self.merge_branch_substitutions(
            &refined_name,
            refined_type,
            &then_inferencer,
            &else_inferencer,
        );

        Ok(Type::union(vec![
            then_inferencer.resolve(consequent_ty),
            else_inferencer.resolve(alternate_ty),
        ]))
    }

    fn merge_branch_substitutions(
        &mut self,
        refined_name: &str,
        refined_type: Type,
        then_inferencer: &Inferencer,
        else_inferencer: &Inferencer,
    ) {
        let mut names = BTreeSet::new();
        names.insert(refined_name.to_string());
        names.extend(then_inferencer.substitutions.keys().cloned());
        names.extend(else_inferencer.substitutions.keys().cloned());

        for name in names {
            let then_ty = if name == refined_name {
                Some(refined_type.clone())
            } else {
                then_inferencer
                    .substitutions
                    .get(&name)
                    .cloned()
                    .map(|ty| then_inferencer.resolve(ty))
            };
            let else_ty = else_inferencer
                .substitutions
                .get(&name)
                .cloned()
                .map(|ty| else_inferencer.resolve(ty));

            let merged = match (then_ty, else_ty) {
                (Some(then_ty), Some(else_ty)) => Type::union(vec![then_ty, else_ty]),
                (Some(then_ty), None) => then_ty,
                (None, Some(else_ty)) => else_ty,
                (None, None) => continue,
            };

            self.substitutions.insert(name, self.resolve(merged));
        }
    }

    fn infer_sequence(
        &mut self,
        exprs: &[Spanned<Expr>],
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        let mut result = Type::Unknown;
        for expr in exprs {
            result = self.infer_expr(expr, env)?;
        }
        Ok(self.resolve(result))
    }

    fn infer_application(
        &mut self,
        operator_ty: Type,
        operands: &[Spanned<Expr>],
        operand_tys: Vec<Type>,
        span: SourceSpan,
    ) -> Result<Type, TypeError> {
        match self.resolve(operator_ty) {
            Type::Procedure(procedure) => self.apply_procedure(procedure, operands, operand_tys),
            actual => Err(TypeError::ExpectedProcedure { actual, span }),
        }
    }

    fn apply_procedure(
        &mut self,
        procedure: ProcedureType,
        operands: &[Spanned<Expr>],
        operand_tys: Vec<Type>,
    ) -> Result<Type, TypeError> {
        match procedure {
            ProcedureType::Fixed { params, result } => {
                if params.len() != operand_tys.len() {
                    return Err(TypeError::ArityMismatch {
                        expected: params.len().to_string(),
                        actual: operand_tys.len(),
                        span: span_for_operands(operands),
                    });
                }

                for ((actual, expected), operand) in
                    operand_tys.into_iter().zip(params).zip(operands)
                {
                    self.unify(actual, expected, operand.span.clone())?;
                }

                Ok(self.resolve(*result))
            }
            ProcedureType::UniformVariadic { param, result } => {
                for (actual, operand) in operand_tys.into_iter().zip(operands) {
                    self.unify(actual, (*param).clone(), operand.span.clone())?;
                }
                Ok(self.resolve(*result))
            }
            ProcedureType::Rest {
                required,
                rest,
                result,
            } => {
                if operand_tys.len() < required.len() {
                    return Err(TypeError::ArityMismatch {
                        expected: format!("at least {}", required.len()),
                        actual: operand_tys.len(),
                        span: span_for_operands(operands),
                    });
                }

                for ((actual, expected), operand) in operand_tys
                    .iter()
                    .cloned()
                    .zip(required.iter().cloned())
                    .zip(operands)
                {
                    self.unify(actual, expected, operand.span.clone())?;
                }

                for (actual, operand) in operand_tys
                    .into_iter()
                    .skip(required.len())
                    .zip(operands.iter().skip(required.len()))
                {
                    self.unify(actual, (*rest).clone(), operand.span.clone())?;
                }

                Ok(self.resolve(*result))
            }
        }
    }

    fn unify(&mut self, actual: Type, expected: Type, span: SourceSpan) -> Result<Type, TypeError> {
        let actual = self.resolve(actual);
        let expected = self.resolve(expected);

        match (actual, expected) {
            (Type::Unknown, ty) | (ty, Type::Unknown) | (Type::Any, ty) | (ty, Type::Any) => Ok(ty),
            (Type::Var(name), ty) | (ty, Type::Var(name)) => {
                self.substitutions.insert(name, ty.clone());
                Ok(ty)
            }
            (Type::ListOf(actual), Type::ListOf(expected)) => self.unify(*actual, *expected, span),
            (Type::Pair(actual_car, actual_cdr), Type::Pair(expected_car, expected_cdr)) => {
                let car = self.unify(*actual_car, *expected_car, span.clone())?;
                let cdr = self.unify(*actual_cdr, *expected_cdr, span)?;
                Ok(Type::Pair(Box::new(car), Box::new(cdr)))
            }
            (actual, expected) if actual == expected => Ok(actual),
            (actual, expected) => Err(TypeError::Mismatch {
                expected,
                actual,
                span,
            }),
        }
    }

    fn resolve(&self, ty: Type) -> Type {
        match ty {
            Type::Var(name) => self
                .substitutions
                .get(&name)
                .cloned()
                .map(|ty| self.resolve(ty))
                .unwrap_or(Type::Var(name)),
            Type::ListOf(item) => Type::ListOf(Box::new(self.resolve(*item))),
            Type::Pair(car, cdr) => {
                Type::Pair(Box::new(self.resolve(*car)), Box::new(self.resolve(*cdr)))
            }
            Type::Procedure(ProcedureType::Fixed { params, result }) => Type::procedure(
                params
                    .into_iter()
                    .map(|ty| self.resolve(ty))
                    .collect::<Vec<_>>(),
                self.resolve(*result),
            ),
            Type::Procedure(ProcedureType::UniformVariadic { param, result }) => {
                Type::uniform_variadic(self.resolve(*param), self.resolve(*result))
            }
            Type::Procedure(ProcedureType::Rest {
                required,
                rest,
                result,
            }) => Type::rest_procedure(
                required
                    .into_iter()
                    .map(|ty| self.resolve(ty))
                    .collect::<Vec<_>>(),
                self.resolve(*rest),
                self.resolve(*result),
            ),
            Type::Union(types) => Type::union(
                types
                    .into_iter()
                    .map(|ty| self.resolve(ty))
                    .collect::<Vec<_>>(),
            ),
            ty => ty,
        }
    }
}

fn type_of_atom(atom: &Atom) -> Type {
    match atom {
        Atom::Identifier(name) => primitive(name)
            .map(|primitive| primitive.signature)
            .unwrap_or_else(|| Type::Var(name.clone())),
        Atom::Integer(_) | Atom::Decimal(_) | Atom::Real(_, _) | Atom::Complex(_) => Type::Number,
        Atom::String(_) => Type::String,
        Atom::Boolean(_) => Type::Boolean,
        Atom::Character(_) => Type::Char,
    }
}

fn span_for_operands(operands: &[Spanned<Expr>]) -> SourceSpan {
    match (operands.first(), operands.last()) {
        (Some(first), Some(last)) => first.span.start..last.span.end,
        _ => 0..0,
    }
}

fn predicate_refinement(condition: &Spanned<Expr>) -> Option<(String, Type)> {
    let Expr::Apply { operator, operands } = &condition.node else {
        return None;
    };
    if operands.len() != 1 {
        return None;
    }

    let Expr::Variable(predicate_name) = &operator.node else {
        return None;
    };
    let Expr::Variable(variable_name) = &operands[0].node else {
        return None;
    };

    primitive(predicate_name).and_then(|primitive| {
        primitive
            .predicate
            .filter(|predicate| predicate.argument == 0)
            .map(|predicate| (variable_name.clone(), predicate.positive))
    })
}

#[cfg(test)]
mod tests {
    use crate::datum_parser::parse;
    use crate::infer::{Inferencer, TypeEnv};
    use crate::surface::classify_program;

    fn infer_one(input: &str) -> String {
        let datums = parse(input).unwrap();
        let program = classify_program(&datums).unwrap();
        let mut env = TypeEnv::new();
        let mut inferencer = Inferencer::new();
        inferencer.infer_program(&program, &mut env).unwrap()[0].to_string()
    }

    #[test]
    fn infers_primitive_arithmetic_lambda() {
        assert_eq!(infer_one("(lambda (x) (+ x 1))"), "(-> number? number?)");
    }

    #[test]
    fn infers_reverse_lambda_from_primitive_signature() {
        assert_eq!(
            infer_one("(lambda (x) (reverse x))"),
            "(-> (listof a) (listof a))"
        );
    }

    #[test]
    fn infers_character_comparison_lambda() {
        assert_eq!(
            infer_one("(lambda (c) (char=? c #\\a))"),
            "(-> char? boolean?)"
        );
    }

    #[test]
    fn infers_union_parameters_from_predicate_branches() {
        assert_eq!(
            infer_one("(lambda (x) (if (string? x) (string-length x) (+ x 1)))"),
            "(-> (U number? string?) number?)"
        );
    }
}
