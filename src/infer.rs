use std::collections::{BTreeMap, BTreeSet, HashMap};

use thiserror::Error;

use crate::stdlib::primitive;
use crate::surface::{Expr, Program, TopLevel};
use crate::syntax::{Atom, Datum, SourceSpan, Spanned};
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
    next_var: usize,
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
            Expr::Quote(datum) => Ok(type_of_datum(datum)),
            Expr::Quasiquote(_) => Ok(Type::Any),
            Expr::Lambda { params, rest, body } => {
                self.infer_lambda(params, rest.as_ref(), body, env)
            }
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
            Expr::Delay(_) => Ok(Type::Any),
            Expr::LetRec { bindings, body } => self.infer_letrec(bindings, body, env),
            Expr::Apply { operator, operands } => {
                let operator_ty = self.infer_expr(operator, env)?;
                let operand_tys = operands
                    .iter()
                    .map(|operand| self.infer_expr(operand, env))
                    .collect::<Result<Vec<_>, _>>()?;

                if matches!(&operator.node, Expr::Variable(name) if name == "apply") {
                    return self.infer_apply_primitive(operands, operand_tys, expr.span.clone());
                }

                self.infer_application(operator_ty, operands, operand_tys, expr.span.clone())
            }
        }
    }

    fn infer_lambda(
        &mut self,
        params: &[Spanned<String>],
        rest: Option<&Spanned<String>>,
        body: &[Spanned<Expr>],
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        let mut local = env.clone();
        for param in params {
            local.define(param.node.clone(), Type::Var(param.node.clone()));
        }
        if let Some(rest) = rest {
            local.define(
                rest.node.clone(),
                Type::ListOf(Box::new(Type::Var(rest.node.clone()))),
            );
        }

        let result = self.infer_sequence(body, &local)?;
        let param_types = params
            .iter()
            .map(|param| self.resolve(Type::Var(param.node.clone())))
            .collect::<Vec<_>>();

        let result = self.resolve(result);
        Ok(match rest {
            Some(rest) => Type::rest_procedure(
                param_types,
                self.resolve(Type::Var(rest.node.clone())),
                result,
            ),
            None => Type::procedure(param_types, result),
        })
    }

    fn infer_letrec(
        &mut self,
        bindings: &[(Spanned<String>, Spanned<Expr>)],
        body: &[Spanned<Expr>],
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        let mut local = env.clone();
        for (name, _) in bindings {
            local.define(name.node.clone(), self.fresh_type_var());
        }

        for (name, value) in bindings {
            let expected =
                local
                    .get(&name.node)
                    .cloned()
                    .ok_or_else(|| TypeError::UnboundVariable {
                        name: name.node.clone(),
                        span: name.span.clone(),
                    })?;
            let actual = self.infer_expr(value, &local)?;
            let inferred = self.unify(actual, expected, value.span.clone())?;
            local.define(name.node.clone(), self.resolve(inferred));
        }

        self.infer_sequence(body, &local)
    }

    fn infer_if(
        &mut self,
        condition: &Spanned<Expr>,
        consequent: &Spanned<Expr>,
        alternate: Option<&Spanned<Expr>>,
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        let refinement = predicate_refinement(condition);
        self.infer_expr(condition, env)?;

        let base = self.clone();

        let mut then_env = env.clone();
        if let Some(refinement) = refinement
            .as_ref()
            .filter(|r| r.branch == RefinedBranch::Then)
        {
            then_env.define(refinement.name.clone(), refinement.positive.clone());
        }
        let mut then_inferencer = base.clone();
        let consequent_ty = then_inferencer.infer_expr(consequent, &then_env)?;

        let mut else_env = env.clone();
        if let Some(refinement) = refinement
            .as_ref()
            .filter(|r| r.branch == RefinedBranch::Else)
        {
            else_env.define(refinement.name.clone(), refinement.positive.clone());
        }
        let mut else_inferencer = base;
        let alternate_ty = match alternate {
            Some(expr) => else_inferencer.infer_expr(expr, &else_env)?,
            None => Type::Unknown,
        };

        self.merge_branch_substitutions(
            refinement.as_ref(),
            consequent,
            alternate,
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
        refinement: Option<&BranchRefinement>,
        consequent: &Spanned<Expr>,
        alternate: Option<&Spanned<Expr>>,
        then_inferencer: &Inferencer,
        else_inferencer: &Inferencer,
    ) {
        let mut names = BTreeSet::new();
        if let Some(refinement) = refinement {
            names.insert(refinement.name.clone());
        }
        names.extend(then_inferencer.substitutions.keys().cloned());
        names.extend(else_inferencer.substitutions.keys().cloned());

        for name in names {
            let then_ty = then_inferencer
                .substitutions
                .get(&name)
                .cloned()
                .map(|ty| then_inferencer.resolve(ty))
                .or_else(|| {
                    refinement.and_then(|refinement| {
                        (refinement.branch == RefinedBranch::Then
                            && name == refinement.name.as_str()
                            && expr_mentions_variable(consequent, &refinement.name))
                        .then(|| refinement.positive.clone())
                    })
                });
            let else_ty = else_inferencer
                .substitutions
                .get(&name)
                .cloned()
                .map(|ty| else_inferencer.resolve(ty))
                .or_else(|| {
                    refinement.and_then(|refinement| {
                        (refinement.branch == RefinedBranch::Else
                            && name == refinement.name.as_str()
                            && alternate
                                .is_some_and(|expr| expr_mentions_variable(expr, &refinement.name)))
                        .then(|| refinement.positive.clone())
                    })
                });

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
            Type::Var(name) => {
                let result = self.fresh_type_var();
                self.substitutions.insert(
                    name,
                    Type::procedure(
                        operand_tys
                            .iter()
                            .cloned()
                            .map(|ty| self.resolve(ty))
                            .collect::<Vec<_>>(),
                        result.clone(),
                    ),
                );
                Ok(self.resolve(result))
            }
            actual => Err(TypeError::ExpectedProcedure { actual, span }),
        }
    }

    fn infer_apply_primitive(
        &mut self,
        operands: &[Spanned<Expr>],
        operand_tys: Vec<Type>,
        span: SourceSpan,
    ) -> Result<Type, TypeError> {
        if operand_tys.len() < 2 {
            return Err(TypeError::ArityMismatch {
                expected: "at least 2".to_string(),
                actual: operand_tys.len(),
                span,
            });
        }

        let mut operand_tys = operand_tys.into_iter();
        let procedure_ty = operand_tys
            .next()
            .expect("arity check ensures a procedure operand");
        let mut arguments = operand_tys.collect::<Vec<_>>();
        let final_list = arguments
            .pop()
            .expect("arity check ensures a final list operand");
        let fixed_operands = &operands[1..operands.len() - 1];
        let final_operand = operands
            .last()
            .expect("arity check ensures a final list operand");

        match self.resolve(procedure_ty) {
            Type::Procedure(ProcedureType::UniformVariadic { param, result }) => {
                for (actual, operand) in arguments.into_iter().zip(fixed_operands) {
                    self.unify(actual, (*param).clone(), operand.span.clone())?;
                }
                self.unify_apply_final_list(final_list, (*param).clone(), final_operand)?;
                Ok(self.resolve(*result))
            }
            Type::Procedure(ProcedureType::Rest {
                required,
                rest,
                result,
            }) if arguments.len() >= required.len() => {
                for ((actual, expected), operand) in arguments
                    .iter()
                    .cloned()
                    .zip(required.iter().cloned())
                    .zip(fixed_operands)
                {
                    self.unify(actual, expected, operand.span.clone())?;
                }
                for (actual, operand) in arguments
                    .into_iter()
                    .skip(required.len())
                    .zip(fixed_operands.iter().skip(required.len()))
                {
                    self.unify(actual, (*rest).clone(), operand.span.clone())?;
                }
                self.unify_apply_final_list(final_list, (*rest).clone(), final_operand)?;
                Ok(self.resolve(*result))
            }
            _ => Ok(Type::Any),
        }
    }

    fn unify_apply_final_list(
        &mut self,
        actual: Type,
        expected_element: Type,
        operand: &Spanned<Expr>,
    ) -> Result<(), TypeError> {
        match self.resolve(actual) {
            Type::ListOf(element) => {
                self.unify(*element, expected_element, operand.span.clone())?;
            }
            Type::Null => {}
            Type::List => {}
            actual => {
                self.unify(actual, Type::List, operand.span.clone())?;
            }
        }
        Ok(())
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
            (Type::Var(name), ty) | (ty, Type::Var(name)) => self.bind_var(name, ty),
            (Type::ListOf(_), Type::List) | (Type::List, Type::ListOf(_)) => Ok(Type::List),
            (Type::Null, Type::List) | (Type::List, Type::Null) => Ok(Type::List),
            (Type::ListOf(actual), Type::ListOf(expected)) => self.unify(*actual, *expected, span),
            (Type::Pair(actual_car, actual_cdr), Type::Pair(expected_car, expected_cdr)) => {
                let car = self.unify(*actual_car, *expected_car, span.clone())?;
                let cdr = self.unify(*actual_cdr, *expected_cdr, span)?;
                Ok(Type::Pair(Box::new(car), Box::new(cdr)))
            }
            (Type::Procedure(actual), Type::Procedure(expected)) => {
                self.unify_procedure(actual, expected, span)
            }
            (actual, expected) if actual == expected => Ok(actual),
            (actual, expected) => Err(TypeError::Mismatch {
                expected,
                actual,
                span,
            }),
        }
    }

    fn unify_procedure(
        &mut self,
        actual: ProcedureType,
        expected: ProcedureType,
        span: SourceSpan,
    ) -> Result<Type, TypeError> {
        match (actual, expected) {
            (
                ProcedureType::Fixed {
                    params: actual_params,
                    result: actual_result,
                },
                ProcedureType::Fixed {
                    params: expected_params,
                    result: expected_result,
                },
            ) => {
                if actual_params.len() != expected_params.len() {
                    return Err(TypeError::ArityMismatch {
                        expected: expected_params.len().to_string(),
                        actual: actual_params.len(),
                        span,
                    });
                }

                let params = actual_params
                    .into_iter()
                    .zip(expected_params)
                    .map(|(actual, expected)| self.unify(actual, expected, span.clone()))
                    .collect::<Result<Vec<_>, _>>()?;
                let result = self.unify(*actual_result, *expected_result, span)?;

                Ok(Type::procedure(params, result))
            }
            (
                ProcedureType::UniformVariadic {
                    param: actual_param,
                    result: actual_result,
                },
                ProcedureType::UniformVariadic {
                    param: expected_param,
                    result: expected_result,
                },
            ) => {
                let param = self.unify(*actual_param, *expected_param, span.clone())?;
                let result = self.unify(*actual_result, *expected_result, span)?;
                Ok(Type::uniform_variadic(param, result))
            }
            (
                ProcedureType::Rest {
                    required: actual_required,
                    rest: actual_rest,
                    result: actual_result,
                },
                ProcedureType::Rest {
                    required: expected_required,
                    rest: expected_rest,
                    result: expected_result,
                },
            ) => {
                if actual_required.len() != expected_required.len() {
                    return Err(TypeError::ArityMismatch {
                        expected: expected_required.len().to_string(),
                        actual: actual_required.len(),
                        span,
                    });
                }

                let required = actual_required
                    .into_iter()
                    .zip(expected_required)
                    .map(|(actual, expected)| self.unify(actual, expected, span.clone()))
                    .collect::<Result<Vec<_>, _>>()?;
                let rest = self.unify(*actual_rest, *expected_rest, span.clone())?;
                let result = self.unify(*actual_result, *expected_result, span)?;

                Ok(Type::rest_procedure(required, rest, result))
            }
            (actual, expected) => Err(TypeError::Mismatch {
                expected: Type::Procedure(expected),
                actual: Type::Procedure(actual),
                span,
            }),
        }
    }

    fn bind_var(&mut self, name: String, ty: Type) -> Result<Type, TypeError> {
        if ty == Type::Var(name.clone()) {
            return Ok(ty);
        }

        let ty = strip_recursive_var(ty, &name);
        self.substitutions.insert(name, ty.clone());
        Ok(ty)
    }

    fn fresh_type_var(&mut self) -> Type {
        let name = format!("t{}", self.next_var);
        self.next_var += 1;
        Type::Var(name)
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

fn strip_recursive_var(ty: Type, name: &str) -> Type {
    match ty {
        Type::Union(types) => {
            let finite = types
                .into_iter()
                .filter(|ty| !matches!(ty, Type::Var(var) if var == name))
                .collect::<Vec<_>>();

            match finite.as_slice() {
                [] => Type::Unknown,
                _ => Type::union(finite),
            }
        }
        ty if contains_var(&ty, name) => Type::Unknown,
        ty => ty,
    }
}

fn contains_var(ty: &Type, name: &str) -> bool {
    match ty {
        Type::Var(var) => var == name,
        Type::Pair(car, cdr) => contains_var(car, name) || contains_var(cdr, name),
        Type::ListOf(element) => contains_var(element, name),
        Type::Procedure(ProcedureType::Fixed { params, result }) => {
            params.iter().any(|ty| contains_var(ty, name)) || contains_var(result, name)
        }
        Type::Procedure(ProcedureType::UniformVariadic { param, result }) => {
            contains_var(param, name) || contains_var(result, name)
        }
        Type::Procedure(ProcedureType::Rest {
            required,
            rest,
            result,
        }) => {
            required.iter().any(|ty| contains_var(ty, name))
                || contains_var(rest, name)
                || contains_var(result, name)
        }
        Type::Union(types) => types.iter().any(|ty| contains_var(ty, name)),
        _ => false,
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

fn type_of_datum(datum: &Spanned<Datum>) -> Type {
    match &datum.node {
        Datum::Atom(Atom::Identifier(_)) => Type::Symbol,
        Datum::Atom(atom) => type_of_atom(atom),
        Datum::List(items) => type_of_list_datums(items),
        Datum::DottedList(items, tail) => {
            items.iter().rev().fold(type_of_datum(tail), |cdr, car| {
                Type::Pair(Box::new(type_of_datum(car)), Box::new(cdr))
            })
        }
        Datum::Vector(_) => Type::Vector,
        Datum::Quote(inner) => abbreviation_datum_type("quote", inner),
        Datum::Quasiquote(inner) => abbreviation_datum_type("quasiquote", inner),
        Datum::Unquote(inner) => abbreviation_datum_type("unquote", inner),
        Datum::UnquoteSplicing(inner) => abbreviation_datum_type("unquote-splicing", inner),
    }
}

fn type_of_list_datums(items: &[Spanned<Datum>]) -> Type {
    if items.is_empty() {
        return Type::Null;
    }

    Type::ListOf(Box::new(Type::union(
        items.iter().map(type_of_datum).collect::<Vec<_>>(),
    )))
}

fn abbreviation_datum_type(_name: &'static str, datum: &Spanned<Datum>) -> Type {
    Type::ListOf(Box::new(Type::union(vec![
        Type::Symbol,
        type_of_datum(datum),
    ])))
}

fn span_for_operands(operands: &[Spanned<Expr>]) -> SourceSpan {
    match (operands.first(), operands.last()) {
        (Some(first), Some(last)) => first.span.start..last.span.end,
        _ => 0..0,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RefinedBranch {
    Then,
    Else,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct BranchRefinement {
    branch: RefinedBranch,
    name: String,
    positive: Type,
}

fn predicate_refinement(condition: &Spanned<Expr>) -> Option<BranchRefinement> {
    if let Some(refinement) = direct_predicate_refinement(condition) {
        return Some(BranchRefinement {
            branch: RefinedBranch::Then,
            name: refinement.0,
            positive: refinement.1,
        });
    }

    let Expr::Apply { operator, operands } = &condition.node else {
        return None;
    };
    let Expr::Variable(operator_name) = &operator.node else {
        return None;
    };
    if operator_name != "not" || operands.len() != 1 {
        return None;
    }

    direct_predicate_refinement(&operands[0]).map(|(name, positive)| BranchRefinement {
        branch: RefinedBranch::Else,
        name,
        positive,
    })
}

fn direct_predicate_refinement(condition: &Spanned<Expr>) -> Option<(String, Type)> {
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

fn expr_mentions_variable(expr: &Spanned<Expr>, name: &str) -> bool {
    match &expr.node {
        Expr::Variable(variable) => variable == name,
        Expr::Lambda { params, rest, body } => {
            !params.iter().any(|param| param.node == name)
                && rest.as_ref().is_none_or(|param| param.node != name)
                && body.iter().any(|expr| expr_mentions_variable(expr, name))
        }
        Expr::If {
            condition,
            consequent,
            alternate,
        } => {
            expr_mentions_variable(condition, name)
                || expr_mentions_variable(consequent, name)
                || alternate
                    .as_deref()
                    .is_some_and(|expr| expr_mentions_variable(expr, name))
        }
        Expr::Begin(exprs) => exprs.iter().any(|expr| expr_mentions_variable(expr, name)),
        Expr::Set {
            name: set_name,
            value,
        } => set_name.node == name || expr_mentions_variable(value, name),
        Expr::Delay(expr) => expr_mentions_variable(expr, name),
        Expr::LetRec { bindings, body } => {
            let shadows = bindings
                .iter()
                .any(|(binding_name, _)| binding_name.node == name);
            !shadows
                && (bindings
                    .iter()
                    .any(|(_, value)| expr_mentions_variable(value, name))
                    || body.iter().any(|expr| expr_mentions_variable(expr, name)))
        }
        Expr::Apply { operator, operands } => {
            expr_mentions_variable(operator, name)
                || operands
                    .iter()
                    .any(|operand| expr_mentions_variable(operand, name))
        }
        Expr::Literal(_) | Expr::Quote(_) | Expr::Quasiquote(_) => false,
    }
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
    fn infers_rest_lambda_formals() {
        assert_eq!(infer_one("(lambda args args)"), "(-> args * (listof args))");
        assert_eq!(
            infer_one("(lambda (x . rest) (reverse rest))"),
            "(-> x a * (listof a))"
        );
    }

    #[test]
    fn infers_internal_definitions() {
        assert_eq!(infer_one("((lambda () (define x 1) x))"), "number?");
        assert_eq!(
            infer_one("((lambda () (define (add1 x) (+ x 1)) (add1 4)))"),
            "number?"
        );
    }

    #[test]
    fn infers_reverse_lambda_from_primitive_signature() {
        assert_eq!(
            infer_one("(lambda (x) (reverse x))"),
            "(-> (listof a) (listof a))"
        );
    }

    #[test]
    fn infers_pair_mutators() {
        assert_eq!(infer_one("(set-car! (cons 1 2) 9)"), "unknown?");
        assert_eq!(infer_one("(set-cdr! (cons 1 2) 9)"), "unknown?");
    }

    #[test]
    fn infers_character_comparison_lambda() {
        assert_eq!(
            infer_one("(lambda (c) (char=? c #\\a))"),
            "(-> char? boolean?)"
        );
    }

    #[test]
    fn infers_character_classification_and_case() {
        assert_eq!(
            infer_one("(lambda (c) (char-alphabetic? c))"),
            "(-> char? boolean?)"
        );
        assert_eq!(infer_one("(char-upcase #\\a)"), "char?");
    }

    #[test]
    fn infers_union_parameters_from_predicate_branches() {
        assert_eq!(
            infer_one("(lambda (x) (if (string? x) (string-length x) (+ x 1)))"),
            "(-> (U number? string?) number?)"
        );
    }

    #[test]
    fn isolates_if_branch_constraints_without_predicates() {
        assert_eq!(
            infer_one("(lambda (x flag) (if flag (+ x 1) (string-length x)))"),
            "(-> (U number? string?) flag number?)"
        );
    }

    #[test]
    fn refines_not_predicates_in_alternate_branch() {
        assert_eq!(
            infer_one("(lambda (x) (if (not (string? x)) (+ x 1) (string-length x)))"),
            "(-> (U number? string?) number?)"
        );
    }

    #[test]
    fn does_not_export_unused_predicate_refinements() {
        assert_eq!(
            infer_one("(lambda (x) (if (number? x) 1 0))"),
            "(-> x number?)"
        );
    }

    #[test]
    fn infers_named_let_result_type() {
        assert_eq!(
            infer_one("(let loop ((n 5) (acc 1)) (if (= n 0) acc (loop (- n 1) (* acc n))))"),
            "number?"
        );
    }

    #[test]
    fn infers_case_result_type() {
        assert_eq!(infer_one("(case 'b ((a c) 10) (else 30))"), "number?");
    }

    #[test]
    fn infers_cond_arrow_result_type() {
        assert_eq!(
            infer_one("(cond (1 => (lambda (x) (+ x 10))) (else 0))"),
            "number?"
        );
    }

    #[test]
    fn infers_do_result_type() {
        assert_eq!(
            infer_one("(do ((i 0 (+ i 1)) (acc 0 (+ acc i))) ((= i 5) acc))"),
            "number?"
        );
    }

    #[test]
    fn infers_delay_conservatively() {
        assert_eq!(infer_one("(delay (+ 1 2))"), "any?");
    }

    #[test]
    fn infers_quasiquote_conservatively() {
        assert_eq!(infer_one("`(1 ,(+ 1 2))"), "any?");
    }

    #[test]
    fn infers_quoted_datum_shapes() {
        assert_eq!(infer_one("'(1 2 3)"), "(listof number?)");
        assert_eq!(infer_one("'()"), "null?");
        assert_eq!(infer_one("'(1 . \"x\")"), "(pair? number? string?)");
        assert_eq!(infer_one("'(1 \"x\")"), "(listof (U number? string?))");
    }

    #[test]
    fn infers_simple_apply_calls() {
        assert_eq!(infer_one("(apply + '(1 2 3))"), "number?");
        assert_eq!(infer_one("(apply string-append '(\"a\" \"b\"))"), "string?");
    }

    #[test]
    fn infers_vector_primitive_types() {
        assert_eq!(infer_one("(vector 1 2 3)"), "vector?");
        assert_eq!(infer_one("(vector-ref (vector 1 2 3) 0)"), "any?");
        assert_eq!(infer_one("(vector-length (vector 1 2 3))"), "number?");
    }

    #[test]
    fn infers_equality_predicates() {
        assert_eq!(infer_one("(equal? '(1) '(1))"), "boolean?");
    }

    #[test]
    fn infers_indexed_list_primitives() {
        assert_eq!(infer_one("(length '(a b c))"), "number?");
        assert_eq!(infer_one("(cadr '(a b c))"), "any?");
        assert_eq!(infer_one("(list-ref '(a b c) 1)"), "any?");
        assert_eq!(infer_one("(list-tail '(a b c) 1)"), "list?");
    }

    #[test]
    fn infers_membership_primitives_conservatively() {
        assert_eq!(infer_one("(member 'b '(a b c))"), "any?");
        assert_eq!(infer_one("(assoc 'b '((a 1) (b 2)))"), "any?");
    }

    #[test]
    fn infers_higher_order_iteration_conservatively() {
        assert_eq!(infer_one("(map + '(1 2) '(3 4))"), "list?");
        assert_eq!(infer_one("(for-each + '(1 2) '(3 4))"), "unknown?");
    }

    #[test]
    fn infers_conversion_primitives() {
        assert_eq!(infer_one("(symbol->string 'hello)"), "string?");
        assert_eq!(infer_one("(string->number \"1\")"), "(U boolean? number?)");
    }

    #[test]
    fn infers_string_primitives() {
        assert_eq!(infer_one("(string #\\a #\\b)"), "string?");
        assert_eq!(infer_one("(string-ref \"abc\" 1)"), "char?");
        assert_eq!(infer_one("(string->list \"ab\")"), "(listof char?)");
        assert_eq!(infer_one("(list->string '(#\\a #\\b))"), "string?");
        assert_eq!(infer_one("(string-set! \"ab\" 0 #\\z)"), "unknown?");
    }

    #[test]
    fn infers_numeric_predicates_and_integer_utilities() {
        assert_eq!(infer_one("(zero? 0)"), "boolean?");
        assert_eq!(infer_one("(quotient 5 2)"), "number?");
        assert_eq!(infer_one("(floor 3/2)"), "number?");
        assert_eq!(infer_one("(exact->inexact 1/2)"), "number?");
        assert_eq!(infer_one("(make-rectangular 1 2)"), "number?");
        assert_eq!(infer_one("(real-part 1+2i)"), "number?");
        assert_eq!(infer_one("(sqrt 4)"), "number?");
        assert_eq!(infer_one("(expt 2 3)"), "number?");
        assert_eq!(
            infer_one("(lambda (x) (if (integer? x) (+ x 1) 0))"),
            "(-> number? number?)"
        );
    }
}
