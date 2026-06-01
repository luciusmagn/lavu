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

    #[error("expected a procedure, got {actual}")]
    ExpectedProcedure { actual: Box<Type>, span: SourceSpan },

    #[error("wrong number of arguments: expected {expected}, got {actual}")]
    ArityMismatch {
        expected: String,
        actual: usize,
        span: SourceSpan,
    },

    #[error("type mismatch: expected {expected}, got {actual}")]
    Mismatch {
        expected: Box<Type>,
        actual: Box<Type>,
        span: SourceSpan,
    },
}

#[derive(Debug, Clone, Default)]
pub struct TypeEnv {
    bindings: HashMap<String, TypeBinding>,
}

#[derive(Debug, Clone)]
struct TypeBinding {
    ty: Type,
    scheme: bool,
}

impl TypeEnv {
    pub fn new() -> Self {
        let mut env = Self::default();
        for primitive in crate::stdlib::r5rs_primitives() {
            env.define_scheme(primitive.name, primitive.signature);
        }
        env
    }

    pub fn define(&mut self, name: impl Into<String>, ty: Type) {
        self.bindings.insert(name.into(), TypeBinding::monotype(ty));
    }

    fn define_inferred(&mut self, name: impl Into<String>, ty: Type) {
        if has_type_var(&ty) {
            self.define_scheme(name, ty);
        } else {
            self.define(name, ty);
        }
    }

    pub fn get(&self, name: &str) -> Option<&Type> {
        self.binding(name).map(|binding| &binding.ty)
    }

    fn define_scheme(&mut self, name: impl Into<String>, ty: Type) {
        self.bindings.insert(name.into(), TypeBinding::scheme(ty));
    }

    fn binding(&self, name: &str) -> Option<&TypeBinding> {
        self.bindings.get(name)
    }
}

impl TypeBinding {
    fn monotype(ty: Type) -> Self {
        Self { ty, scheme: false }
    }

    fn scheme(ty: Type) -> Self {
        Self { ty, scheme: true }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Inferencer {
    substitutions: BTreeMap<String, Type>,
    next_var: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HigherOrderListResult {
    Mapped,
    Unspecified,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ListAccessResult {
    Element,
    Tail,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MembershipResult {
    Tail,
    Entry,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum AppendTail {
    Proper(Option<Type>),
    Improper,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ConstructorKind {
    List,
    Vector,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum PrimitiveApplication {
    Apply,
    Values,
    CallWithValues,
    CallCc,
    Map,
    ForEach,
    List,
    MakeVector,
    Vector,
    ListRef,
    ListTail,
    Car,
    Cdr,
    Append,
    Member,
    Assoc,
    ComposedAccessor(Vec<ListAccessResult>),
}

impl PrimitiveApplication {
    fn classify(expr: &Expr) -> Option<Self> {
        let Expr::Variable(name) = expr else {
            return None;
        };

        match name.as_str() {
            "apply" => Some(Self::Apply),
            "values" => Some(Self::Values),
            "call-with-values" => Some(Self::CallWithValues),
            "call/cc" | "call-with-current-continuation" => Some(Self::CallCc),
            "map" => Some(Self::Map),
            "for-each" => Some(Self::ForEach),
            "list" => Some(Self::List),
            "make-vector" => Some(Self::MakeVector),
            "vector" => Some(Self::Vector),
            "list-ref" => Some(Self::ListRef),
            "list-tail" => Some(Self::ListTail),
            "car" => Some(Self::Car),
            "cdr" => Some(Self::Cdr),
            "append" => Some(Self::Append),
            "memq" | "memv" | "member" => Some(Self::Member),
            "assq" | "assv" | "assoc" => Some(Self::Assoc),
            _ => composed_accessor_steps(name).map(Self::ComposedAccessor),
        }
    }
}

fn composed_accessor_steps(name: &str) -> Option<Vec<ListAccessResult>> {
    let middle = name.strip_prefix('c')?.strip_suffix('r')?;
    if !(2..=4).contains(&middle.len()) {
        return None;
    }

    middle
        .chars()
        .rev()
        .map(|ch| match ch {
            'a' => Some(ListAccessResult::Element),
            'd' => Some(ListAccessResult::Tail),
            _ => None,
        })
        .collect()
}

fn constructor_kind(expr: &Spanned<Expr>) -> Option<ConstructorKind> {
    let Expr::Variable(name) = &expr.node else {
        return None;
    };

    match name.as_str() {
        "list" => Some(ConstructorKind::List),
        "vector" => Some(ConstructorKind::Vector),
        _ => None,
    }
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
                let recursive_seed = matches!(value.node, Expr::Lambda { .. }).then(|| {
                    let seed = self.fresh_type_var();
                    env.define(name.node.clone(), seed.clone());
                    seed
                });
                let ty = self.infer_expr(value, env)?;
                let ty = match recursive_seed {
                    Some(seed) => self.unify(ty, seed, value.span.clone())?,
                    None => ty,
                };
                let ty = self.resolve(ty);
                env.define_inferred(name.node.clone(), ty.clone());
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
            Expr::Variable(name) => self.infer_variable(name, expr.span.clone(), env),
            Expr::Quote(datum) => Ok(type_of_datum(datum)),
            Expr::Quasiquote(datum) => Ok(type_of_quasiquote_datum(datum)),
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
            Expr::Delay(expr) => Ok(Type::PromiseOf(Box::new(self.infer_expr(expr, env)?))),
            Expr::LetRec { bindings, body } => self.infer_letrec(bindings, body, env),
            Expr::Apply { operator, operands } => {
                if let Some((condition, alternate)) = desugared_or_operands(operator, operands) {
                    return self.infer_desugared_or(condition, alternate, env);
                }

                let operator_ty = self.infer_expr(operator, env)?;
                let operand_tys = operands
                    .iter()
                    .map(|operand| self.infer_expr(operand, env))
                    .collect::<Result<Vec<_>, _>>()?;

                if let Some(application) = PrimitiveApplication::classify(&operator.node) {
                    return self.infer_primitive_application(
                        application,
                        operands,
                        operand_tys,
                        expr.span.clone(),
                    );
                }

                self.infer_application(operator_ty, operands, operand_tys, expr.span.clone())
            }
        }
    }

    fn infer_desugared_or(
        &mut self,
        condition: &Spanned<Expr>,
        alternate: &Spanned<Expr>,
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        let truth = Spanned {
            node: Expr::Literal(Atom::Boolean(true)),
            span: condition.span.clone(),
            origin: condition.origin,
        };
        self.infer_if(condition, &truth, Some(alternate), env)
    }

    fn infer_primitive_application(
        &mut self,
        application: PrimitiveApplication,
        operands: &[Spanned<Expr>],
        operand_tys: Vec<Type>,
        span: SourceSpan,
    ) -> Result<Type, TypeError> {
        match application {
            PrimitiveApplication::Apply => self.infer_apply_primitive(operands, operand_tys, span),
            PrimitiveApplication::Values => Ok(Type::Values(
                operand_tys
                    .into_iter()
                    .map(|ty| self.resolve(ty))
                    .collect::<Vec<_>>(),
            )),
            PrimitiveApplication::CallWithValues => {
                self.infer_call_with_values(operands, operand_tys, span)
            }
            PrimitiveApplication::CallCc => self.infer_call_cc(operands, operand_tys, span),
            PrimitiveApplication::Map => self.infer_higher_order_list(
                operands,
                operand_tys,
                span,
                HigherOrderListResult::Mapped,
            ),
            PrimitiveApplication::ForEach => self.infer_higher_order_list(
                operands,
                operand_tys,
                span,
                HigherOrderListResult::Unspecified,
            ),
            PrimitiveApplication::List => Ok(self.infer_list_constructor(operand_tys)),
            PrimitiveApplication::MakeVector => self.infer_make_vector(operands, operand_tys, span),
            PrimitiveApplication::Vector => Ok(self.infer_vector_constructor(operand_tys)),
            PrimitiveApplication::ListRef => {
                self.infer_indexed_list(operands, operand_tys, span, ListAccessResult::Element)
            }
            PrimitiveApplication::ListTail => {
                self.infer_indexed_list(operands, operand_tys, span, ListAccessResult::Tail)
            }
            PrimitiveApplication::Car => {
                self.infer_pair_accessor(operands, operand_tys, span, ListAccessResult::Element)
            }
            PrimitiveApplication::Cdr => {
                self.infer_pair_accessor(operands, operand_tys, span, ListAccessResult::Tail)
            }
            PrimitiveApplication::Append => self.infer_append(operands, operand_tys),
            PrimitiveApplication::Member => {
                self.infer_membership(operands, operand_tys, span, MembershipResult::Tail)
            }
            PrimitiveApplication::Assoc => {
                self.infer_membership(operands, operand_tys, span, MembershipResult::Entry)
            }
            PrimitiveApplication::ComposedAccessor(steps) => {
                self.infer_composed_accessor(operands, operand_tys, span, &steps)
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
        self.next_var = self
            .next_var
            .max(then_inferencer.next_var)
            .max(else_inferencer.next_var);

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
            let mut then_ty = then_inferencer
                .substitutions
                .get(&name)
                .cloned()
                .map(|ty| then_inferencer.resolve(ty));
            let mut else_ty = else_inferencer
                .substitutions
                .get(&name)
                .cloned()
                .map(|ty| else_inferencer.resolve(ty));

            if let Some(refinement) = refinement.filter(|refinement| name == refinement.name) {
                match refinement.branch {
                    RefinedBranch::Then
                        if then_ty.is_none()
                            && (else_ty.is_some()
                                || expr_mentions_variable(consequent, &refinement.name)) =>
                    {
                        then_ty = Some(refinement.positive.clone());
                    }
                    RefinedBranch::Else
                        if else_ty.is_none()
                            && (then_ty.is_some()
                                || alternate.is_some_and(|expr| {
                                    expr_mentions_variable(expr, &refinement.name)
                                })) =>
                    {
                        else_ty = Some(refinement.positive.clone());
                    }
                    _ => {}
                }
            }

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
            actual => Err(TypeError::ExpectedProcedure {
                actual: Box::new(actual),
                span,
            }),
        }
    }

    fn infer_variable(
        &mut self,
        name: &str,
        span: SourceSpan,
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        let binding = env
            .binding(name)
            .ok_or_else(|| TypeError::UnboundVariable {
                name: name.to_string(),
                span,
            })?;
        let ty = if binding.scheme {
            self.instantiate_scheme(&binding.ty)
        } else {
            binding.ty.clone()
        };

        Ok(self.resolve(ty))
    }

    fn instantiate_scheme(&mut self, ty: &Type) -> Type {
        let mut vars = BTreeMap::new();
        self.instantiate_scheme_type(ty, &mut vars)
    }

    fn instantiate_scheme_type(&mut self, ty: &Type, vars: &mut BTreeMap<String, Type>) -> Type {
        match ty {
            Type::Var(name) => {
                if let Some(fresh) = vars.get(name) {
                    fresh.clone()
                } else {
                    let fresh = self.fresh_type_var();
                    vars.insert(name.clone(), fresh.clone());
                    fresh
                }
            }
            Type::Pair(car, cdr) => Type::Pair(
                Box::new(self.instantiate_scheme_type(car, vars)),
                Box::new(self.instantiate_scheme_type(cdr, vars)),
            ),
            Type::ListOf(element) => {
                Type::ListOf(Box::new(self.instantiate_scheme_type(element, vars)))
            }
            Type::VectorOf(element) => {
                Type::VectorOf(Box::new(self.instantiate_scheme_type(element, vars)))
            }
            Type::PromiseOf(element) => {
                Type::PromiseOf(Box::new(self.instantiate_scheme_type(element, vars)))
            }
            Type::Values(types) => Type::Values(
                types
                    .iter()
                    .map(|ty| self.instantiate_scheme_type(ty, vars))
                    .collect(),
            ),
            Type::Procedure(ProcedureType::Fixed { params, result }) => Type::procedure(
                params
                    .iter()
                    .map(|ty| self.instantiate_scheme_type(ty, vars))
                    .collect::<Vec<_>>(),
                self.instantiate_scheme_type(result, vars),
            ),
            Type::Procedure(ProcedureType::Optional {
                required,
                optional,
                result,
            }) => Type::optional_procedure(
                required
                    .iter()
                    .map(|ty| self.instantiate_scheme_type(ty, vars))
                    .collect::<Vec<_>>(),
                optional
                    .iter()
                    .map(|ty| self.instantiate_scheme_type(ty, vars))
                    .collect::<Vec<_>>(),
                self.instantiate_scheme_type(result, vars),
            ),
            Type::Procedure(ProcedureType::UniformVariadic { param, result }) => {
                Type::uniform_variadic(
                    self.instantiate_scheme_type(param, vars),
                    self.instantiate_scheme_type(result, vars),
                )
            }
            Type::Procedure(ProcedureType::Rest {
                required,
                rest,
                result,
            }) => Type::rest_procedure(
                required
                    .iter()
                    .map(|ty| self.instantiate_scheme_type(ty, vars))
                    .collect::<Vec<_>>(),
                self.instantiate_scheme_type(rest, vars),
                self.instantiate_scheme_type(result, vars),
            ),
            Type::Union(types) => Type::union(
                types
                    .iter()
                    .map(|ty| self.instantiate_scheme_type(ty, vars))
                    .collect::<Vec<_>>(),
            ),
            ty => ty.clone(),
        }
    }

    fn infer_call_with_values(
        &mut self,
        operands: &[Spanned<Expr>],
        operand_tys: Vec<Type>,
        span: SourceSpan,
    ) -> Result<Type, TypeError> {
        let [producer_ty, consumer_ty]: [Type; 2] =
            operand_tys
                .try_into()
                .map_err(|operand_tys: Vec<Type>| TypeError::ArityMismatch {
                    expected: "2".to_string(),
                    actual: operand_tys.len(),
                    span: span.clone(),
                })?;

        let produced = match self.resolve(producer_ty) {
            Type::Procedure(ProcedureType::Fixed { params, result }) if params.is_empty() => {
                *result
            }
            Type::Procedure(ProcedureType::Rest {
                required,
                rest: _,
                result,
            }) if required.is_empty() => *result,
            _ => Type::Any,
        };
        let value_tys = match self.resolve(produced) {
            Type::Values(values) => values,
            value => vec![value],
        };
        let value_operands = value_tys
            .iter()
            .map(|_| operands[0].clone())
            .collect::<Vec<_>>();

        self.infer_application(consumer_ty, &value_operands, value_tys, span)
    }

    fn infer_call_cc(
        &mut self,
        operands: &[Spanned<Expr>],
        operand_tys: Vec<Type>,
        span: SourceSpan,
    ) -> Result<Type, TypeError> {
        let [receiver_ty]: [Type; 1] =
            operand_tys
                .try_into()
                .map_err(|operand_tys: Vec<Type>| TypeError::ArityMismatch {
                    expected: "1".to_string(),
                    actual: operand_tys.len(),
                    span: span.clone(),
                })?;

        match self.resolve(receiver_ty) {
            Type::Procedure(receiver) => self.infer_call_cc_receiver(receiver, operands),
            actual => {
                self.unify(actual, call_cc_receiver_type(), operands[0].span.clone())?;
                Ok(Type::Any)
            }
        }
    }

    fn infer_call_cc_receiver(
        &mut self,
        receiver: ProcedureType,
        operands: &[Spanned<Expr>],
    ) -> Result<Type, TypeError> {
        let escape_seed = self.fresh_type_var();
        let continuation = Type::procedure(vec![escape_seed.clone()], Type::Never);
        let direct = self.apply_procedure(receiver, operands, vec![continuation])?;
        let direct = self.resolve(direct);
        let escape = self.resolve(escape_seed.clone());

        if same_type_var(&escape, &escape_seed) {
            return Ok(direct);
        }

        Ok(if direct == Type::Never {
            escape
        } else {
            Type::union(vec![direct, escape])
        })
    }

    fn infer_list_constructor(&self, operand_tys: Vec<Type>) -> Type {
        match operand_tys.as_slice() {
            [] => Type::Null,
            _ => Type::ListOf(Box::new(self.union_resolved(operand_tys))),
        }
    }

    fn infer_make_vector(
        &mut self,
        operands: &[Spanned<Expr>],
        operand_tys: Vec<Type>,
        span: SourceSpan,
    ) -> Result<Type, TypeError> {
        match operand_tys.as_slice() {
            [length] => {
                self.unify(length.clone(), Type::Number, operands[0].span.clone())?;
                Ok(Type::VectorOf(Box::new(Type::Any)))
            }
            [length, fill] => {
                self.unify(length.clone(), Type::Number, operands[0].span.clone())?;
                Ok(Type::VectorOf(Box::new(self.resolve(fill.clone()))))
            }
            _ => Err(TypeError::ArityMismatch {
                expected: "1 to 2".to_string(),
                actual: operand_tys.len(),
                span,
            }),
        }
    }

    fn infer_vector_constructor(&self, operand_tys: Vec<Type>) -> Type {
        match operand_tys.as_slice() {
            [] => Type::Vector,
            _ => Type::VectorOf(Box::new(self.union_resolved(operand_tys))),
        }
    }

    fn union_resolved(&self, types: Vec<Type>) -> Type {
        Type::union(
            types
                .into_iter()
                .map(|ty| self.resolve(ty))
                .collect::<Vec<_>>(),
        )
    }

    fn infer_apply_constructor(
        &mut self,
        constructor: ConstructorKind,
        fixed_argument_tys: Vec<Type>,
        final_list_ty: Type,
        final_operand: &Spanned<Expr>,
    ) -> Result<Type, TypeError> {
        let mut element_tys = fixed_argument_tys
            .into_iter()
            .map(|ty| self.resolve(ty))
            .collect::<Vec<_>>();
        element_tys
            .extend(self.infer_apply_constructor_final_elements(final_list_ty, final_operand)?);

        Ok(match constructor {
            ConstructorKind::List => self.infer_list_constructor(element_tys),
            ConstructorKind::Vector => self.infer_vector_constructor(element_tys),
        })
    }

    fn infer_apply_constructor_final_elements(
        &mut self,
        actual: Type,
        operand: &Spanned<Expr>,
    ) -> Result<Vec<Type>, TypeError> {
        if let Some(types) = quoted_proper_list_types(operand) {
            return Ok(types);
        }

        match self.resolve(actual) {
            Type::Null => Ok(Vec::new()),
            Type::ListOf(element) => Ok(vec![self.resolve(*element)]),
            Type::List | Type::Any | Type::Unknown => Ok(vec![Type::Any]),
            Type::Var(name) => {
                let element = self.fresh_type_var();
                self.substitutions
                    .insert(name, Type::ListOf(Box::new(element.clone())));
                Ok(vec![element])
            }
            actual => {
                self.unify(actual, Type::List, operand.span.clone())?;
                Ok(vec![Type::Any])
            }
        }
    }

    fn infer_higher_order_list(
        &mut self,
        operands: &[Spanned<Expr>],
        operand_tys: Vec<Type>,
        span: SourceSpan,
        result: HigherOrderListResult,
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
        let element_tys = operand_tys
            .zip(&operands[1..])
            .map(|(ty, operand)| self.infer_list_element_type(ty, operand))
            .collect::<Result<Vec<_>, _>>()?;
        if let Some(ty) = self.infer_constructor_mapper(&operands[0], &element_tys, result) {
            return Ok(ty);
        }
        let mapped_ty =
            self.infer_application(procedure_ty, &operands[1..], element_tys, span.clone())?;

        match result {
            HigherOrderListResult::Mapped => Ok(Type::ListOf(Box::new(self.resolve(mapped_ty)))),
            HigherOrderListResult::Unspecified => Ok(Type::Unknown),
        }
    }

    fn infer_constructor_mapper(
        &self,
        procedure: &Spanned<Expr>,
        element_tys: &[Type],
        result: HigherOrderListResult,
    ) -> Option<Type> {
        let mapped = match constructor_kind(procedure)? {
            ConstructorKind::List => self.infer_list_constructor(element_tys.to_vec()),
            ConstructorKind::Vector => self.infer_vector_constructor(element_tys.to_vec()),
        };

        Some(match result {
            HigherOrderListResult::Mapped => Type::ListOf(Box::new(mapped)),
            HigherOrderListResult::Unspecified => Type::Unknown,
        })
    }

    fn infer_list_element_type(
        &mut self,
        actual: Type,
        operand: &Spanned<Expr>,
    ) -> Result<Type, TypeError> {
        match self.resolve(actual) {
            Type::ListOf(element) => Ok(self.resolve(*element)),
            Type::Null | Type::List | Type::Any | Type::Unknown => Ok(Type::Any),
            Type::Var(name) => {
                let element = self.fresh_type_var();
                self.substitutions
                    .insert(name, Type::ListOf(Box::new(element.clone())));
                Ok(element)
            }
            actual => {
                self.unify(actual, Type::List, operand.span.clone())?;
                Ok(Type::Any)
            }
        }
    }

    fn infer_append(
        &mut self,
        operands: &[Spanned<Expr>],
        operand_tys: Vec<Type>,
    ) -> Result<Type, TypeError> {
        let Some((last_ty, leading_tys)) = operand_tys.split_last() else {
            return Ok(Type::Null);
        };

        let mut elements = leading_tys
            .iter()
            .cloned()
            .zip(operands)
            .filter_map(|(ty, operand)| self.infer_append_list_element(ty, operand).transpose())
            .collect::<Result<Vec<_>, _>>()?;

        match self.infer_append_tail_elements(last_ty.clone()) {
            AppendTail::Proper(element) => {
                if let Some(element) = element {
                    elements.push(element);
                }
                Ok(match elements.as_slice() {
                    [] => Type::Null,
                    _ => Type::ListOf(Box::new(Type::union(elements))),
                })
            }
            AppendTail::Improper => Ok(Type::Any),
        }
    }

    fn infer_append_list_element(
        &mut self,
        actual: Type,
        operand: &Spanned<Expr>,
    ) -> Result<Option<Type>, TypeError> {
        match self.resolve(actual) {
            Type::Null => Ok(None),
            actual => self
                .infer_list_element_type(actual, operand)
                .map(|element| Some(self.resolve(element))),
        }
    }

    fn infer_append_tail_elements(&self, actual: Type) -> AppendTail {
        match self.resolve(actual) {
            Type::Null => AppendTail::Proper(None),
            Type::ListOf(element) => AppendTail::Proper(Some(*element)),
            Type::List => AppendTail::Proper(Some(Type::Any)),
            _ => AppendTail::Improper,
        }
    }

    fn infer_indexed_list(
        &mut self,
        operands: &[Spanned<Expr>],
        operand_tys: Vec<Type>,
        span: SourceSpan,
        result: ListAccessResult,
    ) -> Result<Type, TypeError> {
        let [list_ty, index_ty]: [Type; 2] =
            operand_tys
                .try_into()
                .map_err(|operand_tys: Vec<Type>| TypeError::ArityMismatch {
                    expected: "2".to_string(),
                    actual: operand_tys.len(),
                    span,
                })?;

        self.unify(index_ty, Type::Number, operands[1].span.clone())?;
        let element = self.infer_list_element_type(list_ty, &operands[0])?;

        match result {
            ListAccessResult::Element => Ok(self.resolve(element)),
            ListAccessResult::Tail => Ok(Type::ListOf(Box::new(self.resolve(element)))),
        }
    }

    fn infer_pair_accessor(
        &mut self,
        operands: &[Spanned<Expr>],
        operand_tys: Vec<Type>,
        span: SourceSpan,
        result: ListAccessResult,
    ) -> Result<Type, TypeError> {
        let [operand_ty]: [Type; 1] =
            operand_tys
                .try_into()
                .map_err(|operand_tys: Vec<Type>| TypeError::ArityMismatch {
                    expected: "1".to_string(),
                    actual: operand_tys.len(),
                    span,
                })?;

        self.infer_access_type(operand_ty, &operands[0], result)
    }

    fn infer_composed_accessor(
        &mut self,
        operands: &[Spanned<Expr>],
        operand_tys: Vec<Type>,
        span: SourceSpan,
        steps: &[ListAccessResult],
    ) -> Result<Type, TypeError> {
        let [operand_ty]: [Type; 1] =
            operand_tys
                .try_into()
                .map_err(|operand_tys: Vec<Type>| TypeError::ArityMismatch {
                    expected: "1".to_string(),
                    actual: operand_tys.len(),
                    span,
                })?;

        steps.iter().try_fold(operand_ty, |ty, step| {
            self.infer_access_type(ty, &operands[0], *step)
        })
    }

    fn infer_access_type(
        &mut self,
        actual: Type,
        operand: &Spanned<Expr>,
        result: ListAccessResult,
    ) -> Result<Type, TypeError> {
        match self.resolve(actual) {
            Type::Pair(car, cdr) => match result {
                ListAccessResult::Element => Ok(self.resolve(*car)),
                ListAccessResult::Tail => Ok(self.resolve(*cdr)),
            },
            Type::ListOf(element) => match result {
                ListAccessResult::Element => Ok(self.resolve(*element)),
                ListAccessResult::Tail => Ok(Type::ListOf(Box::new(self.resolve(*element)))),
            },
            Type::List => match result {
                ListAccessResult::Element => Ok(Type::Any),
                ListAccessResult::Tail => Ok(Type::List),
            },
            Type::Any | Type::Unknown => Ok(Type::Any),
            Type::Var(name) => {
                let car = self.fresh_type_var();
                let cdr = self.fresh_type_var();
                self.substitutions.insert(
                    name,
                    Type::Pair(Box::new(car.clone()), Box::new(cdr.clone())),
                );

                match result {
                    ListAccessResult::Element => Ok(car),
                    ListAccessResult::Tail => Ok(cdr),
                }
            }
            actual => {
                self.unify(
                    actual,
                    Type::Pair(Box::new(Type::Any), Box::new(Type::Any)),
                    operand.span.clone(),
                )?;
                Ok(Type::Any)
            }
        }
    }

    fn infer_membership(
        &mut self,
        operands: &[Spanned<Expr>],
        operand_tys: Vec<Type>,
        span: SourceSpan,
        result: MembershipResult,
    ) -> Result<Type, TypeError> {
        let [_target_ty, list_ty]: [Type; 2] =
            operand_tys
                .try_into()
                .map_err(|operand_tys: Vec<Type>| TypeError::ArityMismatch {
                    expected: "2".to_string(),
                    actual: operand_tys.len(),
                    span,
                })?;

        let success = self.infer_membership_success(list_ty, &operands[1], result)?;
        Ok(match success {
            Type::Null | Type::Never => Type::Boolean,
            Type::Any | Type::Unknown => Type::Any,
            success => Type::union(vec![Type::Boolean, self.resolve(success)]),
        })
    }

    fn infer_membership_success(
        &mut self,
        actual: Type,
        operand: &Spanned<Expr>,
        result: MembershipResult,
    ) -> Result<Type, TypeError> {
        match self.resolve(actual) {
            Type::ListOf(element) => Ok(match result {
                MembershipResult::Tail => Type::ListOf(element),
                MembershipResult::Entry => self.resolve(*element),
            }),
            Type::Null => Ok(Type::Never),
            Type::List => Ok(match result {
                MembershipResult::Tail => Type::List,
                MembershipResult::Entry => Type::Any,
            }),
            Type::Var(name) => {
                let element = self.fresh_type_var();
                self.substitutions
                    .insert(name, Type::ListOf(Box::new(element.clone())));
                Ok(match result {
                    MembershipResult::Tail => Type::ListOf(Box::new(element)),
                    MembershipResult::Entry => element,
                })
            }
            Type::Any | Type::Unknown => Ok(Type::Any),
            actual => {
                self.unify(actual, Type::List, operand.span.clone())?;
                Ok(match result {
                    MembershipResult::Tail => Type::List,
                    MembershipResult::Entry => Type::Any,
                })
            }
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

        if let Some(constructor) = constructor_kind(&operands[0]) {
            return self.infer_apply_constructor(constructor, arguments, final_list, final_operand);
        }

        match self.resolve(procedure_ty) {
            Type::Procedure(
                procedure @ (ProcedureType::Fixed { .. } | ProcedureType::Optional { .. }),
            ) => {
                let Some(final_arguments) = quoted_proper_list_types(final_operand) else {
                    return Ok(Type::Any);
                };
                self.unify(final_list, Type::List, final_operand.span.clone())?;

                let mut combined_tys = arguments;
                combined_tys.extend(final_arguments.iter().cloned());
                let mut combined_operands = fixed_operands.to_vec();
                combined_operands.extend(final_arguments.iter().map(|_| final_operand.clone()));

                self.apply_procedure(procedure, &combined_operands, combined_tys)
            }
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
            ProcedureType::Optional {
                required,
                optional,
                result,
            } => {
                let maximum = required.len() + optional.len();
                if operand_tys.len() < required.len() || operand_tys.len() > maximum {
                    return Err(TypeError::ArityMismatch {
                        expected: format!("{} to {}", required.len(), maximum),
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

                for ((actual, expected), operand) in operand_tys
                    .into_iter()
                    .skip(required.len())
                    .zip(optional)
                    .zip(operands.iter().skip(required.len()))
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
            (Type::Null, Type::ListOf(_)) | (Type::ListOf(_), Type::Null) => Ok(Type::Null),
            (Type::ListOf(actual), Type::ListOf(expected)) => self.unify(*actual, *expected, span),
            (Type::VectorOf(_), Type::Vector) | (Type::Vector, Type::VectorOf(_)) => {
                Ok(Type::Vector)
            }
            (Type::VectorOf(actual), Type::VectorOf(expected)) => {
                self.unify(*actual, *expected, span)
            }
            (Type::PromiseOf(actual), Type::PromiseOf(expected)) => {
                self.unify(*actual, *expected, span)
            }
            (Type::Values(actual), Type::Values(expected)) => {
                if actual.len() != expected.len() {
                    return Err(TypeError::ArityMismatch {
                        expected: expected.len().to_string(),
                        actual: actual.len(),
                        span,
                    });
                }

                actual
                    .into_iter()
                    .zip(expected)
                    .map(|(actual, expected)| self.unify(actual, expected, span.clone()))
                    .collect::<Result<Vec<_>, _>>()
                    .map(Type::Values)
            }
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
                expected: Box::new(expected),
                actual: Box::new(actual),
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
                ProcedureType::Optional {
                    required: actual_required,
                    optional: actual_optional,
                    result: actual_result,
                },
                ProcedureType::Fixed {
                    params: expected_params,
                    result: expected_result,
                },
            ) => {
                let maximum = actual_required.len() + actual_optional.len();
                if expected_params.len() < actual_required.len() || expected_params.len() > maximum
                {
                    return Err(TypeError::ArityMismatch {
                        expected: format!("{} to {}", actual_required.len(), maximum),
                        actual: expected_params.len(),
                        span,
                    });
                }

                let actual_params = actual_required
                    .into_iter()
                    .chain(actual_optional)
                    .take(expected_params.len());
                let params = actual_params
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
                ProcedureType::Fixed {
                    params: expected_params,
                    result: expected_result,
                },
            ) => {
                let params = expected_params
                    .into_iter()
                    .map(|expected| self.unify((*actual_param).clone(), expected, span.clone()))
                    .collect::<Result<Vec<_>, _>>()?;
                let result = self.unify(*actual_result, *expected_result, span)?;

                Ok(Type::procedure(params, result))
            }
            (
                ProcedureType::Rest {
                    required: actual_required,
                    rest: actual_rest,
                    result: actual_result,
                },
                ProcedureType::Fixed {
                    params: expected_params,
                    result: expected_result,
                },
            ) => {
                if expected_params.len() < actual_required.len() {
                    return Err(TypeError::ArityMismatch {
                        expected: format!("at least {}", actual_required.len()),
                        actual: expected_params.len(),
                        span,
                    });
                }

                let required_len = actual_required.len();
                let required = actual_required
                    .into_iter()
                    .zip(expected_params.iter().cloned())
                    .map(|(actual, expected)| self.unify(actual, expected, span.clone()))
                    .collect::<Result<Vec<_>, _>>()?;
                let rest = expected_params
                    .into_iter()
                    .skip(required_len)
                    .map(|expected| self.unify((*actual_rest).clone(), expected, span.clone()))
                    .collect::<Result<Vec<_>, _>>()?;
                let result = self.unify(*actual_result, *expected_result, span)?;

                Ok(Type::procedure(
                    required.into_iter().chain(rest).collect::<Vec<_>>(),
                    result,
                ))
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
                expected: Box::new(Type::Procedure(expected)),
                actual: Box::new(Type::Procedure(actual)),
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
            Type::VectorOf(item) => Type::VectorOf(Box::new(self.resolve(*item))),
            Type::PromiseOf(item) => Type::PromiseOf(Box::new(self.resolve(*item))),
            Type::Pair(car, cdr) => {
                Type::Pair(Box::new(self.resolve(*car)), Box::new(self.resolve(*cdr)))
            }
            Type::Values(types) => Type::Values(
                types
                    .into_iter()
                    .map(|ty| self.resolve(ty))
                    .collect::<Vec<_>>(),
            ),
            Type::Procedure(ProcedureType::Fixed { params, result }) => Type::procedure(
                params
                    .into_iter()
                    .map(|ty| self.resolve(ty))
                    .collect::<Vec<_>>(),
                self.resolve(*result),
            ),
            Type::Procedure(ProcedureType::Optional {
                required,
                optional,
                result,
            }) => Type::optional_procedure(
                required
                    .into_iter()
                    .map(|ty| self.resolve(ty))
                    .collect::<Vec<_>>(),
                optional
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
        Type::ListOf(element) | Type::VectorOf(element) | Type::PromiseOf(element) => {
            contains_var(element, name)
        }
        Type::Values(types) => types.iter().any(|ty| contains_var(ty, name)),
        Type::Procedure(ProcedureType::Fixed { params, result }) => {
            params.iter().any(|ty| contains_var(ty, name)) || contains_var(result, name)
        }
        Type::Procedure(ProcedureType::Optional {
            required,
            optional,
            result,
        }) => {
            required.iter().any(|ty| contains_var(ty, name))
                || optional.iter().any(|ty| contains_var(ty, name))
                || contains_var(result, name)
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

fn has_type_var(ty: &Type) -> bool {
    match ty {
        Type::Var(_) => true,
        Type::Pair(car, cdr) => has_type_var(car) || has_type_var(cdr),
        Type::ListOf(element) | Type::VectorOf(element) | Type::PromiseOf(element) => {
            has_type_var(element)
        }
        Type::Values(types) | Type::Union(types) => types.iter().any(has_type_var),
        Type::Procedure(ProcedureType::Fixed { params, result }) => {
            params.iter().any(has_type_var) || has_type_var(result)
        }
        Type::Procedure(ProcedureType::Optional {
            required,
            optional,
            result,
        }) => {
            required.iter().any(has_type_var)
                || optional.iter().any(has_type_var)
                || has_type_var(result)
        }
        Type::Procedure(ProcedureType::UniformVariadic { param, result }) => {
            has_type_var(param) || has_type_var(result)
        }
        Type::Procedure(ProcedureType::Rest {
            required,
            rest,
            result,
        }) => required.iter().any(has_type_var) || has_type_var(rest) || has_type_var(result),
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

fn call_cc_receiver_type() -> Type {
    Type::procedure(vec![Type::procedure(vec![Type::Any], Type::Any)], Type::Any)
}

fn same_type_var(left: &Type, right: &Type) -> bool {
    matches!((left, right), (Type::Var(left), Type::Var(right)) if left == right)
}

fn quoted_proper_list_types(expr: &Spanned<Expr>) -> Option<Vec<Type>> {
    let Expr::Quote(datum) = &expr.node else {
        return None;
    };
    let Datum::List(items) = &datum.node else {
        return None;
    };

    Some(items.iter().map(type_of_datum).collect())
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
        Datum::Vector(items) => type_of_vector_datums(items),
        Datum::Quote(inner) => abbreviation_datum_type("quote", inner),
        Datum::Quasiquote(inner) => abbreviation_datum_type("quasiquote", inner),
        Datum::Unquote(inner) => abbreviation_datum_type("unquote", inner),
        Datum::UnquoteSplicing(inner) => abbreviation_datum_type("unquote-splicing", inner),
    }
}

fn type_of_quasiquote_datum(datum: &Spanned<Datum>) -> Type {
    if datum_contains_unquote(datum) {
        Type::Any
    } else {
        type_of_datum(datum)
    }
}

fn datum_contains_unquote(datum: &Spanned<Datum>) -> bool {
    match &datum.node {
        Datum::List(items) | Datum::Vector(items) => items.iter().any(datum_contains_unquote),
        Datum::DottedList(items, tail) => {
            items.iter().any(datum_contains_unquote) || datum_contains_unquote(tail)
        }
        Datum::Quote(inner) | Datum::Quasiquote(inner) => datum_contains_unquote(inner),
        Datum::Unquote(_) | Datum::UnquoteSplicing(_) => true,
        Datum::Atom(_) => false,
    }
}

fn type_of_vector_datums(items: &[Spanned<Datum>]) -> Type {
    if items.is_empty() {
        return Type::Vector;
    }

    Type::VectorOf(Box::new(Type::union(
        items.iter().map(type_of_datum).collect::<Vec<_>>(),
    )))
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

fn desugared_or_operands<'a>(
    operator: &'a Spanned<Expr>,
    operands: &'a [Spanned<Expr>],
) -> Option<(&'a Spanned<Expr>, &'a Spanned<Expr>)> {
    let [condition] = operands else {
        return None;
    };
    direct_predicate_refinement(condition)?;

    let Expr::Lambda { params, rest, body } = &operator.node else {
        return None;
    };
    let [param] = params.as_slice() else {
        return None;
    };
    if rest.is_some() || body.len() != 1 || !param.node.starts_with("__lavu_or_value") {
        return None;
    }

    let Expr::If {
        condition: if_condition,
        consequent,
        alternate: Some(alternate),
    } = &body[0].node
    else {
        return None;
    };
    if variable_name(if_condition).map(String::as_str) != Some(param.node.as_str())
        || variable_name(consequent).map(String::as_str) != Some(param.node.as_str())
    {
        return None;
    }

    Some((condition, alternate))
}

fn variable_name(expr: &Spanned<Expr>) -> Option<&String> {
    match &expr.node {
        Expr::Variable(name) => Some(name),
        _ => None,
    }
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
    use crate::infer::{Inferencer, TypeEnv, TypeError};
    use crate::surface::classify_program;
    use crate::types::Type;

    fn infer_one(input: &str) -> String {
        infer_all(input)[0].clone()
    }

    fn infer_all(input: &str) -> Vec<String> {
        let datums = parse(input).unwrap();
        let program = classify_program(&datums).unwrap();
        let mut env = TypeEnv::new();
        let mut inferencer = Inferencer::new();
        inferencer
            .infer_program(&program, &mut env)
            .unwrap()
            .into_iter()
            .map(|ty| ty.to_string())
            .collect()
    }

    fn infer_error(input: &str) -> TypeError {
        let datums = parse(input).unwrap();
        let program = classify_program(&datums).unwrap();
        let mut env = TypeEnv::new();
        let mut inferencer = Inferencer::new();
        inferencer.infer_program(&program, &mut env).unwrap_err()
    }

    #[test]
    fn displays_type_error_details() {
        assert_eq!(
            infer_error("(+ \"x\" 1)").to_string(),
            "type mismatch: expected number?, got string?"
        );
        assert_eq!(
            infer_error("(car)").to_string(),
            "wrong number of arguments: expected 1, got 0"
        );
        assert_eq!(
            infer_error("(write 1 (current-output-port) (current-output-port))").to_string(),
            "wrong number of arguments: expected 1 to 2, got 3"
        );
        assert_eq!(
            infer_error("(\"x\" 1)").to_string(),
            "expected a procedure, got string?"
        );
    }

    #[test]
    fn infers_primitive_arithmetic_lambda() {
        assert_eq!(infer_one("(lambda (x) (+ x 1))"), "(-> number? number?)");
    }

    #[test]
    fn infers_macro_expanded_expression() {
        assert_eq!(
            infer_one(
                "(define-syntax id
                   (syntax-rules ()
                     ((id x) x)))
                 (id (+ 1 2))"
            ),
            "number?"
        );
    }

    #[test]
    fn infers_internal_syntax_definitions() {
        assert_eq!(
            infer_one(
                "(lambda ()
                   (define-syntax id
                     (syntax-rules ()
                       ((id x) x)))
                   (id (+ 1 2)))"
            ),
            "(-> number?)"
        );
    }

    #[test]
    fn infers_rest_lambda_formals() {
        assert_eq!(infer_one("(lambda args args)"), "(-> args * (listof args))");
        assert_eq!(
            infer_one("(lambda (x . rest) (reverse rest))"),
            "(-> x t0 * (listof t0))"
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
    fn infers_recursive_top_level_procedures() {
        assert_eq!(
            infer_all("(define (count n) (if (= n 0) n (count (- n 1)))) (count 5)"),
            vec!["(-> number? number?)".to_string(), "number?".to_string()]
        );
    }

    #[test]
    fn checks_set_assignment_types() {
        assert_eq!(
            infer_one("(lambda (x) (set! x (+ x 1)) x)"),
            "(-> number? number?)"
        );

        let TypeError::Mismatch {
            expected, actual, ..
        } = infer_error("(define x 1) (set! x \"bad\")")
        else {
            panic!("expected mismatch");
        };
        assert_eq!(*expected, Type::Number);
        assert_eq!(*actual, Type::String);
    }

    #[test]
    fn infers_reverse_lambda_from_primitive_signature() {
        assert_eq!(
            infer_one("(lambda (x) (reverse x))"),
            "(-> (listof t0) (listof t0))"
        );
    }

    #[test]
    fn infers_pair_mutators() {
        assert_eq!(infer_one("(set-car! (cons 1 2) 9)"), "unknown?");
        assert_eq!(infer_one("(set-cdr! (cons 1 2) 9)"), "unknown?");
    }

    #[test]
    fn infers_pair_accessors_over_lists() {
        assert_eq!(infer_one("(car '(1 2))"), "number?");
        assert_eq!(infer_one("(cdr '(1 2))"), "(listof number?)");
        assert_eq!(infer_one("(car (cons \"x\" 2))"), "string?");
        assert_eq!(infer_one("(cdr (cons \"x\" 2))"), "number?");
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
    fn keeps_predicate_only_accepted_branch_inputs() {
        assert_eq!(
            infer_one("(lambda (x) (if (string? x) #t (+ x 1)))"),
            "(-> (U number? string?) (U boolean? number?))"
        );
        assert_eq!(
            infer_one("(lambda (x) (if (not (string? x)) (+ x 1) #t))"),
            "(-> (U number? string?) (U boolean? number?))"
        );
    }

    #[test]
    fn propagates_refinements_through_derived_conditionals() {
        assert_eq!(
            infer_one("(lambda (x) (and (string? x) (string-length x)))"),
            "(-> string? (U boolean? number?))"
        );
        assert_eq!(
            infer_one("(lambda (x) (cond ((string? x) #t) (else (+ x 1))))"),
            "(-> (U number? string?) (U boolean? number?))"
        );
    }

    #[test]
    fn propagates_predicate_refinements_through_or() {
        assert_eq!(
            infer_one("(lambda (x) (or (string? x) (+ x 1)))"),
            "(-> (U number? string?) (U boolean? number?))"
        );
        assert_eq!(
            infer_one("(lambda (x) (or (string? x) #f))"),
            "(-> x boolean?)"
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
    fn infers_delay_and_force() {
        assert_eq!(infer_one("(delay (+ 1 2))"), "(promiseof number?)");
        assert_eq!(infer_one("(force (delay (+ 1 2)))"), "number?");
        assert_eq!(
            infer_one("(lambda (p) (+ (force p) 1))"),
            "(-> (promiseof number?) number?)"
        );
    }

    #[test]
    fn infers_fixed_multiple_values() {
        assert_eq!(infer_one("(values 1 \"x\")"), "(values number? string?)");
        assert_eq!(
            infer_one("(call-with-values (lambda () (values 1 2)) +)"),
            "number?"
        );
        assert_eq!(
            infer_one(
                "(call-with-values (lambda () (values 1 \"xx\")) \
                 (lambda (n s) (+ n (string-length s))))"
            ),
            "number?"
        );
        assert_eq!(
            infer_one("(call-with-values (lambda () (values)) (lambda () 1))"),
            "number?"
        );
    }

    #[test]
    fn infers_quasiquote_conservatively() {
        assert_eq!(infer_one("`(1 2 3)"), "(listof number?)");
        assert_eq!(infer_one("`#(1 \"x\")"), "(vectorof (U number? string?))");
        assert_eq!(infer_one("`(1 ,(+ 1 2))"), "any?");
        assert_eq!(infer_one("`(,@xs)"), "any?");
    }

    #[test]
    fn infers_quoted_datum_shapes() {
        assert_eq!(infer_one("'(1 2 3)"), "(listof number?)");
        assert_eq!(infer_one("'()"), "null?");
        assert_eq!(infer_one("'(1 . \"x\")"), "(pair? number? string?)");
        assert_eq!(infer_one("'(1 \"x\")"), "(listof (U number? string?))");
        assert_eq!(infer_one("'#(1 \"x\")"), "(vectorof (U number? string?))");
        assert_eq!(infer_one("'#()"), "vector?");
    }

    #[test]
    fn infers_simple_apply_calls() {
        assert_eq!(infer_one("(apply + '(1 2 3))"), "number?");
        assert_eq!(infer_one("(apply string-append '(\"a\" \"b\"))"), "string?");
        assert_eq!(infer_one("(apply list '())"), "null?");
        assert_eq!(infer_one("(apply vector '())"), "vector?");
        assert_eq!(
            infer_one("(apply list 1 '(\"x\"))"),
            "(listof (U number? string?))"
        );
        assert_eq!(
            infer_one("(apply vector 1 '(\"x\"))"),
            "(vectorof (U number? string?))"
        );
        assert_eq!(
            infer_one("(apply (lambda (x y) (+ x y)) '(1 2))"),
            "number?"
        );
        assert_eq!(
            infer_error("(apply (lambda (x y) (+ x y)) '(1 2 3))").to_string(),
            "wrong number of arguments: expected 2, got 3"
        );
        assert_eq!(
            infer_one("(apply (lambda (x radix) (number->string x radix)) '(10 16))"),
            "string?"
        );
    }

    #[test]
    fn infers_vector_primitive_types() {
        assert_eq!(infer_one("(vector)"), "vector?");
        assert_eq!(infer_one("(vector 1 2 3)"), "(vectorof number?)");
        assert_eq!(
            infer_one("(vector 1 \"x\")"),
            "(vectorof (U number? string?))"
        );
        assert_eq!(infer_one("(vector-ref (vector 1 2 3) 0)"), "number?");
        assert_eq!(
            infer_one("(vector-ref (vector 1 \"x\") 0)"),
            "(U number? string?)"
        );
        assert_eq!(infer_one("(vector-length (vector 1 2 3))"), "number?");
        assert_eq!(infer_one("(make-vector 3)"), "(vectorof any?)");
        assert_eq!(infer_one("(make-vector 3 #\\a)"), "(vectorof char?)");
        assert_eq!(
            infer_one("(vector->list (vector #\\a #\\b))"),
            "(listof char?)"
        );
        assert_eq!(infer_one("(list->vector '(1 2 3))"), "(vectorof number?)");
    }

    #[test]
    fn instantiates_polymorphic_primitives_per_use() {
        assert_eq!(
            infer_one("((lambda () (reverse '(1)) (reverse '(a))))"),
            "(listof symbol?)"
        );
        assert_eq!(
            infer_one("((lambda () (vector 1) (vector #\\a)))"),
            "(vectorof char?)"
        );
    }

    #[test]
    fn generalizes_top_level_inferred_schemes() {
        assert_eq!(
            infer_all("(define id (lambda (x) x)) (id 1) (id \"x\")"),
            vec![
                "(-> x x)".to_string(),
                "number?".to_string(),
                "string?".to_string(),
            ]
        );
    }

    #[test]
    fn infers_equality_predicates() {
        assert_eq!(infer_one("(equal? '(1) '(1))"), "boolean?");
    }

    #[test]
    fn infers_indexed_list_primitives() {
        assert_eq!(infer_one("(list)"), "null?");
        assert_eq!(infer_one("(list 1 \"x\")"), "(listof (U number? string?))");
        assert_eq!(infer_one("(car (list 1 \"x\"))"), "(U number? string?)");
        assert_eq!(infer_one("(length '(a b c))"), "number?");
        assert_eq!(infer_one("(append)"), "null?");
        assert_eq!(infer_one("(append '(a) '(b c))"), "(listof symbol?)");
        assert_eq!(
            infer_one("(append '(1) '(\"x\"))"),
            "(listof (U number? string?))"
        );
        assert_eq!(infer_one("(append '(a) 'b)"), "any?");
        assert_eq!(infer_one("(cadr '(a b c))"), "symbol?");
        assert_eq!(infer_one("(caddr '(a b c))"), "symbol?");
        assert_eq!(infer_one("(list-ref '(a b c) 1)"), "symbol?");
        assert_eq!(infer_one("(list-tail '(a b c) 1)"), "(listof symbol?)");
        assert_eq!(
            infer_one("(lambda (xs) (string-length (list-ref xs 0)))"),
            "(-> (listof string?) number?)"
        );
    }

    #[test]
    fn infers_membership_primitives() {
        assert_eq!(
            infer_one("(member 'b '(a b c))"),
            "(U boolean? (listof symbol?))"
        );
        assert_eq!(
            infer_one("(assoc 'b '((a 1) (b 2)))"),
            "(U boolean? (listof (U number? symbol?)))"
        );
        assert_eq!(
            infer_one("(lambda (xs) (member 'b xs))"),
            "(-> (listof t0) (U boolean? (listof t0)))"
        );
    }

    #[test]
    fn infers_higher_order_iteration_element_flow() {
        assert_eq!(infer_one("(map + '(1 2) '(3 4))"), "(listof number?)");
        assert_eq!(
            infer_one("(map string-length '(\"a\" \"bb\"))"),
            "(listof number?)"
        );
        assert_eq!(
            infer_one("(map list '(1 2) '(\"a\" \"b\"))"),
            "(listof (listof (U number? string?)))"
        );
        assert_eq!(
            infer_one("(map vector '(1 2) '(\"a\" \"b\"))"),
            "(listof (vectorof (U number? string?)))"
        );
        assert_eq!(
            infer_one("(lambda (xs) (map string-length xs))"),
            "(-> (listof string?) (listof number?))"
        );
        assert_eq!(
            infer_one("(lambda (xs) (for-each string-length xs))"),
            "(-> (listof string?) unknown?)"
        );
        assert_eq!(infer_one("(for-each + '(1 2) '(3 4))"), "unknown?");
    }

    #[test]
    fn infers_conversion_primitives() {
        assert_eq!(infer_one("(symbol->string 'hello)"), "string?");
        assert_eq!(infer_one("(string->number \"1\")"), "(U boolean? number?)");
        assert_eq!(
            infer_one("(string->number \"10\" 16)"),
            "(U boolean? number?)"
        );
    }

    #[test]
    fn infers_output_primitives() {
        assert_eq!(infer_one("(current-input-port)"), "input-port?");
        assert_eq!(infer_one("(input-port? (current-input-port))"), "boolean?");
        assert_eq!(infer_one("(read)"), "any?");
        assert_eq!(infer_one("(read-char)"), "(U char? eof-object?)");
        assert_eq!(infer_one("(current-output-port)"), "output-port?");
        assert_eq!(
            infer_one("(output-port? (current-output-port))"),
            "boolean?"
        );
        assert_eq!(infer_one("(open-output-file \"x\")"), "output-port?");
        assert_eq!(infer_one("(call-with-input-file \"x\" read)"), "any?");
        assert_eq!(
            infer_one("(call-with-output-file \"x\" (lambda (p) (write \"x\" p)))"),
            "any?"
        );
        assert_eq!(
            infer_one("(lambda (f) (call-with-input-file \"x\" f))"),
            "(-> (-> input-port? any?) any?)"
        );
        assert_eq!(infer_one("(with-input-from-file \"x\" read)"), "any?");
        assert_eq!(
            infer_one("(with-output-to-file \"x\" (lambda () (write \"x\")))"),
            "any?"
        );
        assert_eq!(
            infer_one("(lambda (thunk) (with-output-to-file \"x\" thunk))"),
            "(-> (-> any?) any?)"
        );
        assert_eq!(infer_one("(load \"x\")"), "unknown?");
        assert_eq!(
            infer_one("(eval '(+ 1 2) (scheme-report-environment 5))"),
            "any?"
        );
        assert_eq!(infer_one("(interaction-environment)"), "any?");
        assert_eq!(
            infer_one("(dynamic-wind (lambda () 1) (lambda () 2) (lambda () 3))"),
            "any?"
        );
        assert_eq!(
            infer_one("(lambda (before thunk after) (dynamic-wind before thunk after))"),
            "(-> (-> any?) (-> any?) (-> any?) any?)"
        );
        assert_eq!(infer_one("(call/cc (lambda (k) 1))"), "number?");
        assert_eq!(infer_one("(call/cc (lambda (k) (k 5)))"), "number?");
        assert_eq!(
            infer_one("(call-with-current-continuation (lambda (k) (if #t (k 5) \"x\")))"),
            "(U number? string?)"
        );
        assert_eq!(
            infer_one("(lambda (f) (call/cc f))"),
            "(-> (-> (-> any? any?) any?) any?)"
        );
        assert_eq!(infer_one("(write \"x\")"), "unknown?");
    }

    #[test]
    fn infers_string_primitives() {
        assert_eq!(infer_one("(string #\\a #\\b)"), "string?");
        assert_eq!(infer_one("(string-ref \"abc\" 1)"), "char?");
        assert_eq!(infer_one("(string->list \"ab\")"), "(listof char?)");
        assert_eq!(infer_one("(list->string '(#\\a #\\b))"), "string?");
        assert_eq!(infer_one("(list->string '())"), "string?");
        assert_eq!(infer_one("(string-set! \"ab\" 0 #\\z)"), "unknown?");
    }

    #[test]
    fn infers_numeric_predicates_and_integer_utilities() {
        assert_eq!(infer_one("(zero? 0)"), "boolean?");
        assert_eq!(infer_one("(quotient 5 2)"), "number?");
        assert_eq!(infer_one("(floor 3/2)"), "number?");
        assert_eq!(infer_one("(exact->inexact 1/2)"), "number?");
        assert_eq!(infer_one("(make-rectangular 1 2)"), "number?");
        assert_eq!(infer_one("(make-polar 2 0)"), "number?");
        assert_eq!(infer_one("(real-part 1+2i)"), "number?");
        assert_eq!(infer_one("(magnitude 3+4i)"), "number?");
        assert_eq!(infer_one("(exp 0)"), "number?");
        assert_eq!(infer_one("(atan 1 0)"), "number?");
        assert_eq!(infer_one("(rationalize 1.3 0.1)"), "number?");
        assert_eq!(infer_one("(sqrt 4)"), "number?");
        assert_eq!(infer_one("(expt 2 3)"), "number?");
        assert_eq!(
            infer_one("(lambda (x) (if (integer? x) (+ x 1) 0))"),
            "(-> number? number?)"
        );
    }
}
