use std::collections::{BTreeMap, BTreeSet, HashMap};

use num::ToPrimitive;
use thiserror::Error;

use crate::stdlib::primitive;
use crate::surface::{Expr, Program, TopLevel, classify_expr};
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

    #[error("type constraint conflict: expected {expected}, got {actual}")]
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
    primitive: bool,
}

impl TypeEnv {
    pub fn new() -> Self {
        let mut env = Self::default();
        for primitive in crate::stdlib::r5rs_primitives() {
            env.define_primitive(primitive.name, primitive.signature);
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

    fn define_primitive(&mut self, name: impl Into<String>, ty: Type) {
        self.bindings
            .insert(name.into(), TypeBinding::primitive(ty));
    }

    fn binding(&self, name: &str) -> Option<&TypeBinding> {
        self.bindings.get(name)
    }

    fn is_primitive(&self, name: &str) -> bool {
        self.binding(name).is_some_and(|binding| binding.primitive)
    }
}

impl TypeBinding {
    fn monotype(ty: Type) -> Self {
        Self {
            ty,
            scheme: false,
            primitive: false,
        }
    }

    fn scheme(ty: Type) -> Self {
        Self {
            ty,
            scheme: true,
            primitive: false,
        }
    }

    fn primitive(ty: Type) -> Self {
        Self {
            ty,
            scheme: true,
            primitive: true,
        }
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

#[derive(Debug, Clone, PartialEq, Eq)]
enum ApplyFinalList {
    Empty,
    Rest(Type),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ConstructorKind {
    List,
    Vector,
}

struct SingleValueConditionApplication<'a> {
    param: &'a Spanned<String>,
    consequent: &'a Spanned<Expr>,
    alternate: Option<&'a Spanned<Expr>>,
    condition: &'a Spanned<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum PrimitiveApplication {
    Apply,
    Values,
    CallWithValues,
    CallCc,
    CallWithInputFile,
    CallWithOutputFile,
    WithInputFromFile,
    WithOutputToFile,
    DynamicWind,
    Map,
    ForEach,
    List,
    MakeVector,
    Vector,
    VectorToList,
    ListToVector,
    VectorRef,
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
    fn classify(expr: &Expr, env: &TypeEnv) -> Option<Self> {
        let Expr::Variable(name) = expr else {
            return None;
        };

        if !env.is_primitive(name) {
            return None;
        }

        match name.as_str() {
            "apply" => Some(Self::Apply),
            "values" => Some(Self::Values),
            "call-with-values" => Some(Self::CallWithValues),
            "call/cc" | "call-with-current-continuation" => Some(Self::CallCc),
            "call-with-input-file" => Some(Self::CallWithInputFile),
            "call-with-output-file" => Some(Self::CallWithOutputFile),
            "with-input-from-file" => Some(Self::WithInputFromFile),
            "with-output-to-file" => Some(Self::WithOutputToFile),
            "dynamic-wind" => Some(Self::DynamicWind),
            "map" => Some(Self::Map),
            "for-each" => Some(Self::ForEach),
            "list" => Some(Self::List),
            "make-vector" => Some(Self::MakeVector),
            "vector" => Some(Self::Vector),
            "vector->list" => Some(Self::VectorToList),
            "list->vector" => Some(Self::ListToVector),
            "vector-ref" => Some(Self::VectorRef),
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

fn constructor_kind(expr: &Spanned<Expr>, env: &TypeEnv) -> Option<ConstructorKind> {
    match primitive_operator_name(expr, env)? {
        "list" => Some(ConstructorKind::List),
        "vector" => Some(ConstructorKind::Vector),
        _ => None,
    }
}

fn primitive_operator_name<'a>(expr: &'a Spanned<Expr>, env: &TypeEnv) -> Option<&'a str> {
    let Expr::Variable(name) = &expr.node else {
        return None;
    };

    env.is_primitive(name).then_some(name.as_str())
}

fn single_value_condition_application<'a>(
    params: &'a [Spanned<String>],
    rest: Option<&Spanned<String>>,
    body: &'a [Spanned<Expr>],
    operands: &'a [Spanned<Expr>],
) -> Option<SingleValueConditionApplication<'a>> {
    let ([param], None, [body], [condition]) = (params, rest, body, operands) else {
        return None;
    };
    let Expr::If {
        condition: branch_condition,
        consequent,
        alternate,
    } = &body.node
    else {
        return None;
    };
    if variable_name(branch_condition).is_some_and(|name| name == &param.node) {
        Some(SingleValueConditionApplication {
            param,
            consequent,
            alternate: alternate.as_deref(),
            condition,
        })
    } else {
        None
    }
}

fn primitive_unary_operand<'a>(
    expr: &'a Spanned<Expr>,
    name: &str,
    env: &TypeEnv,
) -> Option<&'a Spanned<Expr>> {
    let Expr::Apply { operator, operands } = &expr.node else {
        return None;
    };

    if primitive_operator_name(operator, env) != Some(name) {
        return None;
    }

    let [operand] = operands.as_slice() else {
        return None;
    };
    Some(operand)
}

fn truthy_condition_value_type(
    condition: &Spanned<Expr>,
    condition_ty: Type,
    env: &TypeEnv,
) -> Type {
    if false_or_success_condition(condition, env) {
        false_or_success_type(condition_ty)
    } else {
        condition_ty
    }
}

fn false_or_success_condition(condition: &Spanned<Expr>, env: &TypeEnv) -> bool {
    let Expr::Apply { operator, .. } = &condition.node else {
        return false;
    };
    matches!(
        primitive_operator_name(operator, env),
        Some("memq" | "memv" | "member" | "assq" | "assv" | "assoc")
    )
}

fn false_or_success_type(ty: Type) -> Type {
    match ty {
        Type::Boolean => Type::Never,
        Type::Union(types) => Type::union(
            types
                .into_iter()
                .filter(|ty| ty != &Type::Boolean)
                .collect::<Vec<_>>(),
        ),
        ty => ty,
    }
}

fn equality_refinement(
    operator: &Spanned<Expr>,
    operands: &[Spanned<Expr>],
    env: &TypeEnv,
) -> Option<(String, Type)> {
    if !matches!(
        primitive_operator_name(operator, env),
        Some("eq?" | "eqv?" | "equal?")
    ) {
        return None;
    }
    let [left, right] = operands else {
        return None;
    };

    variable_name(left)
        .and_then(|name| static_expr_type(right).map(|ty| (name.clone(), ty)))
        .or_else(|| {
            variable_name(right)
                .and_then(|name| static_expr_type(left).map(|ty| (name.clone(), ty)))
        })
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
                let mut local = env.clone();
                let recursive_seed = matches!(value.node, Expr::Lambda { .. }).then(|| {
                    let seed = self.fresh_type_var();
                    local.define(name.node.clone(), seed.clone());
                    seed
                });
                let ty = self.infer_expr(value, &local)?;
                let ty = match recursive_seed {
                    Some(seed) => self.unify(ty, seed, value.span.clone())?,
                    None => ty,
                };
                let ty = self.resolve(ty);
                env.define_inferred(name.node.clone(), ty.clone());
                Ok(ty)
            }
            TopLevel::Expr(expr) => self.infer_expr(&form.with_node(expr.clone()), env),
        }
    }

    pub fn infer_expr(&mut self, expr: &Spanned<Expr>, env: &TypeEnv) -> Result<Type, TypeError> {
        match &expr.node {
            Expr::Literal(atom) => Ok(type_of_atom(atom)),
            Expr::Variable(name) => self.infer_variable(name, expr.span.clone(), env),
            Expr::Quote(datum) => Ok(type_of_datum(datum)),
            Expr::Quasiquote(datum) => self.infer_quasiquote_datum(datum, env, 0),
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
                Ok(Type::Unspecified)
            }
            Expr::Delay(expr) => Ok(Type::PromiseOf(Box::new(self.infer_expr(expr, env)?))),
            Expr::LetRec { bindings, body } => self.infer_letrec(bindings, body, env),
            Expr::Apply { operator, operands } => {
                if let Some((condition, alternate)) =
                    self.desugared_or_operands(operator, operands, env)
                {
                    return self.infer_desugared_or(condition, alternate, env);
                }

                if PrimitiveApplication::classify(&operator.node, env)
                    == Some(PrimitiveApplication::CallCc)
                {
                    return self.infer_call_cc_application(operands, expr.span.clone(), env);
                }
                if let Expr::Lambda { params, rest, body } = &operator.node
                    && let Some(application) =
                        single_value_condition_application(params, rest.as_ref(), body, operands)
                {
                    return self.infer_single_value_condition_application(
                        application.param,
                        application.consequent,
                        application.alternate,
                        application.condition,
                        env,
                    );
                }

                let operand_tys = operands
                    .iter()
                    .map(|operand| self.infer_expr(operand, env))
                    .collect::<Result<Vec<_>, _>>()?;

                if self.any_never(&operand_tys) {
                    return Ok(Type::Never);
                }

                if let Expr::Lambda { params, rest, body } = &operator.node {
                    return self.infer_lambda_with_argument_types(
                        params,
                        rest.as_ref(),
                        body,
                        operand_tys,
                        expr.span.clone(),
                        env,
                    );
                }

                if let Some(application) = PrimitiveApplication::classify(&operator.node, env) {
                    return self.infer_primitive_application(
                        application,
                        operands,
                        operand_tys,
                        expr.span.clone(),
                        env,
                    );
                }

                let operator_ty = self.infer_expr(operator, env)?;
                if self.is_never(&operator_ty) {
                    return Ok(Type::Never);
                }
                self.infer_application(operator_ty, operands, operand_tys, expr.span.clone())
            }
        }
    }

    fn any_never(&self, types: &[Type]) -> bool {
        types.iter().any(|ty| self.is_never(ty))
    }

    fn is_never(&self, ty: &Type) -> bool {
        matches!(self.resolve(ty.clone()), Type::Never)
    }

    fn infer_desugared_or(
        &mut self,
        condition: &Spanned<Expr>,
        alternate: &Spanned<Expr>,
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        let refinements = self.predicate_refinements(condition, env);
        let condition_ty = self.infer_expr(condition, env)?;
        let base = self.clone();

        let (_, then_dead) = refined_branch_env(env, &refinements, RefinedBranch::Then);
        let then_inferencer = base.clone();
        let consequent_ty = if then_dead { Type::Never } else { condition_ty };

        let (else_env, else_dead) = refined_branch_env(env, &refinements, RefinedBranch::Else);
        let mut else_inferencer = base;
        let alternate_ty = if else_dead {
            Type::Never
        } else {
            else_inferencer.infer_expr(alternate, &else_env)?
        };

        self.merge_branch_substitutions(&refinements, &then_inferencer, &else_inferencer);
        self.next_var = self
            .next_var
            .max(then_inferencer.next_var)
            .max(else_inferencer.next_var);

        Ok(Type::union(vec![
            then_inferencer.resolve(consequent_ty),
            else_inferencer.resolve(alternate_ty),
        ]))
    }

    fn desugared_or_operands<'a>(
        &self,
        operator: &'a Spanned<Expr>,
        operands: &'a [Spanned<Expr>],
        env: &TypeEnv,
    ) -> Option<(&'a Spanned<Expr>, &'a Spanned<Expr>)> {
        let [condition] = operands else {
            return None;
        };
        if !self.has_predicate_refinements(condition, env) {
            return None;
        }

        let Expr::Lambda { params, rest, body } = &operator.node else {
            return None;
        };
        let [param] = params.as_slice() else {
            return None;
        };
        if rest.is_some() || body.len() != 1 || !param.node.starts_with("#%lavu_or_value_") {
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

    fn has_predicate_refinements(&self, condition: &Spanned<Expr>, env: &TypeEnv) -> bool {
        let mut probe = self.clone();
        !probe.predicate_refinements(condition, env).is_empty()
    }

    fn infer_primitive_application(
        &mut self,
        application: PrimitiveApplication,
        operands: &[Spanned<Expr>],
        operand_tys: Vec<Type>,
        span: SourceSpan,
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        match application {
            PrimitiveApplication::Apply => {
                self.infer_apply_primitive(operands, operand_tys, span, env)
            }
            PrimitiveApplication::Values => Ok(Type::Values(
                operand_tys
                    .into_iter()
                    .map(|ty| self.resolve(ty))
                    .collect::<Vec<_>>(),
            )),
            PrimitiveApplication::CallWithValues => {
                self.infer_call_with_values(operands, operand_tys, span, env)
            }
            PrimitiveApplication::CallCc => self.infer_call_cc(operands, operand_tys, span),
            PrimitiveApplication::CallWithInputFile => {
                self.infer_file_callback(operands, operand_tys, span, Type::InputPort)
            }
            PrimitiveApplication::CallWithOutputFile => {
                self.infer_file_callback(operands, operand_tys, span, Type::OutputPort)
            }
            PrimitiveApplication::WithInputFromFile | PrimitiveApplication::WithOutputToFile => {
                self.infer_file_thunk(operands, operand_tys, span)
            }
            PrimitiveApplication::DynamicWind => {
                self.infer_dynamic_wind(operands, operand_tys, span)
            }
            PrimitiveApplication::Map => self.infer_higher_order_list(
                operands,
                operand_tys,
                span,
                HigherOrderListResult::Mapped,
                env,
            ),
            PrimitiveApplication::ForEach => self.infer_higher_order_list(
                operands,
                operand_tys,
                span,
                HigherOrderListResult::Unspecified,
                env,
            ),
            PrimitiveApplication::List => Ok(self.infer_list_constructor(operand_tys)),
            PrimitiveApplication::MakeVector => self.infer_make_vector(operands, operand_tys, span),
            PrimitiveApplication::Vector => Ok(self.infer_vector_constructor(operand_tys)),
            PrimitiveApplication::VectorToList => {
                self.infer_vector_to_list(operands, operand_tys, span, env)
            }
            PrimitiveApplication::ListToVector => {
                self.infer_list_to_vector(operands, operand_tys, span, env)
            }
            PrimitiveApplication::VectorRef => {
                self.infer_vector_ref(operands, operand_tys, span, env)
            }
            PrimitiveApplication::ListRef => {
                self.infer_indexed_list(operands, operand_tys, span, ListAccessResult::Element, env)
            }
            PrimitiveApplication::ListTail => {
                self.infer_indexed_list(operands, operand_tys, span, ListAccessResult::Tail, env)
            }
            PrimitiveApplication::Car => self.infer_pair_accessor(
                operands,
                operand_tys,
                span,
                ListAccessResult::Element,
                env,
            ),
            PrimitiveApplication::Cdr => {
                self.infer_pair_accessor(operands, operand_tys, span, ListAccessResult::Tail, env)
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

        let predicate_positive = self.lambda_predicate_positive(params, rest, body, &local);
        let result = self.infer_sequence(body, &local)?;
        let param_types = params
            .iter()
            .map(|param| self.resolve(Type::Var(param.node.clone())))
            .collect::<Vec<_>>();

        let result = self.resolve(result);
        if rest.is_none()
            && params.len() == 1
            && result == Type::Boolean
            && let Some(positive) = predicate_positive
        {
            let param = predicate_lambda_param_type(&params[0].node, param_types[0].clone());
            return Ok(Type::predicate_procedure(param, self.resolve(positive)));
        }

        Ok(match rest {
            Some(rest) => Type::rest_procedure(
                param_types,
                self.resolve(Type::Var(rest.node.clone())),
                result,
            ),
            None => Type::procedure(param_types, result),
        })
    }

    fn lambda_predicate_positive(
        &mut self,
        params: &[Spanned<String>],
        rest: Option<&Spanned<String>>,
        body: &[Spanned<Expr>],
        env: &TypeEnv,
    ) -> Option<Type> {
        let ([param], None, [expr]) = (params, rest, body) else {
            return None;
        };
        self.truthy_predicate_refinements(expr, env)
            .into_iter()
            .filter_map(|(name, positive)| (name == param.node).then_some(positive))
            .reduce(intersect_types)
    }

    fn infer_lambda_with_argument_types(
        &mut self,
        params: &[Spanned<String>],
        rest: Option<&Spanned<String>>,
        body: &[Spanned<Expr>],
        argument_tys: Vec<Type>,
        span: SourceSpan,
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        if argument_tys.len() < params.len()
            || (rest.is_none() && argument_tys.len() != params.len())
        {
            let expected = match rest {
                Some(_) => format!("at least {}", params.len()),
                None => params.len().to_string(),
            };
            return Err(TypeError::ArityMismatch {
                expected,
                actual: argument_tys.len(),
                span,
            });
        }

        let mut local = env.clone();
        for (param, ty) in params.iter().zip(argument_tys.iter()) {
            if generalize_direct_argument(ty) {
                local.define_inferred(param.node.clone(), ty.clone());
            } else {
                local.define(param.node.clone(), ty.clone());
            }
        }
        if let Some(rest) = rest {
            let rest_tys = argument_tys[params.len()..].to_vec();
            let rest_ty = match rest_tys.as_slice() {
                [] => Type::Null,
                [single] => Type::ListOf(Box::new(single.clone())),
                _ => Type::ListOf(Box::new(Type::union(rest_tys))),
            };
            local.define(rest.node.clone(), rest_ty);
        }

        let result = self.infer_sequence(body, &local)?;
        Ok(self.resolve(result))
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
        let refinements = self.predicate_refinements(condition, env);
        self.infer_expr(condition, env)?;

        let base = self.clone();

        let (then_env, then_dead) = refined_branch_env(env, &refinements, RefinedBranch::Then);
        let mut then_inferencer = base.clone();
        let consequent_ty = if then_dead {
            Type::Never
        } else {
            then_inferencer.infer_expr(consequent, &then_env)?
        };

        let (else_env, else_dead) = refined_branch_env(env, &refinements, RefinedBranch::Else);
        let mut else_inferencer = base;
        let alternate_ty = match alternate {
            Some(_) if else_dead => Type::Never,
            Some(expr) => else_inferencer.infer_expr(expr, &else_env)?,
            None => Type::Unspecified,
        };

        self.merge_branch_substitutions(&refinements, &then_inferencer, &else_inferencer);
        self.next_var = self
            .next_var
            .max(then_inferencer.next_var)
            .max(else_inferencer.next_var);

        Ok(Type::union(vec![
            then_inferencer.resolve(consequent_ty),
            else_inferencer.resolve(alternate_ty),
        ]))
    }

    fn infer_single_value_condition_application(
        &mut self,
        param: &Spanned<String>,
        consequent: &Spanned<Expr>,
        alternate: Option<&Spanned<Expr>>,
        condition: &Spanned<Expr>,
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        let refinements = self.predicate_refinements(condition, env);
        let condition_ty = self.infer_expr(condition, env)?;
        let truthy_condition_ty =
            truthy_condition_value_type(condition, self.resolve(condition_ty.clone()), env);
        let base = self.clone();

        let (mut then_env, then_dead) = refined_branch_env(env, &refinements, RefinedBranch::Then);
        then_env.define(param.node.clone(), truthy_condition_ty);
        let mut then_inferencer = base.clone();
        let consequent_ty = if then_dead {
            Type::Never
        } else {
            then_inferencer.infer_expr(consequent, &then_env)?
        };

        let (mut else_env, else_dead) = refined_branch_env(env, &refinements, RefinedBranch::Else);
        else_env.define(param.node.clone(), condition_ty);
        let mut else_inferencer = base;
        let alternate_ty = match alternate {
            Some(_) if else_dead => Type::Never,
            Some(expr) => else_inferencer.infer_expr(expr, &else_env)?,
            None => Type::Unspecified,
        };

        self.merge_branch_substitutions(&refinements, &then_inferencer, &else_inferencer);
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
        refinements: &[BranchRefinement],
        then_inferencer: &Inferencer,
        else_inferencer: &Inferencer,
    ) {
        let mut names = BTreeSet::new();
        for refinement in refinements {
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

            if then_ty.is_none()
                && else_ty.is_some()
                && let Some(positive) =
                    branch_refinement_type(refinements, &name, RefinedBranch::Then)
            {
                then_ty = Some(positive);
            }
            if else_ty.is_none()
                && then_ty.is_some()
                && let Some(positive) =
                    branch_refinement_type(refinements, &name, RefinedBranch::Else)
            {
                else_ty = Some(positive);
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
        let mut result = Type::Unspecified;
        for expr in exprs {
            result = self.infer_expr(expr, env)?;
        }
        Ok(self.resolve(result))
    }

    fn predicate_refinements(
        &mut self,
        condition: &Spanned<Expr>,
        env: &TypeEnv,
    ) -> Vec<BranchRefinement> {
        let truthy = self.truthy_predicate_refinements(condition, env);
        if !truthy.is_empty() {
            return truthy
                .into_iter()
                .map(|(name, positive)| BranchRefinement {
                    branch: RefinedBranch::Then,
                    name,
                    positive,
                })
                .collect();
        }

        let Expr::Apply { operator, operands } = &condition.node else {
            return Vec::new();
        };
        let Expr::Variable(operator_name) = &operator.node else {
            return Vec::new();
        };
        if operator_name != "not" || !env.is_primitive(operator_name) || operands.len() != 1 {
            return Vec::new();
        }

        self.truthy_predicate_refinements(&operands[0], env)
            .into_iter()
            .map(|(name, positive)| BranchRefinement {
                branch: RefinedBranch::Else,
                name,
                positive,
            })
            .collect()
    }

    fn truthy_predicate_refinements(
        &mut self,
        condition: &Spanned<Expr>,
        env: &TypeEnv,
    ) -> Vec<(String, Type)> {
        if let Some(refinement) = self.direct_predicate_refinement(condition, env) {
            return vec![refinement];
        }

        if let Expr::Apply { operator, operands } = &condition.node
            && let Some((condition, alternate)) =
                self.desugared_or_operands(operator, operands, env)
        {
            return self.truthy_or_predicate_refinements(condition, alternate, env);
        }

        let Expr::If {
            condition,
            consequent,
            alternate: Some(alternate),
        } = &condition.node
        else {
            return Vec::new();
        };
        if !is_false_literal(alternate) {
            if is_true_literal(consequent) {
                return disjoin_refinements(
                    self.truthy_predicate_refinements(condition, env),
                    self.truthy_predicate_refinements(alternate, env),
                );
            }
            return Vec::new();
        }

        let mut refinements = self.truthy_predicate_refinements(condition, env);
        refinements.extend(self.truthy_predicate_refinements(consequent, env));
        refinements
    }

    fn truthy_or_predicate_refinements(
        &mut self,
        condition: &Spanned<Expr>,
        alternate: &Spanned<Expr>,
        env: &TypeEnv,
    ) -> Vec<(String, Type)> {
        let condition_refinements = self.truthy_predicate_refinements(condition, env);
        let alternate_refinements = self.truthy_predicate_refinements(alternate, env);

        if alternate_refinements.is_empty() {
            return if is_false_literal(alternate) {
                condition_refinements
            } else {
                Vec::new()
            };
        }

        disjoin_refinements(condition_refinements, alternate_refinements)
    }

    fn direct_predicate_refinement(
        &mut self,
        condition: &Spanned<Expr>,
        env: &TypeEnv,
    ) -> Option<(String, Type)> {
        let Expr::Apply { operator, operands } = &condition.node else {
            return None;
        };
        if primitive_operator_name(operator, env) == Some("apply") {
            return self.apply_predicate_refinement(operands, env);
        }
        if let Some(refinement) = equality_refinement(operator, operands, env) {
            return Some(refinement);
        }
        if operands.len() != 1 {
            return None;
        }

        let Expr::Variable(predicate_name) = &operator.node else {
            return None;
        };

        self.predicate_call_refinement(predicate_name, &operands[0], env)
    }

    fn apply_predicate_refinement(
        &mut self,
        operands: &[Spanned<Expr>],
        env: &TypeEnv,
    ) -> Option<(String, Type)> {
        let Expr::Variable(predicate_name) = &operands.first()?.node else {
            return None;
        };
        let argument = visible_apply_predicate_argument(operands, env)?;
        self.predicate_call_refinement(predicate_name, argument, env)
    }

    fn predicate_call_refinement(
        &mut self,
        predicate_name: &str,
        operand: &Spanned<Expr>,
        env: &TypeEnv,
    ) -> Option<(String, Type)> {
        if env.is_primitive(predicate_name) {
            let positive = primitive(predicate_name)?
                .predicate
                .filter(|predicate| predicate.argument == 0)?
                .positive;
            let positive = self.instantiate_scheme(&positive);

            return predicate_operand_refinement(operand, positive, env);
        }

        let positive = self.latent_predicate_positive(predicate_name, env)?;
        self.widen_latent_predicate_variable_operand(operand, env);
        predicate_operand_refinement(operand, positive, env)
    }

    fn latent_predicate_positive(&mut self, name: &str, env: &TypeEnv) -> Option<Type> {
        match self.resolve(env.get(name)?.clone()) {
            Type::Procedure(ProcedureType::Predicate { positive, .. }) => {
                Some(self.resolve(*positive))
            }
            Type::Var(var) => {
                let positive = self.fresh_type_var();
                self.bind_var(var, Type::predicate_procedure(Type::Any, positive.clone()))
                    .ok()?;
                Some(positive)
            }
            _ => None,
        }
    }

    fn widen_latent_predicate_variable_operand(&mut self, operand: &Spanned<Expr>, env: &TypeEnv) {
        let Expr::Variable(name) = &operand.node else {
            return;
        };
        if let Some(Type::Var(var)) = env.get(name).cloned().map(|ty| self.resolve(ty)) {
            self.substitutions.insert(var, Type::Any);
        }
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
            Type::Procedure(ProcedureType::Predicate { param, positive }) => {
                Type::predicate_procedure(
                    self.instantiate_scheme_type(param, vars),
                    self.instantiate_scheme_type(positive, vars),
                )
            }
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
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        let [producer_ty, consumer_ty]: [Type; 2] =
            operand_tys
                .try_into()
                .map_err(|operand_tys: Vec<Type>| TypeError::ArityMismatch {
                    expected: "2".to_string(),
                    actual: operand_tys.len(),
                    span: span.clone(),
                })?;

        let constructor = constructor_kind(&operands[1], env);
        let expected_produced = constructor
            .and_then(|constructor| self.expected_constructor_value(&consumer_ty, constructor))
            .or_else(|| self.expected_call_with_values_result(&consumer_ty));
        let produced = self.infer_call_with_values_producer(
            producer_ty,
            expected_produced,
            operands[0].span.clone(),
        )?;
        let value_tys = match self.resolve(produced) {
            Type::Values(values) => values,
            value => vec![value],
        };
        let value_operands = value_tys
            .iter()
            .map(|_| operands[0].clone())
            .collect::<Vec<_>>();

        if let Some(constructor) = constructor {
            return Ok(self.infer_constructor_result(constructor, value_tys));
        }

        self.infer_application(consumer_ty, &value_operands, value_tys, span)
    }

    fn expected_constructor_value(
        &self,
        consumer_ty: &Type,
        _constructor: ConstructorKind,
    ) -> Option<Type> {
        match self.resolve(consumer_ty.clone()) {
            Type::Procedure(ProcedureType::UniformVariadic { param, .. }) => Some(*param),
            _ => None,
        }
    }

    fn infer_constructor_result(
        &self,
        constructor: ConstructorKind,
        operand_tys: Vec<Type>,
    ) -> Type {
        match constructor {
            ConstructorKind::List => self.infer_list_constructor(operand_tys),
            ConstructorKind::Vector => self.infer_vector_constructor(operand_tys),
        }
    }

    fn infer_call_with_values_producer(
        &mut self,
        producer_ty: Type,
        expected: Option<Type>,
        span: SourceSpan,
    ) -> Result<Type, TypeError> {
        match self.resolve(producer_ty) {
            Type::Var(name) => {
                let result = expected.unwrap_or_else(|| self.fresh_type_var());
                self.bind_var(name, Type::procedure(vec![], result.clone()))?;
                Ok(result)
            }
            Type::Procedure(ProcedureType::Fixed { params, result }) if params.is_empty() => {
                Ok(*result)
            }
            Type::Procedure(ProcedureType::Optional {
                required, result, ..
            }) if required.is_empty() => Ok(*result),
            Type::Procedure(ProcedureType::UniformVariadic { result, .. }) => Ok(*result),
            Type::Procedure(ProcedureType::Rest {
                required, result, ..
            }) if required.is_empty() => Ok(*result),
            Type::Procedure(procedure @ ProcedureType::Predicate { .. }) => {
                self.infer_application(Type::Procedure(procedure), &[], Vec::new(), span)
            }
            Type::Procedure(procedure) => {
                self.infer_application(Type::Procedure(procedure), &[], Vec::new(), span)
            }
            Type::Any | Type::Unknown => Ok(Type::Any),
            actual => Err(TypeError::ExpectedProcedure {
                actual: Box::new(actual),
                span,
            }),
        }
    }

    fn expected_call_with_values_result(&self, consumer_ty: &Type) -> Option<Type> {
        match self.resolve(consumer_ty.clone()) {
            Type::Procedure(ProcedureType::Fixed { params, .. }) => {
                Some(call_with_values_result_for_params(params))
            }
            _ => None,
        }
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
            Type::Var(name) => {
                let escape = self.fresh_type_var();
                let direct = self.fresh_type_var();
                self.substitutions.insert(
                    name,
                    Type::procedure(
                        vec![Type::procedure(vec![escape.clone()], Type::Never)],
                        direct.clone(),
                    ),
                );
                Ok(Type::union(vec![escape, direct]))
            }
            actual => {
                self.unify(actual, call_cc_receiver_type(), operands[0].span.clone())?;
                Ok(Type::Any)
            }
        }
    }

    fn infer_call_cc_application(
        &mut self,
        operands: &[Spanned<Expr>],
        span: SourceSpan,
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        let [receiver] = operands else {
            return Err(TypeError::ArityMismatch {
                expected: "1".to_string(),
                actual: operands.len(),
                span,
            });
        };

        let Expr::Lambda { params, rest, body } = &receiver.node else {
            let receiver_ty = self.infer_expr(receiver, env)?;
            return self.infer_call_cc(operands, vec![receiver_ty], span);
        };

        let escape_seed = self.fresh_type_var();
        let continuation = Type::procedure(vec![escape_seed.clone()], Type::Never);
        let direct = self.infer_lambda_with_argument_types(
            params,
            rest.as_ref(),
            body,
            vec![continuation],
            receiver.span.clone(),
            env,
        )?;

        Ok(self.call_cc_result(direct, escape_seed))
    }

    fn infer_call_cc_receiver(
        &mut self,
        receiver: ProcedureType,
        operands: &[Spanned<Expr>],
    ) -> Result<Type, TypeError> {
        let escape_seed = self.fresh_type_var();
        let continuation = Type::procedure(vec![escape_seed.clone()], Type::Never);
        let direct = self.apply_procedure(receiver, operands, vec![continuation])?;
        Ok(self.call_cc_result(direct, escape_seed))
    }

    fn call_cc_result(&self, direct: Type, escape_seed: Type) -> Type {
        let direct = self.resolve(direct);
        let escape = self.resolve(escape_seed.clone());

        if same_type_var(&escape, &escape_seed) {
            return direct;
        }

        if direct == Type::Never {
            escape
        } else {
            Type::union(vec![direct, escape])
        }
    }

    fn infer_file_callback(
        &mut self,
        operands: &[Spanned<Expr>],
        operand_tys: Vec<Type>,
        span: SourceSpan,
        port_ty: Type,
    ) -> Result<Type, TypeError> {
        let [path_ty, callback_ty]: [Type; 2] =
            operand_tys
                .try_into()
                .map_err(|operand_tys: Vec<Type>| TypeError::ArityMismatch {
                    expected: "2".to_string(),
                    actual: operand_tys.len(),
                    span,
                })?;

        self.unify(path_ty, Type::String, operands[0].span.clone())?;
        let callback_span = operands[1].span.clone();
        let port_operand = synthetic_operand(callback_span.clone());
        self.infer_application(callback_ty, &[port_operand], vec![port_ty], callback_span)
    }

    fn infer_file_thunk(
        &mut self,
        operands: &[Spanned<Expr>],
        operand_tys: Vec<Type>,
        span: SourceSpan,
    ) -> Result<Type, TypeError> {
        let [path_ty, thunk_ty]: [Type; 2] =
            operand_tys
                .try_into()
                .map_err(|operand_tys: Vec<Type>| TypeError::ArityMismatch {
                    expected: "2".to_string(),
                    actual: operand_tys.len(),
                    span,
                })?;

        self.unify(path_ty, Type::String, operands[0].span.clone())?;
        self.infer_nullary_application(thunk_ty, operands[1].span.clone())
    }

    fn infer_dynamic_wind(
        &mut self,
        operands: &[Spanned<Expr>],
        operand_tys: Vec<Type>,
        span: SourceSpan,
    ) -> Result<Type, TypeError> {
        let [before_ty, thunk_ty, after_ty]: [Type; 3] =
            operand_tys
                .try_into()
                .map_err(|operand_tys: Vec<Type>| TypeError::ArityMismatch {
                    expected: "3".to_string(),
                    actual: operand_tys.len(),
                    span,
                })?;

        self.infer_ignored_thunk(before_ty, operands[0].span.clone())?;
        let result = self.infer_nullary_application(thunk_ty, operands[1].span.clone())?;
        self.infer_ignored_thunk(after_ty, operands[2].span.clone())?;
        Ok(result)
    }

    fn infer_ignored_thunk(&mut self, ty: Type, span: SourceSpan) -> Result<(), TypeError> {
        self.unify(ty, Type::procedure(vec![], Type::Any), span)?;
        Ok(())
    }

    fn infer_nullary_application(
        &mut self,
        procedure_ty: Type,
        span: SourceSpan,
    ) -> Result<Type, TypeError> {
        let operand_span = synthetic_operand(span.clone());
        self.infer_application(procedure_ty, &[operand_span], Vec::new(), span)
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

    fn infer_vector_to_list(
        &mut self,
        operands: &[Spanned<Expr>],
        operand_tys: Vec<Type>,
        span: SourceSpan,
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        let [vector_ty]: [Type; 1] =
            operand_tys
                .try_into()
                .map_err(|operand_tys: Vec<Type>| TypeError::ArityMismatch {
                    expected: "1".to_string(),
                    actual: operand_tys.len(),
                    span,
                })?;

        if let Some(ty) = visible_vector_as_list_type(&operands[0], env) {
            return Ok(ty);
        }

        match self.resolve(vector_ty) {
            Type::VectorOf(element) => Ok(Type::ListOf(Box::new(self.resolve(*element)))),
            Type::Vector | Type::Any | Type::Unknown => Ok(Type::List),
            Type::Var(name) => {
                let element = self.fresh_type_var();
                self.substitutions
                    .insert(name, Type::VectorOf(Box::new(element.clone())));
                Ok(Type::ListOf(Box::new(element)))
            }
            actual => {
                self.unify(actual, Type::Vector, operands[0].span.clone())?;
                Ok(Type::List)
            }
        }
    }

    fn infer_list_to_vector(
        &mut self,
        operands: &[Spanned<Expr>],
        operand_tys: Vec<Type>,
        span: SourceSpan,
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        let [list_ty]: [Type; 1] =
            operand_tys
                .try_into()
                .map_err(|operand_tys: Vec<Type>| TypeError::ArityMismatch {
                    expected: "1".to_string(),
                    actual: operand_tys.len(),
                    span,
                })?;

        if let Some(ty) = visible_list_as_vector_type(&operands[0], env) {
            return Ok(ty);
        }

        match self.resolve(list_ty) {
            Type::Null | Type::List => Ok(Type::Vector),
            Type::ListOf(element) => Ok(Type::VectorOf(Box::new(self.resolve(*element)))),
            Type::Any | Type::Unknown => Ok(Type::Vector),
            Type::Var(name) => {
                let element = self.fresh_type_var();
                self.substitutions
                    .insert(name, Type::ListOf(Box::new(element.clone())));
                Ok(Type::VectorOf(Box::new(element)))
            }
            actual => {
                self.unify(actual, Type::List, operands[0].span.clone())?;
                Ok(Type::Vector)
            }
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

    fn infer_quasiquote_datum(
        &mut self,
        datum: &Spanned<Datum>,
        env: &TypeEnv,
        level: usize,
    ) -> Result<Type, TypeError> {
        match &datum.node {
            Datum::Unquote(inner) if level == 0 => self.infer_unquoted_datum(inner, env),
            Datum::Unquote(inner) => Ok(abbreviation_type(
                "unquote",
                self.infer_quasiquote_datum(inner, env, level - 1)?,
            )),
            Datum::UnquoteSplicing(_) if level == 0 => Ok(Type::Any),
            Datum::UnquoteSplicing(inner) => Ok(abbreviation_type(
                "unquote-splicing",
                self.infer_quasiquote_datum(inner, env, level - 1)?,
            )),
            Datum::Quasiquote(inner) => Ok(abbreviation_type(
                "quasiquote",
                self.infer_quasiquote_datum(inner, env, level + 1)?,
            )),
            Datum::List(items) => self.infer_quasiquote_list_type(items, env, level),
            Datum::DottedList(items, tail) => {
                let tail = self.infer_quasiquote_datum(tail, env, level)?;
                self.infer_quasiquote_item_types(items, env, level)
                    .map(|items| {
                        items
                            .into_iter()
                            .rev()
                            .fold(tail, |cdr, car| Type::Pair(Box::new(car), Box::new(cdr)))
                    })
            }
            Datum::Vector(items) => self.infer_quasiquote_vector_type(items, env, level),
            Datum::Quote(inner) => Ok(abbreviation_type("quote", type_of_datum(inner))),
            Datum::Atom(_) => Ok(type_of_datum(datum)),
        }
    }

    fn infer_quasiquote_list_type(
        &mut self,
        items: &[Spanned<Datum>],
        env: &TypeEnv,
        level: usize,
    ) -> Result<Type, TypeError> {
        let items = self.infer_quasiquote_item_types(items, env, level)?;

        Ok(match items.as_slice() {
            [] => Type::Null,
            _ => Type::ListOf(Box::new(Type::union(items))),
        })
    }

    fn infer_quasiquote_vector_type(
        &mut self,
        items: &[Spanned<Datum>],
        env: &TypeEnv,
        level: usize,
    ) -> Result<Type, TypeError> {
        let items = self.infer_quasiquote_item_types(items, env, level)?;

        Ok(match items.as_slice() {
            [] => Type::Vector,
            _ => Type::VectorOf(Box::new(Type::union(items))),
        })
    }

    fn infer_quasiquote_item_types(
        &mut self,
        items: &[Spanned<Datum>],
        env: &TypeEnv,
        level: usize,
    ) -> Result<Vec<Type>, TypeError> {
        let mut types = Vec::new();
        for item in items {
            match &item.node {
                Datum::UnquoteSplicing(inner) if level == 0 => {
                    match self.infer_unquote_splicing_element_type(inner, env)? {
                        Type::Never | Type::Null => {}
                        element => types.push(element),
                    }
                }
                _ => types.push(self.infer_quasiquote_datum(item, env, level)?),
            }
        }
        Ok(types)
    }

    fn infer_unquoted_datum(
        &mut self,
        datum: &Spanned<Datum>,
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        let Ok(expr) = classify_expr(datum) else {
            return Ok(Type::Any);
        };
        self.infer_expr(&expr, env)
    }

    fn infer_unquote_splicing_element_type(
        &mut self,
        datum: &Spanned<Datum>,
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        let ty = self.infer_unquoted_datum(datum, env)?;
        match self.resolve(ty) {
            Type::Null => Ok(Type::Null),
            Type::ListOf(element) => Ok(self.resolve(*element)),
            Type::List | Type::Any | Type::Unknown => Ok(Type::Any),
            Type::Var(name) => {
                let element = self.fresh_type_var();
                self.substitutions
                    .insert(name, Type::ListOf(Box::new(element.clone())));
                Ok(element)
            }
            actual => {
                self.unify(actual, Type::List, datum.span.clone())?;
                Ok(Type::Any)
            }
        }
    }

    fn infer_apply_constructor(
        &mut self,
        constructor: ConstructorKind,
        fixed_argument_tys: Vec<Type>,
        final_list_ty: Type,
        final_operand: &Spanned<Expr>,
        visible_final_arguments: Option<Vec<Type>>,
    ) -> Result<Type, TypeError> {
        let mut element_tys = fixed_argument_tys
            .into_iter()
            .map(|ty| self.resolve(ty))
            .collect::<Vec<_>>();
        element_tys.extend(self.infer_apply_constructor_final_elements(
            final_list_ty,
            final_operand,
            visible_final_arguments,
        )?);

        Ok(match constructor {
            ConstructorKind::List => self.infer_list_constructor(element_tys),
            ConstructorKind::Vector => self.infer_vector_constructor(element_tys),
        })
    }

    fn infer_apply_constructor_final_elements(
        &mut self,
        actual: Type,
        operand: &Spanned<Expr>,
        visible_arguments: Option<Vec<Type>>,
    ) -> Result<Vec<Type>, TypeError> {
        if let Some(types) = visible_arguments {
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

    fn infer_visible_proper_list_items(
        &mut self,
        expr: &Spanned<Expr>,
        env: &TypeEnv,
    ) -> Result<Option<Vec<Type>>, TypeError> {
        if let Some(types) = quoted_proper_list_types(expr) {
            return Ok(Some(types.into_iter().map(|ty| self.resolve(ty)).collect()));
        }
        if let Some(vector) = primitive_unary_operand(expr, "vector->list", env) {
            return self.infer_visible_vector_items(vector, env);
        }

        match &expr.node {
            Expr::Apply { operator, operands }
                if constructor_kind(operator, env) == Some(ConstructorKind::List) =>
            {
                self.infer_visible_expr_items(operands, env).map(Some)
            }
            _ => Ok(None),
        }
    }

    fn infer_visible_vector_items(
        &mut self,
        expr: &Spanned<Expr>,
        env: &TypeEnv,
    ) -> Result<Option<Vec<Type>>, TypeError> {
        if let Some(list) = primitive_unary_operand(expr, "list->vector", env) {
            return self.infer_visible_proper_list_items(list, env);
        }

        match &expr.node {
            Expr::Quote(datum) => match &datum.node {
                Datum::Vector(items) => Ok(Some(
                    items
                        .iter()
                        .map(type_of_datum)
                        .map(|ty| self.resolve(ty))
                        .collect(),
                )),
                _ => Ok(None),
            },
            Expr::Apply { operator, operands }
                if constructor_kind(operator, env) == Some(ConstructorKind::Vector) =>
            {
                self.infer_visible_expr_items(operands, env).map(Some)
            }
            _ => Ok(None),
        }
    }

    fn infer_visible_expr_items(
        &mut self,
        operands: &[Spanned<Expr>],
        env: &TypeEnv,
    ) -> Result<Vec<Type>, TypeError> {
        operands
            .iter()
            .map(|operand| {
                let ty = self.infer_expr(operand, env)?;
                Ok(self.resolve(ty))
            })
            .collect()
    }

    fn infer_higher_order_list(
        &mut self,
        operands: &[Spanned<Expr>],
        operand_tys: Vec<Type>,
        span: SourceSpan,
        result: HigherOrderListResult,
        env: &TypeEnv,
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
        if let Some(ty) = self.infer_constructor_mapper(&operands[0], &element_tys, result, env) {
            return Ok(ty);
        }
        let mapped_ty =
            self.infer_application(procedure_ty, &operands[1..], element_tys, span.clone())?;

        match result {
            HigherOrderListResult::Mapped => Ok(Type::ListOf(Box::new(self.resolve(mapped_ty)))),
            HigherOrderListResult::Unspecified => Ok(Type::Unspecified),
        }
    }

    fn infer_constructor_mapper(
        &self,
        procedure: &Spanned<Expr>,
        element_tys: &[Type],
        result: HigherOrderListResult,
        env: &TypeEnv,
    ) -> Option<Type> {
        let mapped = match constructor_kind(procedure, env)? {
            ConstructorKind::List => self.infer_list_constructor(element_tys.to_vec()),
            ConstructorKind::Vector => self.infer_vector_constructor(element_tys.to_vec()),
        };

        Some(match result {
            HigherOrderListResult::Mapped => Type::ListOf(Box::new(mapped)),
            HigherOrderListResult::Unspecified => Type::Unspecified,
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
        env: &TypeEnv,
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
        if let Some(index) = literal_index(&operands[1])
            && let Some(ty) = visible_indexed_list_type(&operands[0], index, result, env)
        {
            return Ok(ty);
        }
        let element = self.infer_list_element_type(list_ty, &operands[0])?;

        match result {
            ListAccessResult::Element => Ok(self.resolve(element)),
            ListAccessResult::Tail => Ok(Type::ListOf(Box::new(self.resolve(element)))),
        }
    }

    fn infer_vector_ref(
        &mut self,
        operands: &[Spanned<Expr>],
        operand_tys: Vec<Type>,
        span: SourceSpan,
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        let [vector_ty, index_ty]: [Type; 2] =
            operand_tys
                .try_into()
                .map_err(|operand_tys: Vec<Type>| TypeError::ArityMismatch {
                    expected: "2".to_string(),
                    actual: operand_tys.len(),
                    span,
                })?;

        self.unify(index_ty, Type::Number, operands[1].span.clone())?;
        if let Some(index) = literal_index(&operands[1])
            && let Some(ty) = visible_vector_item_type(&operands[0], index, env)
        {
            return Ok(ty);
        }

        match self.resolve(vector_ty) {
            Type::VectorOf(element) => Ok(self.resolve(*element)),
            Type::Vector | Type::Any | Type::Unknown => Ok(Type::Any),
            Type::Var(name) => {
                let element = self.fresh_type_var();
                self.substitutions
                    .insert(name, Type::VectorOf(Box::new(element.clone())));
                Ok(element)
            }
            actual => {
                self.unify(actual, Type::Vector, operands[0].span.clone())?;
                Ok(Type::Any)
            }
        }
    }

    fn infer_pair_accessor(
        &mut self,
        operands: &[Spanned<Expr>],
        operand_tys: Vec<Type>,
        span: SourceSpan,
        result: ListAccessResult,
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        let [operand_ty]: [Type; 1] =
            operand_tys
                .try_into()
                .map_err(|operand_tys: Vec<Type>| TypeError::ArityMismatch {
                    expected: "1".to_string(),
                    actual: operand_tys.len(),
                    span,
                })?;

        let index = match result {
            ListAccessResult::Element => 0,
            ListAccessResult::Tail => 1,
        };
        if let Some(ty) = visible_indexed_list_type(&operands[0], index, result, env) {
            return Ok(ty);
        }

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
        env: &TypeEnv,
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
        let visible_final_arguments = self.infer_visible_proper_list_items(final_operand, env)?;

        if let Some(constructor) = constructor_kind(&operands[0], env) {
            return self.infer_apply_constructor(
                constructor,
                arguments,
                final_list,
                final_operand,
                visible_final_arguments,
            );
        }

        match self.resolve(procedure_ty) {
            Type::Var(name) => {
                let Some(final_arguments) = visible_final_arguments else {
                    let rest = self.infer_apply_final_list(final_list, final_operand)?;
                    let result = self.fresh_type_var();
                    let procedure = match rest {
                        ApplyFinalList::Empty => Type::procedure(arguments, result.clone()),
                        ApplyFinalList::Rest(element) if arguments.is_empty() => {
                            Type::uniform_variadic(element, result.clone())
                        }
                        ApplyFinalList::Rest(element) => {
                            Type::rest_procedure(arguments, element, result.clone())
                        }
                    };
                    self.substitutions.insert(name, procedure);
                    return Ok(self.resolve(result));
                };
                self.unify(final_list, Type::List, final_operand.span.clone())?;

                let mut params = arguments;
                params.extend(final_arguments.into_iter().map(|ty| self.resolve(ty)));
                let result = self.fresh_type_var();
                self.substitutions.insert(
                    name,
                    Type::procedure(
                        params
                            .into_iter()
                            .map(|ty| self.resolve(ty))
                            .collect::<Vec<_>>(),
                        result.clone(),
                    ),
                );
                Ok(self.resolve(result))
            }
            Type::Procedure(
                procedure @ (ProcedureType::Fixed { .. }
                | ProcedureType::Optional { .. }
                | ProcedureType::Predicate { .. }),
            ) => {
                let Some(final_arguments) = visible_final_arguments else {
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

    fn infer_apply_final_list(
        &mut self,
        actual: Type,
        operand: &Spanned<Expr>,
    ) -> Result<ApplyFinalList, TypeError> {
        match self.resolve(actual) {
            Type::Null => Ok(ApplyFinalList::Empty),
            Type::ListOf(element) => Ok(ApplyFinalList::Rest(self.resolve(*element))),
            Type::List | Type::Any | Type::Unknown => Ok(ApplyFinalList::Rest(Type::Any)),
            Type::Var(name) => {
                let element = self.fresh_type_var();
                self.substitutions
                    .insert(name, Type::ListOf(Box::new(element.clone())));
                Ok(ApplyFinalList::Rest(element))
            }
            actual => {
                self.unify(actual, Type::List, operand.span.clone())?;
                Ok(ApplyFinalList::Rest(Type::Any))
            }
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
            ProcedureType::Predicate { param, .. } => {
                if operand_tys.len() != 1 {
                    return Err(TypeError::ArityMismatch {
                        expected: "1".to_string(),
                        actual: operand_tys.len(),
                        span: span_for_operands(operands),
                    });
                }

                let actual = operand_tys
                    .into_iter()
                    .next()
                    .expect("arity check ensures one operand");
                let operand = operands.first().expect("arity check ensures one operand");
                self.unify(actual, *param, operand.span.clone())?;
                Ok(Type::Boolean)
            }
        }
    }

    fn unify(&mut self, actual: Type, expected: Type, span: SourceSpan) -> Result<Type, TypeError> {
        let actual = self.resolve(actual);
        let expected = self.resolve(expected);

        match (actual, expected) {
            (Type::Unknown, ty) | (ty, Type::Unknown) | (Type::Any, ty) | (ty, Type::Any) => Ok(ty),
            (Type::Var(actual), Type::Var(expected)) => self.bind_var(expected, Type::Var(actual)),
            (Type::Var(name), ty) | (ty, Type::Var(name)) => self.bind_var(name, ty),
            (Type::Never, _) | (_, Type::Never) => Ok(Type::Never),
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
            (
                ProcedureType::Predicate {
                    param: actual_param,
                    positive: actual_positive,
                },
                ProcedureType::Predicate {
                    param: expected_param,
                    positive: expected_positive,
                },
            ) => {
                let param = self.unify(*actual_param, *expected_param, span.clone())?;
                let positive = self.unify(*actual_positive, *expected_positive, span)?;
                Ok(Type::predicate_procedure(param, positive))
            }
            (
                ProcedureType::Predicate { param, .. },
                ProcedureType::Fixed {
                    params,
                    result: expected_result,
                },
            ) if params.len() == 1 => {
                let param = self.unify(*param, params[0].clone(), span.clone())?;
                let result = self.unify(Type::Boolean, *expected_result, span)?;
                Ok(Type::procedure(vec![param], result))
            }
            (
                ProcedureType::Fixed {
                    params,
                    result: actual_result,
                },
                ProcedureType::Predicate { param, .. },
            ) if params.len() == 1 => {
                let param = self.unify(params[0].clone(), *param, span.clone())?;
                let result = self.unify(*actual_result, Type::Boolean, span)?;
                Ok(Type::procedure(vec![param], result))
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
            Type::Procedure(ProcedureType::Predicate { param, positive }) => {
                Type::predicate_procedure(self.resolve(*param), self.resolve(*positive))
            }
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
        Type::Procedure(ProcedureType::Predicate { param, positive }) => {
            contains_var(param, name) || contains_var(positive, name)
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
        Type::Procedure(ProcedureType::Predicate { param, positive }) => {
            has_type_var(param) || has_type_var(positive)
        }
        _ => false,
    }
}

fn type_of_atom(atom: &Atom) -> Type {
    match atom {
        Atom::Identifier(name) => primitive(name)
            .map(|primitive| primitive.signature)
            .unwrap_or_else(|| Type::Var(name.clone())),
        Atom::Integer(_)
        | Atom::Decimal(_)
        | Atom::Real(_, _)
        | Atom::ExactComplex(_)
        | Atom::Complex(_) => Type::Number,
        Atom::String(_) => Type::String,
        Atom::Boolean(_) => Type::Boolean,
        Atom::Character(_) => Type::Char,
    }
}

fn call_cc_receiver_type() -> Type {
    Type::procedure(
        vec![Type::procedure(vec![Type::Any], Type::Never)],
        Type::Any,
    )
}

fn call_with_values_result_for_params(params: Vec<Type>) -> Type {
    match params.as_slice() {
        [single] => single.clone(),
        _ => Type::Values(params),
    }
}

fn predicate_lambda_param_type(param_name: &str, ty: Type) -> Type {
    if same_type_var(&ty, &Type::Var(param_name.to_string())) {
        Type::Any
    } else {
        wildcard_type_vars(ty)
    }
}

fn generalize_direct_argument(ty: &Type) -> bool {
    matches!(ty, Type::Procedure(_)) && has_type_var(ty) && !procedure_returns_never(ty)
}

fn procedure_returns_never(ty: &Type) -> bool {
    match ty {
        Type::Procedure(ProcedureType::Fixed { result, .. })
        | Type::Procedure(ProcedureType::Optional { result, .. })
        | Type::Procedure(ProcedureType::UniformVariadic { result, .. })
        | Type::Procedure(ProcedureType::Rest { result, .. }) => result.as_ref() == &Type::Never,
        Type::Procedure(ProcedureType::Predicate { .. }) => false,
        _ => false,
    }
}

fn wildcard_type_vars(ty: Type) -> Type {
    match ty {
        Type::Var(_) => Type::Any,
        Type::Pair(car, cdr) => Type::Pair(
            Box::new(wildcard_type_vars(*car)),
            Box::new(wildcard_type_vars(*cdr)),
        ),
        Type::ListOf(element) => Type::ListOf(Box::new(wildcard_type_vars(*element))),
        Type::VectorOf(element) => Type::VectorOf(Box::new(wildcard_type_vars(*element))),
        Type::PromiseOf(element) => Type::PromiseOf(Box::new(wildcard_type_vars(*element))),
        Type::Values(types) => Type::Values(types.into_iter().map(wildcard_type_vars).collect()),
        Type::Procedure(ProcedureType::Fixed { params, result }) => Type::procedure(
            params
                .into_iter()
                .map(wildcard_type_vars)
                .collect::<Vec<_>>(),
            wildcard_type_vars(*result),
        ),
        Type::Procedure(ProcedureType::Optional {
            required,
            optional,
            result,
        }) => Type::optional_procedure(
            required
                .into_iter()
                .map(wildcard_type_vars)
                .collect::<Vec<_>>(),
            optional
                .into_iter()
                .map(wildcard_type_vars)
                .collect::<Vec<_>>(),
            wildcard_type_vars(*result),
        ),
        Type::Procedure(ProcedureType::UniformVariadic { param, result }) => {
            Type::uniform_variadic(wildcard_type_vars(*param), wildcard_type_vars(*result))
        }
        Type::Procedure(ProcedureType::Rest {
            required,
            rest,
            result,
        }) => Type::rest_procedure(
            required
                .into_iter()
                .map(wildcard_type_vars)
                .collect::<Vec<_>>(),
            wildcard_type_vars(*rest),
            wildcard_type_vars(*result),
        ),
        Type::Procedure(ProcedureType::Predicate { param, positive }) => {
            Type::predicate_procedure(wildcard_type_vars(*param), wildcard_type_vars(*positive))
        }
        Type::Union(types) => Type::union(
            types
                .into_iter()
                .map(wildcard_type_vars)
                .collect::<Vec<_>>(),
        ),
        ty => ty,
    }
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

fn literal_index(expr: &Spanned<Expr>) -> Option<usize> {
    match &expr.node {
        Expr::Literal(Atom::Integer(index)) => index.to_usize(),
        _ => None,
    }
}

fn visible_indexed_list_type(
    expr: &Spanned<Expr>,
    index: usize,
    result: ListAccessResult,
    env: &TypeEnv,
) -> Option<Type> {
    if let Some(vector) = primitive_unary_operand(expr, "vector->list", env) {
        return visible_indexed_vector_as_list_type(vector, index, result, env);
    }

    match &expr.node {
        Expr::Quote(datum) => match &datum.node {
            Datum::List(items) => match result {
                ListAccessResult::Element => items.get(index).map(type_of_datum),
                ListAccessResult::Tail if index <= items.len() => {
                    Some(type_of_list_datums(&items[index..]))
                }
                ListAccessResult::Tail => None,
            },
            _ => None,
        },
        Expr::Apply { operator, operands }
            if constructor_kind(operator, env) == Some(ConstructorKind::List) =>
        {
            match result {
                ListAccessResult::Element => operands.get(index).and_then(static_expr_type),
                ListAccessResult::Tail if index <= operands.len() => {
                    visible_expr_list_type(&operands[index..])
                }
                ListAccessResult::Tail => None,
            }
        }
        _ => None,
    }
}

fn visible_vector_item_type(expr: &Spanned<Expr>, index: usize, env: &TypeEnv) -> Option<Type> {
    if let Some(list) = primitive_unary_operand(expr, "list->vector", env) {
        return visible_indexed_list_type(list, index, ListAccessResult::Element, env);
    }

    match &expr.node {
        Expr::Quote(datum) => match &datum.node {
            Datum::Vector(items) => items.get(index).map(type_of_datum),
            _ => None,
        },
        Expr::Apply { operator, operands }
            if constructor_kind(operator, env) == Some(ConstructorKind::Vector) =>
        {
            operands.get(index).and_then(static_expr_type)
        }
        _ => None,
    }
}

fn visible_indexed_vector_as_list_type(
    expr: &Spanned<Expr>,
    index: usize,
    result: ListAccessResult,
    env: &TypeEnv,
) -> Option<Type> {
    if let Some(list) = primitive_unary_operand(expr, "list->vector", env) {
        return visible_indexed_list_type(list, index, result, env);
    }

    match &expr.node {
        Expr::Quote(datum) => match &datum.node {
            Datum::Vector(items) => match result {
                ListAccessResult::Element => items.get(index).map(type_of_datum),
                ListAccessResult::Tail if index <= items.len() => {
                    Some(type_of_list_datums(&items[index..]))
                }
                ListAccessResult::Tail => None,
            },
            _ => None,
        },
        Expr::Apply { operator, operands }
            if constructor_kind(operator, env) == Some(ConstructorKind::Vector) =>
        {
            match result {
                ListAccessResult::Element => operands.get(index).and_then(static_expr_type),
                ListAccessResult::Tail if index <= operands.len() => {
                    visible_expr_list_type(&operands[index..])
                }
                ListAccessResult::Tail => None,
            }
        }
        _ => None,
    }
}

fn visible_vector_as_list_type(expr: &Spanned<Expr>, env: &TypeEnv) -> Option<Type> {
    if let Some(list) = primitive_unary_operand(expr, "list->vector", env) {
        return visible_list_type(list, env);
    }

    match &expr.node {
        Expr::Quote(datum) => match &datum.node {
            Datum::Vector(items) => Some(type_of_list_datums(items)),
            _ => None,
        },
        Expr::Apply { operator, operands }
            if constructor_kind(operator, env) == Some(ConstructorKind::Vector) =>
        {
            visible_expr_list_type(operands)
        }
        _ => None,
    }
}

fn visible_list_as_vector_type(expr: &Spanned<Expr>, env: &TypeEnv) -> Option<Type> {
    if let Some(vector) = primitive_unary_operand(expr, "vector->list", env) {
        return visible_vector_type(vector, env);
    }

    match &expr.node {
        Expr::Quote(datum) => match &datum.node {
            Datum::List(items) => Some(type_of_vector_datums(items)),
            _ => None,
        },
        Expr::Apply { operator, operands }
            if constructor_kind(operator, env) == Some(ConstructorKind::List) =>
        {
            visible_expr_vector_type(operands)
        }
        _ => None,
    }
}

fn visible_list_type(expr: &Spanned<Expr>, env: &TypeEnv) -> Option<Type> {
    if let Some(vector) = primitive_unary_operand(expr, "vector->list", env) {
        return visible_vector_as_list_type(vector, env);
    }

    match &expr.node {
        Expr::Quote(datum) => match &datum.node {
            Datum::List(items) => Some(type_of_list_datums(items)),
            _ => None,
        },
        Expr::Apply { operator, operands }
            if constructor_kind(operator, env) == Some(ConstructorKind::List) =>
        {
            visible_expr_list_type(operands)
        }
        _ => None,
    }
}

fn visible_vector_type(expr: &Spanned<Expr>, env: &TypeEnv) -> Option<Type> {
    if let Some(list) = primitive_unary_operand(expr, "list->vector", env) {
        return visible_list_as_vector_type(list, env);
    }

    match &expr.node {
        Expr::Quote(datum) => match &datum.node {
            Datum::Vector(items) => Some(type_of_vector_datums(items)),
            _ => None,
        },
        Expr::Apply { operator, operands }
            if constructor_kind(operator, env) == Some(ConstructorKind::Vector) =>
        {
            visible_expr_vector_type(operands)
        }
        _ => None,
    }
}

fn static_expr_type(expr: &Spanned<Expr>) -> Option<Type> {
    match &expr.node {
        Expr::Literal(atom) => Some(type_of_atom(atom)),
        Expr::Quote(datum) => Some(type_of_datum(datum)),
        _ => None,
    }
}

fn visible_expr_list_type(items: &[Spanned<Expr>]) -> Option<Type> {
    if items.is_empty() {
        return Some(Type::Null);
    }

    Some(Type::ListOf(Box::new(Type::union(
        items
            .iter()
            .map(static_expr_type)
            .collect::<Option<Vec<_>>>()?,
    ))))
}

fn visible_expr_vector_type(items: &[Spanned<Expr>]) -> Option<Type> {
    if items.is_empty() {
        return Some(Type::Vector);
    }

    Some(Type::VectorOf(Box::new(Type::union(
        items
            .iter()
            .map(static_expr_type)
            .collect::<Option<Vec<_>>>()?,
    ))))
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
    abbreviation_type(_name, type_of_datum(datum))
}

fn abbreviation_type(_name: &'static str, datum: Type) -> Type {
    Type::ListOf(Box::new(Type::union(vec![Type::Symbol, datum])))
}

fn span_for_operands(operands: &[Spanned<Expr>]) -> SourceSpan {
    match (operands.first(), operands.last()) {
        (Some(first), Some(last)) => first.span.start..last.span.end,
        _ => 0..0,
    }
}

fn synthetic_operand(span: SourceSpan) -> Spanned<Expr> {
    Spanned::new(Expr::Literal(Atom::Boolean(true)), span)
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

fn branch_refinement_type(
    refinements: &[BranchRefinement],
    name: &str,
    branch: RefinedBranch,
) -> Option<Type> {
    refinements
        .iter()
        .filter(|refinement| refinement.branch == branch && refinement.name == name)
        .map(|refinement| refinement.positive.clone())
        .reduce(intersect_types)
}

fn disjoin_refinements(
    left: Vec<(String, Type)>,
    right: Vec<(String, Type)>,
) -> Vec<(String, Type)> {
    let left = refinement_map(left);
    let right = refinement_map(right);
    if left.is_empty() || left.keys().collect::<Vec<_>>() != right.keys().collect::<Vec<_>>() {
        return Vec::new();
    }

    left.into_iter()
        .map(|(name, left)| {
            let right = right
                .get(&name)
                .cloned()
                .expect("key equality ensures a right refinement");
            (name, Type::union(vec![left, right]))
        })
        .collect()
}

fn refinement_map(refinements: Vec<(String, Type)>) -> BTreeMap<String, Type> {
    refinements
        .into_iter()
        .fold(BTreeMap::new(), |mut map, (name, ty)| {
            map.entry(name)
                .and_modify(|existing| {
                    *existing = intersect_types(existing.clone(), ty.clone());
                })
                .or_insert(ty);
            map
        })
}

fn refined_branch_env(
    env: &TypeEnv,
    refinements: &[BranchRefinement],
    branch: RefinedBranch,
) -> (TypeEnv, bool) {
    let mut refined = env.clone();
    let mut dead = false;

    for refinement in refinements
        .iter()
        .filter(|refinement| refinement.branch == branch)
    {
        let ty = refined
            .get(&refinement.name)
            .cloned()
            .map(|existing| intersect_types(existing, refinement.positive.clone()))
            .unwrap_or_else(|| refinement.positive.clone());
        dead |= ty == Type::Never;
        refined.define(refinement.name.clone(), ty);
    }

    (refined, dead)
}

fn intersect_types(left: Type, right: Type) -> Type {
    match (left, right) {
        (Type::Never, _) | (_, Type::Never) => Type::Never,
        (Type::Unknown, ty) | (ty, Type::Unknown) | (Type::Any, ty) | (ty, Type::Any) => ty,
        (Type::Var(_), ty) | (ty, Type::Var(_)) => ty,
        (Type::Union(left), Type::Union(right)) => intersect_union(left, Type::Union(right)),
        (Type::Union(types), ty) | (ty, Type::Union(types)) => intersect_union(types, ty),
        (Type::Pair(left_car, left_cdr), Type::Pair(right_car, right_cdr)) => {
            let car = intersect_types(*left_car, *right_car);
            let cdr = intersect_types(*left_cdr, *right_cdr);
            if car == Type::Never || cdr == Type::Never {
                Type::Never
            } else {
                Type::Pair(Box::new(car), Box::new(cdr))
            }
        }
        (Type::ListOf(left), Type::ListOf(right)) => match intersect_types(*left, *right) {
            Type::Never => Type::Null,
            element => Type::ListOf(Box::new(element)),
        },
        (Type::List, Type::ListOf(element)) | (Type::ListOf(element), Type::List) => {
            Type::ListOf(element)
        }
        (Type::List, Type::Null) | (Type::Null, Type::List) => Type::Null,
        (Type::ListOf(_), Type::Null) | (Type::Null, Type::ListOf(_)) => Type::Null,
        (Type::VectorOf(left), Type::VectorOf(right)) => match intersect_types(*left, *right) {
            Type::Never => Type::Never,
            element => Type::VectorOf(Box::new(element)),
        },
        (Type::Vector, Type::VectorOf(element)) | (Type::VectorOf(element), Type::Vector) => {
            Type::VectorOf(element)
        }
        (Type::PromiseOf(left), Type::PromiseOf(right)) => match intersect_types(*left, *right) {
            Type::Never => Type::Never,
            element => Type::PromiseOf(Box::new(element)),
        },
        (Type::Values(left), Type::Values(right)) if left.len() == right.len() => {
            let values = left
                .into_iter()
                .zip(right)
                .map(|(left, right)| intersect_types(left, right))
                .collect::<Vec<_>>();
            if values.iter().any(|ty| ty == &Type::Never) {
                Type::Never
            } else {
                Type::Values(values)
            }
        }
        (Type::Port, Type::InputPort) | (Type::InputPort, Type::Port) => Type::InputPort,
        (Type::Port, Type::OutputPort) | (Type::OutputPort, Type::Port) => Type::OutputPort,
        (left, right) if left == right => left,
        _ => Type::Never,
    }
}

fn intersect_union(types: Vec<Type>, ty: Type) -> Type {
    Type::union(
        types
            .into_iter()
            .map(|item| intersect_types(item, ty.clone()))
            .collect::<Vec<_>>(),
    )
}

fn is_false_literal(expr: &Spanned<Expr>) -> bool {
    matches!(expr.node, Expr::Literal(Atom::Boolean(false)))
}

fn is_true_literal(expr: &Spanned<Expr>) -> bool {
    matches!(expr.node, Expr::Literal(Atom::Boolean(true)))
}

fn visible_apply_predicate_argument<'a>(
    operands: &'a [Spanned<Expr>],
    env: &TypeEnv,
) -> Option<&'a Spanned<Expr>> {
    if operands.len() < 2 {
        return None;
    }

    let final_operand = operands.last()?;
    let mut arguments = operands[1..operands.len() - 1].iter().collect::<Vec<_>>();
    arguments.extend(visible_apply_final_expr_items(final_operand, env)?);

    match arguments.as_slice() {
        [argument] => Some(*argument),
        _ => None,
    }
}

fn visible_apply_final_expr_items<'a>(
    expr: &'a Spanned<Expr>,
    env: &TypeEnv,
) -> Option<Vec<&'a Spanned<Expr>>> {
    match &expr.node {
        Expr::Quote(datum) if matches!(&datum.node, Datum::List(items) if items.is_empty()) => {
            Some(Vec::new())
        }
        Expr::Apply { operator, operands }
            if constructor_kind(operator, env) == Some(ConstructorKind::List) =>
        {
            Some(operands.iter().collect())
        }
        _ => None,
    }
}

fn predicate_operand_refinement(
    operand: &Spanned<Expr>,
    positive: Type,
    env: &TypeEnv,
) -> Option<(String, Type)> {
    match &operand.node {
        Expr::Variable(variable_name) => Some((variable_name.clone(), positive)),
        Expr::Apply { .. } => {
            let (variable_name, steps) = accessor_operand_path(operand, env)?;
            Some((variable_name, accessor_refinement_type(&steps, positive)))
        }
        _ => None,
    }
}

fn accessor_operand_path(
    operand: &Spanned<Expr>,
    env: &TypeEnv,
) -> Option<(String, Vec<ListAccessResult>)> {
    let Expr::Apply { operator, operands } = &operand.node else {
        return None;
    };
    let [target] = operands.as_slice() else {
        return None;
    };
    let steps = accessor_refinement_steps(operator, env)?;

    match &target.node {
        Expr::Variable(variable_name) => Some((variable_name.clone(), steps)),
        Expr::Apply { .. } => {
            let (variable_name, mut target_steps) = accessor_operand_path(target, env)?;
            target_steps.extend(steps);
            Some((variable_name, target_steps))
        }
        _ => None,
    }
}

fn accessor_refinement_steps(
    operator: &Spanned<Expr>,
    env: &TypeEnv,
) -> Option<Vec<ListAccessResult>> {
    match primitive_operator_name(operator, env)? {
        "car" => Some(vec![ListAccessResult::Element]),
        "cdr" => Some(vec![ListAccessResult::Tail]),
        name => composed_accessor_steps(name),
    }
}

fn accessor_refinement_type(steps: &[ListAccessResult], positive: Type) -> Type {
    steps.iter().rev().fold(positive, |inner, step| match step {
        ListAccessResult::Element => Type::Pair(Box::new(inner), Box::new(Type::Any)),
        ListAccessResult::Tail => Type::Pair(Box::new(Type::Any), Box::new(inner)),
    })
}

fn variable_name(expr: &Spanned<Expr>) -> Option<&String> {
    match &expr.node {
        Expr::Variable(name) => Some(name),
        _ => None,
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

    fn infer_error_with_env(input: &str, env: &mut TypeEnv) -> TypeError {
        let datums = parse(input).unwrap();
        let program = classify_program(&datums).unwrap();
        let mut inferencer = Inferencer::new();
        inferencer.infer_program(&program, env).unwrap_err()
    }

    #[test]
    fn displays_type_error_details() {
        assert_eq!(
            infer_error("(+ \"x\" 1)").to_string(),
            "type constraint conflict: expected number?, got string?"
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
    fn rejects_impossible_arithmetic_lambda_bodies() {
        assert_eq!(
            infer_error("(lambda (x) (+ x \"hello\"))").to_string(),
            "type constraint conflict: expected number?, got string?"
        );
        assert_eq!(
            infer_error("(define (broken x) (+ x \"hello\"))").to_string(),
            "type constraint conflict: expected number?, got string?"
        );
    }

    #[test]
    fn rejects_predicate_refined_branch_conflicts() {
        assert_eq!(
            infer_error("(lambda (x) (if (string? x) (+ x 1) 0))").to_string(),
            "type constraint conflict: expected number?, got string?"
        );
        assert_eq!(
            infer_error("(lambda (x) (if (not (number? x)) 0 (+ x \"hello\")))").to_string(),
            "type constraint conflict: expected number?, got string?"
        );
        assert_eq!(
            infer_error("(lambda (x) (or (not (string? x)) (+ x 1)))").to_string(),
            "type constraint conflict: expected number?, got string?"
        );
    }

    #[test]
    fn failed_recursive_definitions_do_not_update_type_env() {
        let mut env = TypeEnv::new();

        assert_eq!(
            infer_error_with_env("(define (broken x) (+ x \"hello\"))", &mut env).to_string(),
            "type constraint conflict: expected number?, got string?"
        );

        assert!(env.binding("broken").is_none());
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
            "(-> x rest * (listof rest))"
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
    fn infers_direct_lambda_applications_without_local_name_leaks() {
        assert_eq!(infer_one("(lambda (x) (let ((y x)) y))"), "(-> x x)");
        assert_eq!(infer_one("(lambda (x) (or x 1))"), "(-> x (U number? x))");
        assert_eq!(
            infer_one("(lambda (x) (case x ((a) 1) ((b) \"b\") (else x)))"),
            "(-> x (U number? string? x))"
        );
    }

    #[test]
    fn generalizes_direct_lambda_arguments() {
        assert_eq!(
            infer_one("(let ((id (lambda (x) x))) (list (id 1) (id \"x\")))"),
            "(listof (U number? string?))"
        );
        assert_eq!(
            infer_one("(let ((wrap (lambda (x) (list x)))) (list (wrap 1) (wrap \"x\")))"),
            "(listof (listof (U number? string?)))"
        );
        assert_eq!(
            infer_one("((lambda (id) (list (id 1) (id \"x\"))) (lambda (x) x))"),
            "(listof (U number? string?))"
        );
        assert_eq!(infer_one("(call/cc (lambda (k) (k 5)))"), "number?");
    }

    #[test]
    fn preserves_caller_names_when_unifying_type_variables() {
        assert_eq!(infer_one("(lambda (x) (let loop ((y x)) y))"), "(-> x x)");
        assert_eq!(infer_one("(lambda (x) (do ((y x y)) (#t y)))"), "(-> x x)");
        assert_eq!(
            infer_one("(lambda (x) (letrec ((f (lambda (y) y))) (f x)))"),
            "(-> x x)"
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
        assert_eq!(
            infer_all("(define x 1) (set! x 2)"),
            vec!["number?".to_string(), "unspecified?".to_string()]
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
        assert_eq!(infer_one("(set-car! (cons 1 2) 9)"), "unspecified?");
        assert_eq!(infer_one("(set-cdr! (cons 1 2) 9)"), "unspecified?");
    }

    #[test]
    fn infers_pair_accessors_over_lists() {
        assert_eq!(
            infer_one("cadddr"),
            "(-> (pair? any? (pair? any? (pair? any? (pair? t0 any?)))) t0)"
        );
        assert_eq!(infer_one("(car '(1 2))"), "number?");
        assert_eq!(infer_one("(cdr '(1 2))"), "(listof number?)");
        assert_eq!(infer_one("(car '(1 \"x\"))"), "number?");
        assert_eq!(infer_one("(cdr '(1 \"x\"))"), "(listof string?)");
        assert_eq!(infer_one("(cdr '(1))"), "null?");
        assert_eq!(infer_one("(car (list 1 \"x\"))"), "number?");
        assert_eq!(infer_one("(cdr (list 1 \"x\"))"), "(listof string?)");
        assert_eq!(infer_one("(car (cons \"x\" 2))"), "string?");
        assert_eq!(infer_one("(cdr (cons \"x\" 2))"), "number?");
    }

    #[test]
    fn infers_character_comparison_lambda() {
        assert_eq!(
            infer_one("(lambda (c) (char=? c #\\a))"),
            "(-> char? boolean?)"
        );
        assert_eq!(
            infer_error("(char=? #\\a #\\a #\\a)").to_string(),
            "wrong number of arguments: expected 2, got 3"
        );
    }

    #[test]
    fn infers_character_classification_and_case() {
        assert_eq!(
            infer_one("(lambda (c) (char-alphabetic? c))"),
            "(-> char? boolean? : char?)"
        );
        assert_eq!(infer_one("(char-upcase #\\a)"), "char?");
    }

    #[test]
    fn infers_simple_predicate_lambdas_latently() {
        assert_eq!(
            infer_one("(lambda (x) (string? x))"),
            "(-> any? boolean? : string?)"
        );
        assert_eq!(
            infer_one("(lambda (x) (zero? x))"),
            "(-> number? boolean? : number?)"
        );
        assert_eq!(
            infer_one("(lambda (x) (number? (car x)))"),
            "(-> (pair? any? any?) boolean? : (pair? number? any?))"
        );
        assert_eq!(
            infer_one("(lambda (x) (if (string? x) #t #f))"),
            "(-> any? boolean? : string?)"
        );
        assert_eq!(
            infer_one("(lambda (x) (and (string? x) #t))"),
            "(-> any? boolean? : string?)"
        );
        assert_eq!(
            infer_one("(lambda (x) (and (pair? x) (number? (car x))))"),
            "(-> any? boolean? : (pair? number? any?))"
        );
        assert_eq!(
            infer_one("(lambda (x) (or (string? x) #f))"),
            "(-> any? boolean? : string?)"
        );
        assert_eq!(
            infer_one("(lambda (x) (or (string? x) (number? x)))"),
            "(-> any? boolean? : (U number? string?))"
        );
        assert_eq!(
            infer_one("(lambda (x) (if (string? x) #t (number? x)))"),
            "(-> any? boolean? : (U number? string?))"
        );
        assert_eq!(
            infer_one("(lambda (x) (cond ((string? x) #t) ((number? x) #t) (else #f)))"),
            "(-> any? boolean? : (U number? string?))"
        );
        assert_eq!(
            infer_all(
                "(define stringy? (lambda (x) (string? x)))
                 (lambda (proc x) (if (stringy? x) (proc x) #f))"
            ),
            vec![
                "(-> any? boolean? : string?)".to_string(),
                "(-> (-> string? t1) any? (U boolean? t1))".to_string(),
            ]
        );
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
            infer_one("(lambda (x) (if (string? x) (string-length x) #f))"),
            "(-> x (U boolean? number?))"
        );
        assert_eq!(
            infer_one("(lambda (x flag) (if (and (string? x) flag) (string-length x) 0))"),
            "(-> x flag number?)"
        );
        assert_eq!(
            infer_one("(lambda (x flag) (if (and flag (string? x)) (string-length x) 0))"),
            "(-> x flag number?)"
        );
        assert_eq!(
            infer_one("(lambda (x y) (if (and (string? x) (number? y)) y 0))"),
            "(-> x y number?)"
        );
        assert_eq!(
            infer_one("(lambda (x y) (if (not (and (string? x) (number? y))) 0 y))"),
            "(-> x y number?)"
        );
        assert_eq!(
            infer_one("(lambda (x) (and (string? x) (string-length x)))"),
            "(-> x (U boolean? number?))"
        );
        assert_eq!(
            infer_one("(lambda (x) (cond ((string? x) #t) (else (+ x 1))))"),
            "(-> (U number? string?) (U boolean? number?))"
        );
    }

    #[test]
    fn propagates_pair_accessor_predicate_refinements() {
        assert_eq!(
            infer_one("(lambda (x) (if (and (pair? x) (number? (car x))) (car x) 0))"),
            "(-> x number?)"
        );
        assert_eq!(
            infer_one("(lambda (x) (if (and (pair? x) (number? (cdr x))) (cdr x) 0))"),
            "(-> x number?)"
        );
        assert_eq!(
            infer_one(
                "(lambda (x)
                   (if (and (pair? x) (number? (car x)) (number? (cdr x)))
                       (cons (car x) (cdr x))
                       #f))"
            ),
            "(-> x (U boolean? (pair? number? number?)))"
        );
    }

    #[test]
    fn propagates_composed_accessor_predicate_refinements() {
        assert_eq!(
            infer_one(
                "(lambda (x)
                   (if (and (pair? x) (pair? (cdr x)) (number? (cadr x)))
                       (list (cadr x))
                       (quote ())))"
            ),
            "(-> x (listof number?))"
        );
        assert_eq!(
            infer_one(
                "(lambda (x)
                   (if (and (pair? x)
                            (pair? (cdr x))
                            (pair? (cddr x))
                            (string? (caddr x)))
                       (list (caddr x))
                       (quote ())))"
            ),
            "(-> x (listof string?))"
        );
        assert_eq!(
            infer_one(
                "(lambda (x)
                   (if (and (pair? x) (pair? (cdr x)) (number? (cadr x)))
                       (cons (cadr x) (cddr x))
                       #f))"
            ),
            "(-> x (U boolean? (pair? number? t1)))"
        );
    }

    #[test]
    fn propagates_nested_accessor_predicate_refinements() {
        assert_eq!(
            infer_one(
                "(lambda (x)
                   (if (and (pair? x) (pair? (cdr x)) (number? (car (cdr x))))
                       (list (car (cdr x)))
                       (quote ())))"
            ),
            "(-> x (listof number?))"
        );
        assert_eq!(
            infer_one(
                "(lambda (x)
                   (if (and (pair? x)
                            (pair? (cdr x))
                            (pair? (cdr (cdr x)))
                            (string? (car (cdr (cdr x)))))
                       (list (car (cdr (cdr x))))
                       (quote ())))"
            ),
            "(-> x (listof string?))"
        );
    }

    #[test]
    fn treats_contradictory_refinement_paths_as_never() {
        assert_eq!(
            infer_one("(lambda (x) (if (and (number? x) (string? x)) \"dead\" 0))"),
            "(-> x number?)"
        );
        assert_eq!(
            infer_one(
                "(lambda (x)
                   (if (number? x)
                       (if (string? x) \"dead\" (+ x 5))
                       0))"
            ),
            "(-> x number?)"
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
            "(-> any? boolean? : string?)"
        );
        assert_eq!(
            infer_one("(lambda (pred x) (or (pred x) #f))"),
            "(-> (-> any? boolean? : t0) any? boolean?)"
        );
        assert_eq!(
            infer_one("(lambda (pred x) (or (apply pred (list x)) #f))"),
            "(-> (-> any? boolean? : t0) any? boolean?)"
        );
        assert_eq!(
            infer_one("(lambda (pred proc x) (or (pred x) (proc x)))"),
            "(-> (-> any? boolean? : t0) (-> any? t1) any? (U boolean? t1))"
        );
        assert_eq!(
            infer_one("(lambda (pred proc x) (or (not (pred x)) (proc x)))"),
            "(-> (-> any? boolean? : t0) (-> t0 t1) any? (U boolean? t1))"
        );
        assert_eq!(
            infer_one("(lambda (x flag) (or (and (string? x) flag) #f))"),
            "(-> x flag (U boolean? flag))"
        );
        assert_eq!(
            infer_one("(lambda (x) (if (or (string? x) (number? x)) x #f))"),
            "(-> x (U boolean? number? string?))"
        );
        assert_eq!(
            infer_one("(lambda (x y) (if (or (string? x) (number? y)) x #f))"),
            "(-> x y (U boolean? x))"
        );
    }

    #[test]
    fn instantiates_procedure_predicate_result_refinements() {
        assert_eq!(
            infer_one("(lambda (x) (if (procedure? x) (x) \"fallback\"))"),
            "(-> x (U string? t0))"
        );
        assert_eq!(
            infer_one(
                "(lambda (x)
                   (if (number? x)
                       (+ x 10)
                       (if (string? x)
                           (string-append x \"!\")
                           (if (procedure? x)
                               (x)
                               \"unknown\"))))"
            ),
            "(-> x (U number? string? t0))"
        );
        assert_eq!(
            infer_one(
                "(lambda (x y)
                   (if (and (procedure? x) (procedure? y))
                       (list (x) (y))
                       '()))"
            ),
            "(-> x y (listof (U t0 t1)))"
        );
    }

    #[test]
    fn infers_latent_predicate_refinements() {
        assert_eq!(
            infer_one("(lambda (pred proc x) (if (pred x) (proc x) #f))"),
            "(-> (-> any? boolean? : t0) (-> t0 t1) any? (U boolean? t1))"
        );
        assert_eq!(
            infer_one("(lambda (pred proc x) (if (apply pred (list x)) (proc x) #f))"),
            "(-> (-> any? boolean? : t0) (-> t0 t1) any? (U boolean? t1))"
        );
        assert_eq!(
            infer_one("(lambda (pred proc x) (if (apply pred x (quote ())) (proc x) #f))"),
            "(-> (-> any? boolean? : t0) (-> t0 t1) any? (U boolean? t1))"
        );
        assert_eq!(
            infer_one("(lambda (x) (if (apply string? (list x)) (string-length x) 0))"),
            "(-> x number?)"
        );
        assert_eq!(
            infer_one("(lambda (pred x) (if (pred x) x #f))"),
            "(-> (-> any? boolean? : t0) any? (U boolean? t0))"
        );
        assert_eq!(
            infer_one(
                "((lambda (pred proc x) (if (pred x) (proc x) #f))
                  string? string-length \"hi\")"
            ),
            "(U boolean? number?)"
        );
        assert_eq!(
            infer_one(
                "(lambda (pred proc x)
                   (if (and (pair? x) (pred (car x)))
                       (proc (car x))
                       #f))"
            ),
            "(-> (-> any? boolean? : t0) (-> t0 t1) x (U boolean? t1))"
        );
        assert_eq!(
            infer_one(
                "((lambda (pred proc x) (if (pred x) (proc x) #f))
                  zero? + 0)"
            ),
            "(U boolean? number?)"
        );
        assert_eq!(
            infer_one(
                "((lambda (pred proc x) (if (pred x) (proc x) #f))
                  char-alphabetic? char-upcase #\\a)"
            ),
            "(U boolean? char?)"
        );
        assert_eq!(
            infer_one("(lambda (pred proc x) (if (pred x) (apply proc (list x)) #f))"),
            "(-> (-> any? boolean? : t0) (-> t0 t1) any? (U boolean? t1))"
        );
    }

    #[test]
    fn covers_occurrence_typing_stress_examples() {
        assert_eq!(
            infer_one(
                "(lambda (x)
                   (if (number? x)
                       (+ x 10)
                       (if (string? x)
                           (string-append x \"!\")
                           (if (procedure? x)
                               (x)
                               \"unknown\"))))"
            ),
            "(-> x (U number? string? t0))"
        );
        assert_eq!(
            infer_one(
                "(lambda (x)
                   (if (and (pair? x) (number? (car x)) (number? (cdr x)))
                       (+ (car x) (cdr x))
                       0))"
            ),
            "(-> x number?)"
        );
        assert_eq!(
            infer_one(
                "(lambda (x)
                   (if (number? x)
                       (if (string? x)
                           (string-length x)
                           (+ x 5))
                       0))"
            ),
            "(-> x number?)"
        );
        assert_eq!(
            infer_one("(lambda (pred proc x) (if (pred x) (proc x) #f))"),
            "(-> (-> any? boolean? : t0) (-> t0 t1) any? (U boolean? t1))"
        );
    }

    #[test]
    fn rejects_latent_predicate_call_site_conflicts() {
        assert_eq!(
            infer_error(
                "((lambda (pred proc x) (if (pred x) (proc x) #f))
                  string? + \"hi\")"
            )
            .to_string(),
            "type constraint conflict: expected number?, got string?"
        );
        assert_eq!(
            infer_error(
                "((lambda (pred proc x) (if (pred x) (proc x) #f))
                  number? string-length 1)"
            )
            .to_string(),
            "type constraint conflict: expected string?, got number?"
        );
        assert_eq!(
            infer_error(
                "((lambda (pred proc x) (if (pred x) (apply proc (list x)) #f))
                  string? + \"hi\")"
            )
            .to_string(),
            "type constraint conflict: expected number?, got string?"
        );
        assert_eq!(
            infer_error(
                "((lambda (pred proc x) (if (apply pred (list x)) (proc x) #f))
                  string? + \"hi\")"
            )
            .to_string(),
            "type constraint conflict: expected number?, got string?"
        );
        assert_eq!(
            infer_error(
                "(define stringy? (lambda (x) (string? x)))
                 ((lambda (proc x) (if (stringy? x) (proc x) #f)) + \"hi\")"
            )
            .to_string(),
            "type constraint conflict: expected number?, got string?"
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
        assert_eq!(infer_one("(let - ((n (- 1))) n)"), "number?");
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
        assert_eq!(
            infer_one(
                "(lambda (x)
                   (cond ((string? x) => (lambda (ok) (string-length x)))
                         (else 0)))"
            ),
            "(-> x number?)"
        );
        assert_eq!(
            infer_one(
                "(lambda (x)
                   (cond ((and (pair? x) (number? (car x)))
                          => (lambda (ok) (car x)))
                         (else 0)))"
            ),
            "(-> x number?)"
        );
        assert_eq!(
            infer_one(
                "(lambda (x)
                   ((lambda (ok) (if ok (string-length x) 0))
                    (string? x)))"
            ),
            "(-> x number?)"
        );
        assert_eq!(
            infer_one("(lambda (x xs) (cond ((member x xs) => car) (else #f)))"),
            "(-> x (listof t0) (U boolean? t0))"
        );
        assert_eq!(
            infer_one("(lambda (x xs) (cond ((assoc x xs) => cdr) (else #f)))"),
            "(-> x (listof (pair? t1 t2)) (U boolean? t2))"
        );
        assert_eq!(
            infer_one("(lambda (x) (cond ((member x (quote ())) => car) (else 0)))"),
            "(-> x number?)"
        );
    }

    #[test]
    fn infers_do_result_type() {
        assert_eq!(
            infer_one("(do ((i 0 (+ i 1)) (acc 0 (+ acc i))) ((= i 5) acc))"),
            "number?"
        );
        assert_eq!(infer_one("(do ((i 0 (+ i 1))) ((= i 1)))"), "unspecified?");
    }

    #[test]
    fn infers_omitted_if_alternate_as_unspecified() {
        assert_eq!(infer_one("(if #f 1)"), "(U number? unspecified?)");
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
            infer_one("(call-with-values (lambda () (values 1 \"x\")) list)"),
            "(listof (U number? string?))"
        );
        assert_eq!(
            infer_one("(call-with-values (lambda () (values 1 \"x\")) vector)"),
            "(vectorof (U number? string?))"
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
        assert_eq!(
            infer_one("(lambda (producer) (call-with-values producer list))"),
            "(-> (-> t0) (listof t0))"
        );
        assert_eq!(
            infer_one("(lambda (producer) (call-with-values producer +))"),
            "(-> (-> number?) number?)"
        );
        assert_eq!(
            infer_one("(lambda (producer) (call-with-values producer (lambda (x y) (+ x y))))"),
            "(-> (-> (values number? number?)) number?)"
        );
        assert_eq!(
            infer_one("(lambda (producer) (call-with-values producer (lambda () 1)))"),
            "(-> (-> (values)) number?)"
        );
        assert_eq!(
            infer_error("(call-with-values 1 list)").to_string(),
            "expected a procedure, got number?"
        );
    }

    #[test]
    fn infers_quasiquote_shapes() {
        assert_eq!(infer_one("`(1 2 3)"), "(listof number?)");
        assert_eq!(infer_one("`#(1 \"x\")"), "(vectorof (U number? string?))");
        assert_eq!(infer_one("`(1 ,(+ 1 2))"), "(listof number?)");
        assert_eq!(
            infer_one("`#(1 ,(string-append \"a\" \"b\"))"),
            "(vectorof (U number? string?))"
        );
        assert_eq!(infer_one("(lambda xs `(,@xs))"), "(-> xs * (listof xs))");
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
    fn normalizes_collection_branch_unions() {
        assert_eq!(
            infer_one("(if #t '(1) '(\"x\"))"),
            "(listof (U number? string?))"
        );
        assert_eq!(infer_one("(if #t '() '(1))"), "(listof number?)");
        assert_eq!(
            infer_one("(if #t '#(1) '#(\"x\"))"),
            "(vectorof (U number? string?))"
        );
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
            infer_one("(apply (lambda (x y) (+ x y)) (list 1 2))"),
            "number?"
        );
        assert_eq!(
            infer_one("(apply (lambda (x y) (+ x y)) (vector->list (vector 1 2)))"),
            "number?"
        );
        assert_eq!(
            infer_one("(lambda (x y) (apply (lambda (a b) (+ a b)) (list x y)))"),
            "(-> number? number? number?)"
        );
        assert_eq!(
            infer_one("(lambda (f xs) (apply f xs))"),
            "(-> (->* t0 t1) (listof t0) t1)"
        );
        assert_eq!(
            infer_one("(lambda (f x xs) (apply f x xs))"),
            "(-> (-> x t0 * t1) x (listof t0) t1)"
        );
        assert_eq!(
            infer_error("(apply (lambda (x y) (+ x y)) '(1 2 3))").to_string(),
            "wrong number of arguments: expected 2, got 3"
        );
        assert_eq!(
            infer_error("(apply (lambda (x y) (+ x y)) (list 1 2 3))").to_string(),
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
        assert_eq!(infer_one("(vector-ref (vector 1 \"x\") 0)"), "number?");
        assert_eq!(infer_one("(vector-ref '#(1 \"x\") 1)"), "string?");
        assert_eq!(
            infer_one("(lambda (x) (vector-ref (vector x \"x\") 1))"),
            "(-> x string?)"
        );
        assert_eq!(infer_one("(vector-length (vector 1 2 3))"), "number?");
        assert_eq!(infer_one("(make-vector 3)"), "(vectorof any?)");
        assert_eq!(infer_one("(make-vector 3 #\\a)"), "(vectorof char?)");
        assert_eq!(infer_one("(vector->list (vector))"), "null?");
        assert_eq!(
            infer_one("(vector->list (vector #\\a #\\b))"),
            "(listof char?)"
        );
        assert_eq!(infer_one("(list->vector '())"), "vector?");
        assert_eq!(infer_one("(list->vector '(1 2 3))"), "(vectorof number?)");
        assert_eq!(
            infer_one("(lambda (x) (vector-ref (list->vector (list x \"x\")) 1))"),
            "(-> x string?)"
        );
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
    fn respects_primitive_shadowing() {
        assert_eq!(
            infer_one("((lambda (list) (list 1 \"x\")) (lambda (x y) x))"),
            "number?"
        );
        assert_eq!(
            infer_one("((lambda (map) (map 1)) (lambda (x) \"ok\"))"),
            "string?"
        );
        assert_eq!(
            infer_one("((lambda (apply) (apply 1)) (lambda (x) \"ok\"))"),
            "string?"
        );
        assert_eq!(
            infer_one("((lambda (vector) (apply vector 1 '(\"x\"))) (lambda (x y) x))"),
            "number?"
        );
        assert_eq!(
            infer_one("((lambda (list) (map list '(1) '(\"x\"))) (lambda (x y) x))"),
            "(listof number?)"
        );
        assert_eq!(
            infer_one(
                "((lambda (list)
                    (call-with-values (lambda () (values 1 \"x\")) list))
                  (lambda (x y) x))"
            ),
            "number?"
        );
        assert_eq!(
            infer_one("(lambda (string? x) (if (string? x) (+ x 1) 0))"),
            "(-> (-> any? boolean? : number?) any? number?)"
        );
        assert_eq!(
            infer_one(
                "(let ((car (lambda (x) x)))
                   (lambda (x) (if (number? (car x)) x #f)))"
            ),
            "(-> x (U boolean? x))"
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
        assert_eq!(
            infer_one("(lambda (x) (if (eq? x (quote done)) x #f))"),
            "(-> x (U boolean? symbol?))"
        );
        assert_eq!(
            infer_one("(lambda (x) (if (eqv? #\\a x) x #f))"),
            "(-> x (U boolean? char?))"
        );
        assert_eq!(
            infer_one("(lambda (x) (if (equal? x (quote (1 2))) x #f))"),
            "(-> x (U boolean? (listof number?)))"
        );
        assert_eq!(
            infer_one("(lambda (eq? x) (if (eq? x (quote done)) x #f))"),
            "(-> (-> x symbol? t0) x (U boolean? x))"
        );
    }

    #[test]
    fn infers_indexed_list_primitives() {
        assert_eq!(infer_one("list-ref"), "(-> (listof t0) number? t0)");
        assert_eq!(
            infer_one("list-tail"),
            "(-> (listof t0) number? (listof t0))"
        );
        assert_eq!(infer_one("(list)"), "null?");
        assert_eq!(infer_one("(list 1 \"x\")"), "(listof (U number? string?))");
        assert_eq!(infer_one("(car (list 1 \"x\"))"), "number?");
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
        assert_eq!(infer_one("(list-ref '(1 \"x\") 0)"), "number?");
        assert_eq!(infer_one("(list-ref '(1 \"x\") 1)"), "string?");
        assert_eq!(infer_one("(list-tail '(1 \"x\") 1)"), "(listof string?)");
        assert_eq!(infer_one("(list-tail '(1 \"x\") 2)"), "null?");
        assert_eq!(infer_one("(list-ref (list 1 \"x\") 0)"), "number?");
        assert_eq!(
            infer_one("(lambda (x) (list-ref (list x \"x\") 1))"),
            "(-> x string?)"
        );
        assert_eq!(
            infer_one("(lambda (x) (list-tail (list x \"x\") 1))"),
            "(-> x (listof string?))"
        );
        assert_eq!(
            infer_one("(lambda (x) (list-ref (vector->list (vector x \"x\")) 1))"),
            "(-> x string?)"
        );
        assert_eq!(
            infer_one("(lambda (x) (list-tail (vector->list (vector x \"x\")) 1))"),
            "(-> x (listof string?))"
        );
        assert_eq!(
            infer_one("(lambda (xs) (string-length (list-ref xs 0)))"),
            "(-> (listof string?) number?)"
        );
    }

    #[test]
    fn infers_membership_primitives() {
        assert_eq!(
            infer_one("member"),
            "(-> any? (listof t0) (U boolean? (listof t0)))"
        );
        assert_eq!(infer_one("assoc"), "(-> any? (listof t0) (U boolean? t0))");
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
        assert_eq!(
            infer_one("map"),
            "(-> (-> t0 t0 * t1) (listof t0) (listof t0) * (listof t1))"
        );
        assert_eq!(
            infer_one("for-each"),
            "(-> (-> t0 t0 * any?) (listof t0) (listof t0) * unspecified?)"
        );
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
            "(-> (listof string?) unspecified?)"
        );
        assert_eq!(infer_one("(for-each + '(1 2) '(3 4))"), "unspecified?");
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
        assert_eq!(
            infer_one("call-with-input-file"),
            "(-> string? (-> input-port? t0) t0)"
        );
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
            "unspecified?"
        );
        assert_eq!(
            infer_one("(lambda (f) (call-with-input-file \"x\" f))"),
            "(-> (-> input-port? t0) t0)"
        );
        assert_eq!(
            infer_one("(lambda (f) (call-with-output-file \"x\" f))"),
            "(-> (-> output-port? t0) t0)"
        );
        assert_eq!(infer_one("(with-input-from-file \"x\" read)"), "any?");
        assert_eq!(
            infer_one("(with-output-to-file \"x\" (lambda () (write \"x\")))"),
            "unspecified?"
        );
        assert_eq!(
            infer_one("(lambda (thunk) (with-output-to-file \"x\" thunk))"),
            "(-> (-> t0) t0)"
        );
        assert_eq!(infer_one("(load \"x\")"), "unspecified?");
        assert_eq!(
            infer_one("(eval '(+ 1 2) (scheme-report-environment 5))"),
            "any?"
        );
        assert_eq!(infer_one("(interaction-environment)"), "any?");
        assert_eq!(
            infer_one("(dynamic-wind (lambda () 1) (lambda () 2) (lambda () 3))"),
            "number?"
        );
        assert_eq!(
            infer_one("dynamic-wind"),
            "(-> (-> any?) (-> t0) (-> any?) t0)"
        );
        assert_eq!(
            infer_one("(lambda (before thunk after) (dynamic-wind before thunk after))"),
            "(-> (-> any?) (-> t0) (-> any?) t0)"
        );
        assert_eq!(infer_one("(call/cc (lambda (k) 1))"), "number?");
        assert_eq!(infer_one("(call/cc (lambda (k) (k 5)))"), "number?");
        assert_eq!(
            infer_one("(call/cc (lambda (k) (+ (k 5) \"x\")))"),
            "number?"
        );
        assert_eq!(infer_one("(call/cc (lambda (k) ((k 5) \"x\")))"), "number?");
        assert_eq!(
            infer_one("(call-with-current-continuation (lambda (k) (if #t (k 5) \"x\")))"),
            "(U number? string?)"
        );
        assert_eq!(
            infer_one("(lambda (f) (call/cc f))"),
            "(-> (-> (-> t0 never?) t1) (U t0 t1))"
        );
        assert_eq!(infer_one("(write \"x\")"), "unspecified?");
    }

    #[test]
    fn infers_string_primitives() {
        assert_eq!(infer_one("(string #\\a #\\b)"), "string?");
        assert_eq!(infer_one("(string-ref \"abc\" 1)"), "char?");
        assert_eq!(infer_one("(string->list \"ab\")"), "(listof char?)");
        assert_eq!(infer_one("(list->string '(#\\a #\\b))"), "string?");
        assert_eq!(infer_one("(list->string '())"), "string?");
        assert_eq!(infer_one("(string-set! \"ab\" 0 #\\z)"), "unspecified?");
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
            "(-> x number?)"
        );
    }
}
