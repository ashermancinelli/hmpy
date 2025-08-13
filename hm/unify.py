from typing import Iterable

from hm.hm_ast import (
    AST,
    Apply,
    BoolLit,
    BinOp,
    Case,
    Identifier,
    IntLit,
    Lambda,
    Let,
    Letrec,
    Match,
    VariantDecl,
    BoolType,
    IntType,
    FloatType,
    NoneType,
)
from hm.util import TypeCheckError, log, logwrap

from hm.type_expr import TypeExpr, TypeVariable, TypeOperator, Function

Env = dict[str, TypeExpr]


class TypeCheck:
    def __init__(self):
        # self.environment: dict[str, TypeExpr] = environment
        pass

    def __str__(self):
        return "TypeCheck()"

    @logwrap
    def infer(self, ast: AST, env: Env, concrete_types: set[TypeExpr]) -> TypeExpr:
        inferred_type = self._infer(ast, env, concrete_types)
        ast.type = inferred_type
        return inferred_type

    @logwrap
    def _infer(self, ast: AST, env: Env, concrete_types: set[TypeExpr]) -> TypeExpr:
        match ast:
            case Identifier(name=name):
                if name.isnumeric():
                    return self.infer(IntLit(int(name)), env, concrete_types)
                return self.typeof(name, env, concrete_types)
            case IntLit():
                return IntType()
            case BoolLit():
                return BoolType()
            case BinOp(_, lhs, rhs):
                # TODO: type promotion rules
                lhs_type = self.infer(lhs, env, concrete_types)
                rhs_type = self.infer(rhs, env, concrete_types)
                self.unify_type_expressions(lhs_type, rhs_type)
                return lhs_type
            case Apply(fn=fn, arg=arg):
                fun_type = self.infer(fn, env, concrete_types)
                arg_type = self.infer(arg, env, concrete_types)
                result_type = TypeVariable()
                self.unify_type_expressions(Function(arg_type, result_type), fun_type)
                return result_type
            case Lambda(param=param, body=body):
                arg_type = TypeVariable()
                result_type = self.infer(
                    body,
                    env.copy() | {param: arg_type},
                    concrete_types.copy() | {arg_type},
                )
                return Function(arg_type, result_type)
            case Let(name=name, value=value, body=body):
                value_type = self.infer(value, env, concrete_types)
                body_type = self.infer(
                    body, env.copy() | {name: value_type}, concrete_types.copy()
                )
                return body_type
            case Letrec(name=name, value=value, body=body):
                new_type = TypeVariable()
                E = env.copy() | {name: new_type}
                C = concrete_types.copy() | {new_type}
                value_type = self.infer(value, E, C)
                self.unify_type_expressions(new_type, value_type)
                return self.infer(body, E, C)
            case Match(expr=expr, cases=cases):
                expr_type = TypeVariable()
                concrete_types.add(expr_type)
                inferred_expr_type = self.infer(expr, env, concrete_types)

                self.unify_type_expressions(expr_type, inferred_expr_type)
                match_result_type = TypeVariable()
                for case in cases:
                    pattern_type = self.infer(case.pattern, env, concrete_types)
                    self.unify_type_expressions(
                        pattern_type, Function(expr_type, BoolType())
                    )
                    body_type = self.infer(case.body, env, concrete_types)
                    self.unify_type_expressions(match_result_type, body_type)
                return match_result_type
            case VariantDecl(name=name, type_name_pairs=type_name_pairs):
                type_var = TypeOperator(name)
                for ctor, type_expr in type_name_pairs:
                    env[ctor] = Function(type_expr, type_var)
                return type_var
            case _:
                assert False

    @logwrap
    def typeof(self, name: str, env: Env, concrete_types: set[TypeExpr]) -> TypeExpr:
        if val := env.get(name, None):
            return self.deep_copy_with_new_generic_typevars(val, concrete_types)
        raise TypeCheckError(f"Undefined symbol: {name}")

    @logwrap
    def deep_copy_with_new_generic_typevars(
        self, type_variable: TypeExpr, concrete_type_vars: set[TypeExpr]
    ) -> TypeExpr:
        mapping: dict[TypeExpr, TypeExpr] = {}

        def recursive_copy(type_expr: TypeExpr) -> TypeExpr:
            type_expr = self.prune(type_expr)
            match type_expr:
                case TypeVariable():
                    if self.is_generic(type_expr, concrete_type_vars):
                        if type_expr not in mapping:
                            mapping[type_expr] = TypeVariable()
                        return mapping[type_expr]
                    else:
                        return type_expr
                case TypeOperator():
                    copied_subtypes = [recursive_copy(t) for t in type_expr.types]
                    return TypeOperator(type_expr.name, *copied_subtypes)

        return recursive_copy(type_variable)

    @logwrap
    def prune_type_expression(self, type_expression: TypeVariable) -> TypeExpr:
        if type_expression.instantiated():
            assert type_expression.instance
            ret = type_expression.instance = self.prune(type_expression.instance)
            return ret
        return type_expression

    @logwrap
    def prune(self, type_expression: TypeExpr) -> TypeExpr:
        """
                The function Prune is used whenever a type expression has to be inspected: it will always
        return a type expression which is either an uninstantiated type variable or a type operator; i.e. it
        will skip instantiated variables, and will actually prune them from expressions to remove long
        chains of instantiated variables.
        """
        match type_expression:
            case TypeVariable():
                if type_expression.instantiated():
                    return self.prune_type_expression(type_expression)
                return type_expression
            case TypeOperator():
                return type_expression

    @logwrap
    def is_concrete(self, expr: TypeExpr, concrete_type_exprs: set[TypeExpr]) -> bool:
        return self.is_sub_type_expression_of_any(expr, concrete_type_exprs)

    @logwrap
    def is_generic(self, expr: TypeExpr, concrete_type_exprs: set[TypeExpr]) -> bool:
        return not self.is_sub_type_expression_of_any(expr, concrete_type_exprs)

    @logwrap
    def is_sub_type_expression_of_any(
        self, maybe_subexpr: TypeExpr, expr_iterable: Iterable[TypeExpr]
    ) -> bool:
        return any(
            self.is_sub_type_expression_of(maybe_subexpr, member_type)
            for member_type in expr_iterable
        )

    @logwrap
    def is_sub_type_expression_of(
        self, maybe_subexpr: TypeExpr, expr: TypeExpr
    ) -> bool:
        expr = self.prune(expr)
        match expr:
            case TypeVariable():
                return expr == maybe_subexpr
            case TypeOperator():
                return self.is_sub_type_expression_of_any(maybe_subexpr, expr.types)

    @logwrap
    def unify_type_expressions(self, expr1: TypeExpr, expr2: TypeExpr) -> None:
        expr1, expr2 = self.prune(expr1), self.prune(expr2)
        match expr1:
            case TypeVariable():
                if expr1 != expr2:
                    if self.is_sub_type_expression_of(expr1, expr2):
                        raise TypeCheckError(
                            f"Recursive unification when trying to unify these types: {expr1} {expr2}"
                        )
                    expr1.assign(expr2)

            case TypeOperator(name=n1, types=ts1):
                match expr2:
                    case TypeVariable():
                        self.unify_type_expressions(expr2, expr1)
                    case TypeOperator(name=n2, types=ts2):
                        log(n1, ts1)
                        log(n2, ts2)
                        if n1 != n2 or len(ts1) != len(ts2):
                            raise TypeCheckError(
                                f"Could not unify types: {expr1} {expr2}"
                            )
                        for subtype1, subtype2 in zip(ts1, ts2):
                            self.unify_type_expressions(subtype1, subtype2)
