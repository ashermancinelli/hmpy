#!/usr/bin/env python
from typing import Iterable, Union

from dsl import (
    AST,
    Apply,
    BoolLit,
    Case,
    Identifier,
    IntLit,
    Lambda,
    Let,
    Letrec,
    Match,
    VariantDecl,
)
from util import TypeCheckError, log, logwrap

from type_expr import TypeExpr, TypeVariable, TypeOperator, Function


IntType = TypeOperator("int")
BoolType = TypeOperator("bool")
NoneType = TypeOperator("None")
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
                return IntType
            case BoolLit():
                return BoolType
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
                        pattern_type, Function(expr_type, BoolType)
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
            case _:
                print(expr1, expr2)
                assert False


def typevars(how_many: int):
    return [TypeVariable() for _ in range(how_many)]


def test(base_env, ast: AST | list[AST]):
    match ast:
        case list():
            for a in ast:
                test(base_env, a)
        case _:
            print()
            tc = TypeCheck()
            print(ast)
            try:
                result = tc.infer(ast, base_env, set())
                print("Typed result:", result)
            except TypeCheckError as e:
                print("Error:", e)


PairType = TypeOperator("*", *typevars(2))


t1 = TypeVariable()
default_environment: Env = {
    "true": BoolType,
    "false": BoolType,
    "*": Function(IntType, Function(IntType, IntType)),
    "+": Function(IntType, Function(IntType, IntType)),
    "-": Function(IntType, Function(IntType, IntType)),
    "pair": Function(PairType[0], Function(PairType[1], PairType)),
    "ite": Function(BoolType, Function(t1, Function(t1, t1))),
    "is_zero": Function(IntType, BoolType),
    "None": NoneType,
    "=": Function(IntType, Function(IntType, BoolType)),
    "!=": Function(IntType, Function(IntType, BoolType)),
    "<": Function(IntType, Function(IntType, BoolType)),
    "<=": Function(IntType, Function(IntType, BoolType)),
    ">": Function(IntType, Function(IntType, BoolType)),
    ">=": Function(IntType, Function(IntType, BoolType)),
}

for length in range(3, 10):
    name = f"tuple{length}"
    ty = TypeOperator("*", *typevars(length))
    default_environment[name] = Function.make(ty.types, ty)

eq, ne, lt, le, gt, ge = Identifier.make("=", "!=", "<", "<=", ">", ">=")
times, minus, plus, ite, pair, is_zero, none = Identifier.make(
    "*", "-", "+", "ite", "pair", "is_zero", "None"
)
x, y, z = Identifier.make("x", "y", "z")
tuple3, tuple4, tuple5, tuple6, tuple7, tuple8, tuple9 = [
    Identifier(f"tuple{i}") for i in range(3, 10)
]
lit0, lit1, lit2, lit3, lit4, lit5 = (
    IntLit(0),
    IntLit(1),
    IntLit(2),
    IntLit(3),
    IntLit(4),
    IntLit(5),
)
true, false = BoolLit(True), BoolLit(False)
f, g, h, m, n = Identifier.make("f", "g", "h", "m", "n")
incr, decr = plus(x, lit1), minus(x, lit1)
A, B, C = Identifier.make("A", "B", "C")
identity = Identifier("identity")

if __name__ == "__main__":
    tests: list[AST] = [
        Lambda(x.name, pair(x(lit5), x(true))),
        # CHECK: Error: Could not unify types: bool int
        Lambda(x.name, pair(x(lit5), x(lit3))),
        # CHECK: Typed result:
        # CHECK-SAME: ((int -> [[T1:T[0-9]+]])
        # CHECK-SAME: -> ([[T1]] * [[T1]]))
        Lambda(x.name, pair(x(true), x(false))),
        # CHECK: Typed result:
        # CHECK-SAME: ((bool -> [[T1:T[0-9]+]])
        # CHECK-SAME: -> ([[T1]] * [[T1]]))
        Lambda(x, Lambda(y, tuple3(x(lit5), x(lit3), y(true)))),
        # CHECK: Typed result:
        # CHECK-SAME: ((int -> [[A:T[0-9]+]])
        # CHECK-SAME: -> ((bool -> [[B:T[0-9]+]])
        # CHECK-SAME: -> ([[A]] * [[A]] * [[B]])))
        Lambda(f, Lambda(g, Lambda(h, f(g(h))))),
        # Transitivity/Function Composition
        # CHECK: Typed result:
        # CHECK-SAME: (([[B:T[0-9]+]] -> [[C:T[0-9]+]])
        # CHECK-SAME: -> (([[A:T[0-9]+]] -> [[B]]) -> ([[A]] -> [[C]])))
        Let(f, Lambda(g, g), pair(f(lit5), f(true))),
        # CHECK: Typed result:
        # CHECK-SAME: (int * bool)
        # Factorial
        Letrec(
            f, Lambda(n, ite(is_zero(n), lit1, times(n, f(minus(n, lit1))))), f(lit5)
        ),
        # CHECK: Typed result:
        # CHECK-SAME: int
        Lambda(x, A(x)),
        # CHECK: Typed result: (None -> l)
        Lambda(x, B(x)),
        # CHECK: Typed result: (int -> l)
        Lambda(y, Lambda(x, C(pair(x, y)))),
        # CHECK: Typed result: (int -> (int -> l))
        Match(
            lit1,
            Case(eq(lit1), lit1),
            Case(eq(lit2), lit2),
            Case(Lambda(none, true), lit3),
        ),
        # CHECK: Typed result: int
        Match(
            lit1,
            Case(eq(lit1), lit1),
            Case(eq(lit2), lit2),
            Case(Lambda(none, true), true),
        ),
        # CHECK: Error: Could not unify types: int bool
        Let(
            f, Lambda(x, Match(x, Case(eq(lit1), lit1), Case(eq(lit2), lit2))), f(lit1)
        ),
        # CHECK: Typed result: int
        Letrec(
            f,
            Lambda(
                x,
                Match(x, Case(gt(lit5), lit1), Case(Lambda(x, true), f(plus(x, lit1)))),
            ),
            f(lit1),
        ),
        # CHECK: Typed result: int
        Let(identity, Lambda(x, x), identity(lit1)),
        # CHECK: Typed result: int
    ]
    test(default_environment, tests)
