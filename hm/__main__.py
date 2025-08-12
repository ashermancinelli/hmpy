#!/usr/bin/env python
# RUN: python %s | FileCheck %s
from hm.test import default_environment
from hm.type_expr import TypeOperator, TypeVariable, Function
from hm.unify import BoolType, Env, IntType, NoneType, TypeCheck
from hm.util import TypeCheckError
from hm.hm_ast import (
    AST,
    Identifier,
    IntLit,
    Lambda,
    BoolLit,
    Let,
    Letrec,
    Match,
    Case,
)


def typevars(how_many: int):
    return TypeVariable.make(how_many)


def test(base_env: Env, ast: AST | list[AST]):
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
    Letrec(f, Lambda(n, ite(is_zero(n), lit1, times(n, f(minus(n, lit1))))), f(lit5)),
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
    Let(f, Lambda(x, Match(x, Case(eq(lit1), lit1), Case(eq(lit2), lit2))), f(lit1)),
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
