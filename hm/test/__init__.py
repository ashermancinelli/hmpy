from hm.hm_ast import (
    BoolLit,
    Identifier,
    IntLit,
    IntType,
    BoolType,
    FloatType,
    NoneType,
)
from hm.type_expr import TypeOperator, TypeVariable, Function
from hm.unify import Env

PairType = TypeOperator("*", *TypeVariable.make(2))

t1 = TypeVariable()

default_environment: Env = {
    "true": BoolType(),
    "false": BoolType(),
    "*": Function(IntType(), Function(IntType(), IntType())),
    "+": Function(IntType(), Function(IntType(), IntType())),
    "-": Function(IntType(), Function(IntType(), IntType())),
    "pair": Function(PairType[0], Function(PairType[1], PairType)),
    "ite": Function(BoolType(), Function(t1, Function(t1, t1))),
    "is_zero": Function(IntType(), BoolType()),
    "None": NoneType(),
    "=": Function(IntType(), Function(IntType(), BoolType())),
    "!=": Function(IntType(), Function(IntType(), BoolType())),
    "<": Function(IntType(), Function(IntType(), BoolType())),
    "<=": Function(IntType(), Function(IntType(), BoolType())),
    ">": Function(IntType(), Function(IntType(), BoolType())),
    ">=": Function(IntType(), Function(IntType(), BoolType())),
}

for length in range(3, 10):
    name = f"tuple{length}"
    ty = TypeOperator("*", *TypeVariable.make(length))
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
