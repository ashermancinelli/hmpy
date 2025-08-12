
from hm.unify import BoolType, Env, IntType, Function


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
