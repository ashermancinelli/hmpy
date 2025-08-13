import os
import sys

from hm.test import *
from hm.hm_ast import *
from hm.type_expr import TypeOperator, TypeExpr, TypeVariable
from hm.unify import TypeCheck, Env, TypeCheckError


def typecheck(asts: list[AST], env: dict[str, TypeExpr] = dict()):
    typecheck = TypeCheck()
    result = None
    env = default_environment.copy() | env
    for ast in asts:
        print(ast)
        try:
            result = typecheck.infer(ast, env, set())
            print("Typed result: ", result)
        except TypeCheckError as e:
            print("Error: ", e)
