import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from ast import *
from hm import *


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
