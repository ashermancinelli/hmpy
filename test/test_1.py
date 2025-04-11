# RUN: python3 %s | FileCheck %s
from harness import *

typecheck([
    VariantDecl("l", [
        ('A', NoneType),
        ('B', IntType),
        ('C', TypeOperator('*', IntType, IntType)),
    ]),
    # CHECK: Type: l
    Lambda(x, C(pair(x(lit5), x(lit3)))),
    # CHECK: ((int -> int) -> l)
    Lambda(x, B(x(lit5))),
    # CHECK: ((int -> int) -> l)
])
