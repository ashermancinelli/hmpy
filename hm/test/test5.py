# RUN: python3 %s | FileCheck %s
from hm.test.harness import *

typecheck(
    [
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
)
