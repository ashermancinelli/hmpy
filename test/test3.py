# RUN: python3 %s | FileCheck %s
from harness import *

typecheck(
    [
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
    ]
)
