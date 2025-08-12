# RUN: python3 %s | FileCheck %s
from harness import *

typecheck(
    [
        Lambda(x.name, pair(x(lit5), x(true))),
        # CHECK: Error: Could not unify types: bool int
    ]
)

typecheck(
    [
        Match(
            lit1,
            Case(eq(lit1), lit1),
            Case(eq(lit2), lit2),
            Case(Lambda(none, true), true),
        ),
        # CHECK: Error: Could not unify types: int bool
    ]
)

typecheck(
    [
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
    ]
)
