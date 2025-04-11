# RUN: python3 %s | FileCheck %s
from harness import *

typecheck(
    [
        Match(
            lit1,
            Case(eq(lit1), lit1),
            Case(eq(lit2), lit2),
            Case(Lambda(none, true), lit3),
        ),
        # CHECK: Typed result: int
    ]
)
