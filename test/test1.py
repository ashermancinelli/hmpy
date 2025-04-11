# RUN: python3 %s | FileCheck %s
from harness import *

typecheck(
    [
        VariantDecl(
            "l",
            [
                ("A", NoneType),
                ("B", IntType),
                ("C", TypeOperator("*", IntType, IntType)),
            ],
        ),
        # CHECK: Typed result: l
        Lambda(x, C(pair(x(lit5), x(lit3)))),
        # CHECK: Typed result: ((int -> int) -> l)
        Lambda(x, B(x(lit5))),
        # CHECK: Typed result: ((int -> int) -> l)
        Lambda(x, A(x)),
        # CHECK: Typed result: (None -> l)
        Lambda(x, B(x)),
        # CHECK: Typed result: (int -> l)
        Lambda(y, Lambda(x, C(pair(x, y)))),
        # CHECK: Typed result: (int -> (int -> l))
    ]
)
