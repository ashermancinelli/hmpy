# RUN: python3 %s | FileCheck %s
from hm.test.harness import *

isa = Identifier("isa")
l = Identifier("l")
typecheck(
    [
        VariantDecl(
            "l",
            [
                ("A", NoneType()),
                ("B", IntType()),
                ("C", TypeOperator("*", IntType(), IntType())),
            ],
        ),
        # CHECK: Typed result: l
        Lambda(x, C(pair(x(lit5), x(lit3)))),
        # CHECK: Typed result: ((int -> int) -> l)
        Lambda(x, B(x(lit5))),
        # CHECK: Typed result: ((int -> int) -> l)
        Lambda(x, A(x)),
        # CHECK: Typed result: (none -> l)
        Lambda(x, B(x)),
        # CHECK: Typed result: (int -> l)
        Lambda(y, Lambda(x, C(pair(x, y)))),
        # CHECK: Typed result: (int -> (int -> l))
        Lambda(
            y,
            Match(
                B(y),
                Case(isa, lit5),
                Case(Lambda(x, true), lit0),
            ),
        ),
        # CHECK: Typed result: (int -> int)
    ],
    {
        "isa": Function(TypeVariable(), BoolType()),
    },
)
