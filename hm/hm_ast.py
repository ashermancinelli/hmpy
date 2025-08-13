from ast import TypeAlias
from dataclasses import dataclass
import functools
from typing import Any
from hm.type_expr import TypeExpr, TypeOperator


class AST:
    def __init__(self):
        self.type: TypeExpr | None = None


class Apply(AST):
    def __init__(self, fn: AST, arg: AST):
        super().__init__()
        self.fn: AST = fn
        self.arg: AST = arg

    def __str__(self):
        return f"({self.fn} {self.arg})"


class Identifier(AST):
    def __init__(self, name: str):
        super().__init__()
        self.name: str = name

    def __call__(self, *args: AST):
        app = Apply(self, args[0])
        for arg in args[1:]:
            app = Apply(app, arg)
        return app

    @staticmethod
    def make(*args: str):
        return map(Identifier, args)

    def __str__(self):
        return self.name


class IntLit(AST):
    def __init__(self, value: int):
        super().__init__()
        self.value: int = value

    def __str__(self):
        return str(self.value)


class BoolLit(AST):
    def __init__(self, value: bool):
        super().__init__()
        self.value: bool = value

    def __str__(self):
        return str(self.value)


@dataclass
class BinOp(AST):
    op: str
    lhs: AST
    rhs: AST

    def __str__(self):
        return f"({self.lhs} {self.op} {self.rhs})"


Add = functools.partial(BinOp, "+")
Sub = functools.partial(BinOp, "-")
Div = functools.partial(BinOp, "/")
Mul = functools.partial(BinOp, "*")


class Let(AST):
    def __init__(self, name: str | Identifier, value: AST, body: AST):
        super().__init__()
        self.name: str = name.name if isinstance(name, Identifier) else name
        self.value: AST = value
        self.body: AST = body

    def __str__(self):
        return f"let {self.name} = {self.value} in {self.body}"


class Letrec(AST):
    def __init__(self, name: str | Identifier, value: AST, body: AST):
        super().__init__()
        self.name: str = name.name if isinstance(name, Identifier) else name
        self.value: AST = value
        self.body: AST = body

    def __str__(self):
        return f"(letrec {self.name} = {self.value} in {self.body})"


class Lambda(AST):
    def __init__(self, param: Identifier | str, body: AST):
        super().__init__()
        self.param: str = param.name if isinstance(param, Identifier) else param
        self.body: AST = body

    def __str__(self):
        return f"(λ{self.param} . {self.body})"

    def __call__(self, *args: AST):
        app = Apply(self, args[0])
        for arg in args[1:]:
            app = Apply(app, arg)
        return app


class Case(AST):
    def __init__(self, pattern: AST, body: AST):
        super().__init__()
        self.pattern: AST = pattern
        self.body: AST = body

    def __str__(self):
        return f"{self.pattern} => {self.body}"


class Match(AST):
    def __init__(self, expr: AST, *cases: Case):
        super().__init__()
        self.expr: AST = expr
        self.cases: list[Case] = list(cases)

    def __str__(self):
        cases = " | ".join(f"{case}" for case in self.cases)
        return f"match {self.expr} with {cases}"


class WildcardCase(Case):
    def __init__(self, body: AST):
        super().__init__(Lambda(Identifier("x"), BoolLit(True)), body)

    def __str__(self):
        return f"_ => {self.body}"


class VariantDecl(AST):
    def __init__(self, name: str, type_name_pairs: list[tuple[str, TypeExpr]]):
        super().__init__()
        self.name: str = name
        self.type_name_pairs: list[tuple[str, TypeExpr]] = type_name_pairs

    def __str__(self):
        return f"type {self.name} = {' | '.join(f'{t} of {n}' for t, n in self.type_name_pairs)}"


@dataclass
class VariantCase(AST):
    name: str
    value: "TypeRHS"


@dataclass
class Variant(AST):
    variants: tuple[VariantCase]


class IntType(TypeOperator):
    def __init__(self):
        super().__init__("int")

class BoolType(TypeOperator):
    def __init__(self):
        super().__init__("bool")

class FloatType(TypeOperator):
    def __init__(self):
        super().__init__("float")

class NoneType(TypeOperator):
    def __init__(self):
        super().__init__("none")

ScalarType = BoolType | IntType | FloatType | NoneType
TypeRHS = Variant | ScalarType


@dataclass
class Type(AST):
    name: str
    value: TypeRHS
