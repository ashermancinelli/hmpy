

from typing import Union

from util import logwrap


TypeExpr = Union["TypeOperator", "TypeVariable"]


class TypeVariable:
    _id: int = 0

    def __init__(self):
        self._id = TypeVariable._id
        TypeVariable._id += 1
        self.instance: TypeExpr | None = None
        self._name: str | None = None

    def id(self) -> int:
        return self._id

    def instantiated(self) -> bool:
        return self.instance is not None

    @logwrap
    def assign(self, type_expr: TypeExpr) -> None:
        self.instance = type_expr

    def name(self):
        if self._name is None:
            self._name = f"T{self.id()}"
        return self._name

    def __str__(self):
        if self.instance is not None:
            return str(self.instance)
        return self.name()

    def __repr__(self):
        return str(self)


class TypeOperator:
    def __init__(self, name, *types: TypeExpr):
        self.name: str = name
        self.types: list[TypeExpr] = list(types)

    def __str__(self):
        match self.types:
            case []:
                return self.name
            case [l, r]:
                return f"({l} {self.name} {r})"
            case _:
                sep = f" {self.name} "
                return "(" + sep.join(map(str, self.types)) + ")"

    def __repr__(self):
        return str(self)

    def __getitem__(self, index: int):
        return self.types[index]


class Function(TypeOperator):
    def __init__(self, from_type: TypeExpr, to_type: TypeExpr):
        super().__init__("->", from_type, to_type)

    @staticmethod
    def make(arg_types: list[TypeExpr], ret_type: TypeExpr) -> TypeExpr:
        function: TypeExpr = ret_type
        for arg in reversed(arg_types):
            function = Function(arg, function)
        return function
