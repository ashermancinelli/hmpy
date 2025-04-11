
class AST:
    def __init__(self):
        self.type = None

class Apply(AST): ...

class Identifier(AST):
    def __init__(self, name):
        self.name = name

    def __call__(self, *args: AST):
        app = Apply(self, args[0])
        for arg in args[1:]:
            app = Apply(app, arg)
        return app

    @staticmethod
    def make(*args):
        return map(Identifier, args)

    def __str__(self):
        return self.name


class IntLit(AST):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)


class BoolLit(AST):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)


class BinOp(AST):
    def __init__(self, op, lhs, rhs):
        self.op, self.lhs, self.rhs = op, lhs, rhs

    def __str__(self):
        return f"{self.lhs} {self.op} {self.rhs}"


class Let(AST):
    def __init__(self, name, value, body):
        self.name = name.name if isinstance(name, Identifier) else name
        self.value, self.body = value, body

    def __str__(self):
        return f"let {self.name} = {self.value} in {self.body}"


class Letrec(AST):
    def __init__(self, name, value, body):
        self.name = name.name if isinstance(name, Identifier) else name
        self.value, self.body = value, body

    def __str__(self):
        return f"(letrec {self.name} = {self.value} in {self.body})"

class Lambda(AST):
    def __init__(self, param, body):
        self.param = param.name if isinstance(param, Identifier) else param
        self.body = body

    def __str__(self):
        return f"(λ{self.param} . {self.body})"

    def __call__(self, *args: AST):
        app = Apply(self, args[0])
        for arg in args[1:]:
            app = Apply(app, arg)
        return app


class Apply(AST):
    def __init__(self, fn, arg):
        self.fn, self.arg = fn, arg

    def __str__(self):
        return f"({self.fn} {self.arg})"

class Match(AST):
    def __init__(self, expr, *cases):
        self.expr, self.cases = expr, cases

    def __str__(self):
        cases = ' | '.join(f'{case}' for case in self.cases)
        return f"match {self.expr} with {cases}"

class Case(AST):
    def __init__(self, pattern, body):
        self.pattern, self.body = pattern, body

    def __str__(self):
        return f"{self.pattern} => {self.body}"

class WildcardCase(Case):
    def __init__(self, body):
        super().__init__(Lambda(Identifier("x"), BoolLit(True)), body)

    def __str__(self):
        return f"_ => {self.body}"

class VariantDecl(AST):
    def __init__(self, name, type_name_pairs):
        self.name, self.type_name_pairs = name, type_name_pairs

    def __str__(self):
        return f"type {self.name} = {' | '.join(f'{t} of {n}' for t, n in self.type_name_pairs)}"
