# RUN: python %s | FileCheck %s

"""

match 1 with (= 1) => 1 | (= 2) => 2 | (λNone . True) => 0

match T1 with (T1 -> bool) -> T2 | (T1 -> bool) -> T3

"""

import functools

DEBUG = False


def log(*a, **kw):
    pass

class TypeCheckError(Exception):
    def __init__(self, message):
        self.message = message

    def __str__(self):
        return self.message


if DEBUG:

    def log(*a, **kw):
        print(*a, **kw)


def logwrap(func):
    """
    A decorator that logs the function name, arguments, and return value.
    Only active when DEBUG is True.
    """

    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        if not DEBUG:
            return func(*args, **kwargs)

        args_strs = [str(a) for a in args]
        kwargs_repr = [f"{k}={v}" for k, v in kwargs.items()]
        signature = ", ".join(args_strs + kwargs_repr)

        print(f"-- {func.__name__}({signature})")
        result = func(*args, **kwargs)
        print(f"{func.__name__} -> {result}")

        return result

    return wrapper


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
        super().__init__(Lambda(x, true), body)

    def __str__(self):
        return f"_ => {self.body}"

#type l = A | B of int | C of int * int;;
class VariantDecl(AST):
    def __init__(self, name, type_name_pairs):
        self.name, self.type_name_pairs = name, type_name_pairs

    def __str__(self):
        return f"{self.name} = {', '.join(f'{t} = {n}' for t, n in self.type_name_pairs)}"

class TypeOperator: ...


class TypeVariable: ...


TypeExpr = TypeOperator | TypeVariable


class TypeVariable:
    _id = 0

    def __init__(self):
        self._id = TypeVariable._id
        TypeVariable._id += 1
        self.instance = None
        self._name = None

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
    def __init__(self, name, *types: list[TypeExpr]):
        self.name = name
        self.types = types

    def __str__(self):
        match self.types:
            case []:
                return self.name
            case [l, r]:
                return f"({l} {self.name} {r})"
            case _:
                sep = f' {self.name} '
                return '(' + sep.join(map(str, self.types)) + ')'

    def __repr__(self):
        return str(self)

    def __getitem__(self, index):
        return self.types[index]


class Function(TypeOperator):
    def __init__(self, from_type, to_type):
        super().__init__("->", from_type, to_type)

    @staticmethod
    def make(arg_types, ret_type):
        F = ret_type
        for arg in reversed(arg_types):
            F = Function(arg, F)
        return F


IntType = TypeOperator("int")
BoolType = TypeOperator("bool")
NoneType = TypeOperator('None')


class TypeCheck:
    def __init__(self):
        # self.environment: dict[str, TypeExpr] = environment
        pass

    def __str__(self):
        return "TypeCheck()"

    @logwrap
    def infer(self, ast, env, concrete_types: list[TypeExpr]) -> TypeVariable:
        inferred_type = self._infer(ast, env, concrete_types)
        ast.type = inferred_type
        return inferred_type

    @logwrap
    def _infer(self, ast, env, concrete_types: list[TypeExpr]) -> TypeVariable:
        match ast:
            case Identifier(name=name):
                if name.isnumeric():
                    return self.infer(IntLit(int(name)), env, concrete_types)
                return self.typeof(name, env, concrete_types)
            case IntLit():
                return IntType
            case BoolLit():
                return BoolType
            case Apply(fn=fn, arg=arg):
                fun_type = self.infer(fn, env, concrete_types)
                arg_type = self.infer(arg, env, concrete_types)
                result_type = TypeVariable()
                self.unify_type_expressions(Function(arg_type, result_type), fun_type)
                return result_type
            case Lambda(param=param, body=body):
                arg_type = TypeVariable()
                result_type = self.infer(body, env.copy() | {param: arg_type}, concrete_types.copy() | {arg_type})
                return Function(arg_type, result_type)
            case Let(name=name, value=value, body=body):
                value_type = self.infer(value, env, concrete_types)
                body_type = self.infer(body, env.copy() | {name: value_type}, concrete_types.copy())
                return body_type
            case Letrec(name=name, value=value, body=body):
                new_type = TypeVariable()
                E = env.copy() | {name: new_type}
                C = concrete_types.copy() | {new_type}
                value_type = self.infer(value, E, C)
                self.unify_type_expressions(new_type, value_type)
                return self.infer(body, E, C)
            case Match(expr=expr, cases=cases):
                expr_type = TypeVariable()
                concrete_types.add(expr_type)
                inferred_expr_type = self.infer(expr, env, concrete_types)

                self.unify_type_expressions(expr_type, inferred_expr_type)
                match_result_type = TypeVariable()
                for case in cases:
                    # All patterns are T -> bool
                    pattern_type = self.infer(case.pattern, env, concrete_types)
                    self.unify_type_expressions(pattern_type, Function(expr_type, BoolType))
                    body_type = self.infer(case.body, env, concrete_types)
                    self.unify_type_expressions(match_result_type, body_type)
                return match_result_type
            case _:
                assert False

    @logwrap
    def typeof(self, name: str, env, concrete_types: set[TypeExpr]) -> TypeVariable:
        if val := env.get(name, None):
            return self.deep_copy_with_new_generic_typevars(
                val, concrete_types
            )
        raise TypeCheckError(f"Undefined symbol: {name}")

    @logwrap
    def deep_copy_with_new_generic_typevars(
        self, type_variable: TypeExpr, concrete_type_vars: list[TypeExpr]
    ) -> TypeExpr:
        mapping: dict[TypeExpr] = {}

        def recursive_copy(type_expr: TypeExpr) -> TypeExpr:
            type_expr = self.prune(type_expr)
            match type_expr:
                case TypeVariable():
                    if self.is_generic(type_expr, concrete_type_vars):
                        if type_expr not in mapping:
                            mapping[type_expr] = TypeVariable()
                        return mapping[type_expr]
                    else:
                        return type_expr
                case TypeOperator():
                    copied_subtypes = [recursive_copy(t) for t in type_expr.types]
                    return TypeOperator(type_expr.name, *copied_subtypes)

        return recursive_copy(type_variable)

    @logwrap
    def prune_type_expression(self, type_expression: TypeExpr) -> TypeExpr:
        type_expression.instance = self.prune(type_expression.instance)
        return type_expression.instance

    @logwrap
    def prune(self, type_expression: TypeExpr) -> TypeExpr:
        """
                The function Prune is used whenever a type expression has to be inspected: it will always
        return a type expression which is either an uninstantiated type variable or a type operator; i.e. it
        will skip instantiated variables, and will actually prune them from expressions to remove long
        chains of instantiated variables.
        """
        match type_expression:
            case TypeVariable():
                if type_expression.instantiated():
                    return self.prune_type_expression(type_expression)
                return type_expression
            case TypeOperator():
                return type_expression

    @logwrap
    def is_concrete(self, expr: TypeExpr, concrete_type_exprs: list[TypeExpr]) -> bool:
        return self.is_sub_type_expression_of_any(expr, concrete_type_exprs)

    @logwrap
    def is_generic(self, expr: TypeExpr, concrete_type_exprs: list[TypeExpr]) -> bool:
        return not self.is_sub_type_expression_of_any(expr, concrete_type_exprs)

    @logwrap
    def is_sub_type_expression_of_any(
        self, maybe_subexpr: TypeExpr, expr_iterable: list[TypeExpr]
    ) -> bool:
        return any(
            self.is_sub_type_expression_of(maybe_subexpr, member_type)
            for member_type in expr_iterable
        )

    @logwrap
    def is_sub_type_expression_of(
        self, maybe_subexpr: TypeExpr, expr: TypeExpr
    ) -> bool:
        expr = self.prune(expr)
        match expr:
            case TypeVariable():
                return expr == maybe_subexpr
            case TypeOperator():
                return self.is_sub_type_expression_of_any(maybe_subexpr, expr.types)

    @logwrap
    def unify_type_expressions(self, expr1: TypeExpr, expr2: TypeExpr) -> None:
        expr1, expr2 = self.prune(expr1), self.prune(expr2)
        match expr1:
            case TypeVariable():
                if expr1 != expr2:
                    if self.is_sub_type_expression_of(expr1, expr2):
                        raise TypeCheckError(
                            f"Recursive unification when trying to unify these types: {expr1} {expr2}"
                        )
                    expr1.assign(expr2)

            case TypeOperator(name=n1, types=ts1):
                match expr2:
                    case TypeVariable():
                        self.unify_type_expressions(expr2, expr1)
                    case TypeOperator(name=n2, types=ts2):
                        log(n1, ts1)
                        log(n2, ts2)
                        if n1 != n2 or len(ts1) != len(ts2):
                            raise TypeCheckError(f"Could not unify types: {expr1} {expr2}")
                        for subtype1, subtype2 in zip(ts1, ts2):
                            self.unify_type_expressions(subtype1, subtype2)
            case _:
                print(expr1, expr2)
                assert False


def typevars(how_many: int):
    return [TypeVariable() for _ in range(how_many)]


def test(base_env, ast: AST | list[AST]):
    match ast:
        case list():
            for a in ast:
                test(base_env, a)
        case _:
            print()
            tc = TypeCheck()
            print(ast)
            try:
                result = tc.infer(ast, base_env, set())
                print('Typed result:', result)
            except TypeCheckError as e:
                print('Error:', e)


if __name__ == "__main__":
    PairType = TypeOperator("*", *typevars(2))
    t1 = TypeVariable()
    environment = {
        "true": BoolType,
        "false": BoolType,
        "*": Function(IntType, Function(IntType, IntType)),
        "+": Function(IntType, Function(IntType, IntType)),
        "-": Function(IntType, Function(IntType, IntType)),
        "pair": Function(PairType[0], Function(PairType[1], PairType)),
        "ite": Function(BoolType, Function(t1, Function(t1, t1))),
        "is_zero": Function(IntType, BoolType),
        "None": NoneType,
        "=": Function(IntType, Function(IntType, BoolType)),
        "!=": Function(IntType, Function(IntType, BoolType)),
        "<": Function(IntType, Function(IntType, BoolType)),
        "<=": Function(IntType, Function(IntType, BoolType)),
        ">": Function(IntType, Function(IntType, BoolType)),
        ">=": Function(IntType, Function(IntType, BoolType)),
    }

    for length in range(3, 10):
        name = f'tuple{length}'
        ty = TypeOperator("*", *typevars(length))
        environment[name] = Function.make(ty.types, ty)

    eq, ne, lt, le, gt, ge = Identifier.make("=", "!=", "<", "<=", ">", ">=")
    times, minus, plus, ite, pair, is_zero, none = Identifier.make("*", "-", "+", "ite", "pair", "is_zero", "None")
    x, y, z = Identifier.make("x", "y", "z")
    tuple3, tuple4, tuple5, tuple6, tuple7, tuple8, tuple9 = [Identifier(f'tuple{i}') for i in range(3, 10)]
    lit0, lit1, lit2, lit3, lit4, lit5 = IntLit(0), IntLit(1), IntLit(2), IntLit(3), IntLit(4), IntLit(5)
    true, false = BoolLit(True), BoolLit(False)
    f, g, h, m, n = Identifier.make("f", "g", "h", "m", "n")
    incr, decr = plus(x, lit1), minus(x, lit1)

    A, B, C = Identifier.make("A", "B", "C")
    identity = Identifier("identity")

    l = VariantDecl("l", [
        ('A', NoneType),
        ('B', IntType),
        ('C', TypeOperator("*", IntType, IntType)),
    ])

    def sema_variant_decl(vd: VariantDecl):
        type_var = TypeOperator(vd.name)
        for ctor, type_expr in vd.type_name_pairs:
            ctor_type = Function(type_expr, type_var)
            environment[ctor] = ctor_type

    sema_variant_decl(l)

    tests = [
        Lambda(x.name, pair(x(lit5), x(true))),
        # CHECK: Error: Could not unify types: bool int

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

        Lambda(f, Lambda(g, Lambda(h, f(g(h))))),
        # Transitivity/Function Composition
        # CHECK: Typed result:
        # CHECK-SAME: (([[B:T[0-9]+]] -> [[C:T[0-9]+]])
        # CHECK-SAME: -> (([[A:T[0-9]+]] -> [[B]]) -> ([[A]] -> [[C]])))

        Let(f, Lambda(g, g), pair(f(lit5), f(true))),
        # CHECK: Typed result:
        # CHECK-SAME: (int * bool)
    
        # Factorial
        Letrec(f, Lambda(n, ite(is_zero(n), lit1, times(n, f(minus(n, lit1))))), f(lit5)),
        # CHECK: Typed result:
        # CHECK-SAME: int

        Lambda(x, A(x)),
        # CHECK: Typed result: (None -> l)

        Lambda(x, B(x)),
        # CHECK: Typed result: (int -> l)

        Lambda(y, Lambda(x, C(pair(x, y)))),
        # CHECK: Typed result: (int -> (int -> l))

        Match(lit1, Case(eq(lit1), lit1), Case(eq(lit2), lit2), Case(Lambda(none, true), lit3)),
        # CHECK: Typed result: int

        Match(lit1, Case(eq(lit1), lit1), Case(eq(lit2), lit2), Case(Lambda(none, true), true)),
        # CHECK: Error: Could not unify types: int bool

        Let(f, Lambda(x, Match(x, Case(eq(lit1), lit1), Case(eq(lit2), lit2))), f(lit1)),
        # CHECK: Typed result: int

        Letrec(f, Lambda(x, Match(x, Case(gt(lit5), lit1), Case(Lambda(x, true), f(plus(x, lit1))))), f(lit1)),
        # CHECK: Typed result: int

        Let(identity, Lambda(x, x), identity(lit1)),
        # CHECK: Typed result: int
    ]
    test(environment, tests)
