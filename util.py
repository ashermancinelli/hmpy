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
