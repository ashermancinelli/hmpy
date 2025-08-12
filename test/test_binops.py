# RUN: python3 %s | FileCheck %s
from harness import *

typecheck([
    Let(f, Lambda(x, Lambda(y, Add(x, y))), f),
])
# CHECK: Typed result:  (T47 -> (T47 -> T47))
