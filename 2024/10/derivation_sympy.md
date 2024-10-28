# Derivation with SymPy

When you want to calculate a derivation of an expression, [SymPy](https://www.sympy.org/) will help. For instance, when you want to calculate the derivation of `f(x) = x ** 2`, let SymPy do it for you.

```
import sympy as sp

# Define symbols
x = sp.symbols('x')

# This is your function
f = x ** 2

# Calculate the derivation
d = sp.diff(f, x)

print(d) # 2*x
```

To use mathematical functions like `sin`, use it in SymPy.

```
import sympy as sp

x = sp.symbols('x')
f = sp.sin(x)
d = sp.diff(f, x)

print(d) # cos(x)
```

You can calculate partial derivations as well.

```
import sympy as sp

x, y = sp.symbols('x, y')
f = sp.sin(x) * y
dx = sp.diff(f, x)
dy = sp.diff(f, y)

print(dx) # y*cos(x)
ptint(dy) # sin(x)
```

Also Jacobian matrix.

```
import sympy as sp

x, y = sp.symbols('x, y')
h = sp.Matrix([sp.sqrt(x ** 2 + y ** 2), sp.atan(y / x)])
j = h.jacobian([x, y])

print(j) # Matrix([[x/sqrt(x**2 + y**2), y/sqrt(x**2 + y**2)], [-y/(x**2*(1 + y**2/x**2)), 1/(x*(1 + y**2/x**2))]])
```
