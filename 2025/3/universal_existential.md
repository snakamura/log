# Universal and existential quantification in Rust, Swift and Haskell

In Rust, you can return `Box<dyn X>` (or other smart pointers) or `impl X` to return an existential type. You can hide an actual type using `impl X` without boxing a value.

```
trait X {}

struct X1;
impl X for X1 {}

// A callee decides a type which a caller can't know, boxed
fn x1() -> Box<dyn X> { Box::new(X1) }

// A callee decides a type which a caller can't know, unboxed
fn x2() -> impl X { X1 }

trait Y {
    fn new() -> Self;
}

struct Y1;
impl Y for Y1 {
    fn new() -> Self { Y1 }
}

// A caller decides a type
fn y1<T: Y>() -> T { T::new() }

fn main() {
    let v1 = x1();
    let v2 = x2();
    let v3: Y1 = y1();
    let v4 = y1::<Y1>();
}
```

Swift has very similar mechanics. Use `any X` instead of `Box<dyn X>`, and `some X` instead of `impl X`. Swift automatically boxes a value with `any X` when necessary.

```
protocol X {}

struct X1: X {}

// A callee decides a type which a caller can't know, boxed
func x1() -> any X { X1() }

// A callee decides a type which a caller can't know, unboxed
func x2() -> some X { X1() }

protocol Y {
    static func new() -> Self
}

struct Y1: Y {
    static func new() -> Y1 { Y1() }
}

// A caller decides a type
func y1<T: Y>() -> T { T.new() }

let v1 = x1()
let v2 = x2()
let v3: Y1 = y1()
// let v4 = y1<Y1>() // Swift doesn't allow this
```

Haskell doesn't have an equivalent of `impl X` or `some X` since values are usually boxed.

```
import Data.Kind

type Some :: (Type -> Constraint) -> Type
data Some c where
  Some :: c a => a -> Some c

class X a

data X1 = X1
instance X X1

-- A callee decides a type which a caller can't know, boxed
x1 :: Some X
x1 = Some X1

class Y a where
  new :: a

data Y1 = Y1
instance Y Y1 where
  new = Y1

-- A caller decides a type
y1 :: Y a => a
y1 = new

v1 :: Some X
v1 = x1

v2 :: Y1
v2 = y1
```
