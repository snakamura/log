# CPS to `Mu`

Imagine that you have a function of type `a -> b`.

```
f :: a -> b
```

You can convert it to a function taking `a` and `b -> r`, then returning `r`.

```
f' :: a -> (b -> r) -> r
```

This is called CPS (Continuation Passing Style) because you're passing `b -> r` to `f'` which is what you'd like to do with `b` next.

For example, imagine that you have this function which adds `1`.

```
addOne :: Int -> Int
addOne n = n + 1
```

Its CPS version is this `addOne'`.

```
addOne' :: Int -> (Int -> r) -> r
addOne' n f = f (n + 1)
```

 If this function takes no arguments, it'll be `g :: () -> (b -> r) -> r` which is isomorphic to `g' :: (b -> r) -> r`. So having a value `b` is the same thing as passing `b -> r` to `g'`.

 ```
 true :: Bool
 true = True

 true' :: (Bool -> r) -> r
 true' f = f True
 ```

Now, let's expand this a bit. If we can express a single boolean value as a function taking a boolean and returning `r`, we should be able to express any boolean value as a function taking two `() -> r` and returning `r` (`(() -> r) -> (() -> r) -> r`). A caller will pass a function to be invoked for `True` as a first argument, and `False` as a second argument. A function expressing `True` invokes the first argument, and a function expressing `False` invokes the second argument. Since `() -> r` is isomorphic to `r`, we'll use `r -> r -> r` instead of `(() -> r) -> (() -> r) -> r`.

```
type Bool = forall r. r -> r -> r

true, false :: Bool
true = \t f -> t
false = \t f -> f
```

Another example is expressing natural numbers. Any natural number can be represented by zero or a natural number next to another natural number. Functions expressing natural numbers take these two options as parameters; a function returning a next value (`f`) and a value representing zero (`z`). `zero` returns the second parameter `z`, `one` applies the function `f` to the value `z`, `two` applies the functions twice, and so on. The function returns a next natural number (`succ`) applies `f` and `z` to the number, and apply `f` one more time.

```
type Natural = forall r. (r -> r) -> r -> r

zero, one, two, three :: Natural
zero = \f z -> z
one = \f z -> f z
two = \f z -> f (f z)
three = \f z -> f (f (f z))

succ :: Natural -> Natural
succ n = \f z -> f (n f z)
```

Structures such as pairs can be represented in this style as well. For example, a pair will be represented by a function taking a function that takes two values `a` and `b`.

```
type Pair a b = forall r. (a -> b -> r) -> r

pair :: a -> b -> Pair a b
pair a b = \f -> f a b

fst :: Pair a b -> a
fst p = p (\a b -> a)

snd :: Pair a b -> b
snd p = p (\a b -> b)
```

The same goes for a list. A list is empty or a value with another list. So it'll be represented as a function takes a function taking a value and another list `a -> r -> r`, and a value for an empty list `r`.

```
type List a = forall r. (a -> r -> r) -> r -> r

nil :: List a
nil = \f r -> r

cons :: a -> List a -> List a
cons a as = \f r -> f a (as f r)

fold :: (a -> r -> r) -> r -> List a -> r
fold f a as = as f a
```

This construct is called church encoding.

When you look at `type Bool = forall r. r -> r -> r`, you'll find that we can transform it this way.

```
   forall r. r -> r -> r
=> forall r. (r, r) -> r
=> forall r. (() -> r, () -> r) -> r
=> forall r. (Either () () -> r) -> r
=> forall r. (Sum (Const ()) (Const ()) r -> r) -> r
```

Now you can write `Bool` this way.

```
type Bool = forall r. (Sum (Const ()) (Const ()) r -> r) -> r

true, false :: Bool
true = \alg -> alg (InL (Const ()))
false = \alg -> alg (InR (Const ()))
```

Applying the same steps to `Number`, `Pair`, and `List` will result in these types.

```
type Natural = forall r. (Sum (Const ()) Identity r -> r) -> r

zero, one, two, three :: Natural
zero = \alg -> alg (InL (Const ()))
one = \alg -> alg (InR (Identity (alg (InL (Const ())))))
two = \alg -> alg (InR (Identity (alg (InR (Identity (alg (InL (Const ()))))))))
three = \alg -> alg (InR (Identity (alg (InR (Identity (alg (InR (Identity (alg (InL (Const ())))))))))))

succ :: Natural -> Natural
succ n = \alg -> alg (InR (Identity (n alg)))
```

```
type Pair a b = forall r. (Product (Const a) (Const b) r -> r) -> r

pair :: a -> b -> Pair a b
pair a b = \alg -> alg (Pair (Const a) (Const b))

fst :: Pair a b -> a
fst p = p (\(Pair (Const a) (Const b)) -> a)

snd :: Pair a b -> b
snd p = p (\(Pair (Const a) (Const b)) -> b)
```

```
type List a = forall r. (Sum (Const ()) (Product (Const a) Identity) r -> r) -> r

nil :: List a
nil = \alg -> alg (InL (Const ()))

cons :: a -> List a -> List a
cons a as = \alg -> alg (InR (Pair (Const a) (Identity (as alg))))

fold :: (a -> r -> r) -> r -> List a -> r
fold f a as = as g
  where
    g (InL (Const ())) = a
    g (InR (Pair (Const a') (Identity as'))) = f a' as'
```

You should've noticed that all of them are in the form of `forall r. (f r -> r) -> r` for some functor `f`. And, it's the `Mu` we saw in [the previous post](./strict_fix2.html).

```
type Bool = Mu (Sum (Const ()) (Const ()))

true, false :: Bool
true = In $ \alg -> alg (InL (Const ()))
false = In $ \alg -> alg (InR (Const ()))

type Natural = Mu (Sum (Const ()) Identity)

zero, one, two, three :: Natural
zero = In $ \alg -> alg (InL (Const ()))
one = In $ \alg -> alg (InR (Identity (alg (InL (Const ())))))
two = In $ \alg -> alg (InR (Identity (alg (InR (Identity (alg (InL (Const ()))))))))
three = In $ \alg -> alg (InR (Identity (alg (InR (Identity (alg (InR (Identity (alg (InL (Const ())))))))))))

succ :: Natural -> Natural
succ (In n) = In $ \alg -> alg (InR (Identity (n alg)))

type Pair a b = Mu (Product (Const a) (Const b))

pair :: a -> b -> Pair a b
pair a b = In $ \alg -> alg (Pair (Const a) (Const b))

fst :: Pair a b -> a
fst (In p) = p (\(Pair (Const a) (Const b)) -> a)

snd :: Pair a b -> b
snd (In p) = p (\(Pair (Const a) (Const b)) -> b)

type List a = Mu (Sum (Const ()) (Product (Const a) Identity))

nil :: List a
nil = In $ \alg -> alg (InL (Const ()))

cons :: a -> List a -> List a
cons a (In as) = In $ \alg -> alg (InR (Pair (Const a) (Identity (as alg))))

fold :: (a -> r -> r) -> r -> List a -> r
fold f a (In as) = as g
  where
    g (InL (Const ())) = a
    g (InR (Pair (Const a') (Identity as'))) = f a' as'
```

You should remember that `Mu` represents the least fixed point of a functor. This means that `Bool` is the least fixed point of `Sum (Const ()) (Const ())` ($F a = 1 + 1$), `Natural` is the least fixed point of `Sum (Const ()) Identity`, `Pair` is the least fixed point of `Product (Const a) (Const b)`, `List` is the least fixed point of `Sum (Const ()) (Product (Const a) Identity)` ($F a = 1 + (a \times r)$), and so on.
