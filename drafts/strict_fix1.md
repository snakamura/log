# Fixed point in Strict Haskell, part 1

In Haskell, we often use `Fix` to express a fixed point of a functor.

```
type Fix :: (Type -> Type) -> Type
newtype Fix f = Fix (f (Fix f))
```

For example, a fixed point of `Maybe` is natural numbers.

```
type Nat :: Type
type Nat = Fix Maybe
```

You can express 0, 1, and 2 like this.

```
zero, one, two :: Nat
zero = Fix Nothing
one = Fix (Just (Fix Nothing))
two = Fix (Just (Fix (Just (Fix Nothing))))
```

`Fix` has these four basic operations.

```
-- Embed a functor in a fixed point
embed :: f (Fix f) -> Fix f
embed = Fix

-- Extract a functor from a fixed point
project :: Fix f -> f (Fix f)
project (Fix ffixf) = ffixf

-- Catamorphism (Apply an F-algebra recursively to fold a value from a value of the fixed point)
cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . project

-- Anamorphism (Apply an F-coalgebra recursively to generate a value of the fixed point type)
ana :: (Functor f) => (a -> f a) -> a -> Fix f
ana coalg = embed . fmap (ana coalg) . coalg
```

You can have functions to convert `Natural` to `Nat` and back and forth using `cata` and `ana`.

```
fromNatural :: Natural -> Nat
fromNatural = ana coalg
  where
    coalg :: Natural -> Maybe Natural
    coalg 0 = Nothing
    coalg n = Just $ n - 1

toNatural :: Nat -> Natural
toNatural = cata alg
  where
    alg :: Maybe Natural -> Natural
    alg Nothing = 0
    alg (Just n) = n + 1
```

An interesting point is that it can express infinity.

```
inf :: Nat
inf = Fix (Just inf)
```

For example, when you apply `inf` to this `is`, you'll get `False`. For example, `is 100 inf` will be `False`.

```
is :: Natural -> Nat -> Bool
is n nat = cata alg nat n
  where
    alg :: Maybe (Natural -> Bool) -> (Natural -> Bool)
    alg Nothing = (== 0)
    alg (Just f) = \n' -> n' > 0 && f (n' - 1)
```

This is because Haskell is lazy. When you enable `Strict` extension, `is 100 inf` won't terminate because `inf` tries to build an infinite chain like `Fix (Just (Fix (Just (Fix (Just ...)))))`. How can we express infinity with Strict Haskell then?

One of the options is to make `Fix` hold a function instead of a value.

```
type Fix' :: (Type -> Type) -> Type
newtype Fix' f = Fix' (() -> f (Fix' f))

type Nat' :: Type
type Nat' = Fix' Maybe

inf' :: Nat'
inf' = Fix' (\_ -> Just inf')
```

Now, you can build `inf'` because it won't build its child until the function is called. But it's just emulating laziness. Are there any other representations?

Theoretically, `Fix` represents the least fixed point of a functor $\mu F$ ($F$ is a functor) which only contains finite structures and doesn't contain an indefinitely nested structure. There is a greatest fixed point $\nu F$ as well which contains an indefinitely nested structure. While the least fixed point was built by building a nested structure, the greatest fixed point is built by observing a nested structure.

In Haskell, $\nu F$ can be represented as `Nu`.

```
type Nu :: (Type -> Type) -> Type
data Nu f where
  Out :: (s -> f s) -> s -> Nu f
```

As you can see, it's an anamorphism itself. It just knows how to build a fixed point from a coalgebra and an initial value. So its `ana` is identical to its constructor `Out`.

```
ana :: (Functor f) => (a -> f a) -> a -> Nu f
ana = Out
```

How can we build values of of `Nu Maybe`, for example?

```
type Nat :: Type
type Nat = Nu Maybe

zero, one :: Nat
zero = Out (\() -> Nothing) ()
one = Out f True
  where
    f False = Nothing
    f True = Just False
```

As you can see, we need a coalgebra for one value to represent `zero` (`Maybe () -> ()`), and need two values to represent `one` (`Maybe Bool -> Bool`). When you have a natural number `n`, it can have `n + 1` values (from `0` to `n`), and you can express it as `Nat`.

```
fromNatural :: Natural -> Nat
fromNatural = ana coalg
  where
    coalg :: Natural -> Maybe Natural
    coalg 0 = Nothing
    coalg n = Just $ n - 1
```

`inf` can be represented with a coalgebra that always generate `Just ()`.

```
inf :: Nat
inf = ana (\_ -> Just ()) ()
```

When you define `is`, `is 100 inf` returns `False` even with `Strict` extension.

```
is :: Natural -> Nat -> Bool
is n nat = case (project nat, n) of
    (Nothing, 0) -> True
    (Nothing, _) -> False
    (Just nat', n') -> n' > 0 && is (n' - 1) nat'
```

Note that `project` is a function to unwrap one layer.

```
project :: (Functor f) => Nu f -> f (Nu f)
project (Out coalg value) = Out coalg <$> coalg value
```

Let's define the two other basic functions `cata` and `embed` now. The definition of `cata` is identical to `cata` for `Fix`.

```
cata :: (Functor f) => (f a -> a) -> Nu f -> a
cata alg = alg . fmap (cata alg) . project
```

It first unwraps one layer to get `f (Nu f)`, then applies `alg` recursively to its content `Nu f` with `cata` to get `f a`, then apply `alg` to get `a`.

`embed` is a bit more complex. We need to have a function that first generates a passed value (`f (Nu f)`), then generates original values in `Nu f`. We can do this by first wrapping `f (Nu f)` in `Right` and applying anamorphism to it. The coalgebra returns `f (Nu f)` for the first value in `Right`, and returns a content of `Nu f` by applying `project` for the other values in `Left`.

```
embed :: (Functor f) => f (Nu f) -> Nu f
embed @f = ana coalg . Right
  where
    coalg :: Either (Nu f) (f (Nu f)) -> f (Either (Nu f) (f (Nu f)))
    coalg (Left nu) = Left <$> project nu
    coalg (Right fnuf) = Left <$> fnuf
```

This folding is very common and can be factored out as apomorphism. Then, `embed` can be written using it.

```
apo :: (Functor f) => (a -> f (Either (Nu f) a)) -> a -> Nu f
apo @f @a psi = ana coalg . Right
  where
    coalg :: Either (Nu f) a -> f (Either (Nu f) a)
    coalg (Left nu) = Left <$> project nu
    coalg (Right a) = psi a

embed :: (Functor f) => f (Nu f) -> Nu f
embed = apo (fmap Left)
```

Even though `is 100 inf` terminates, it won't terminate when you write it with `cata`.

```
is' :: Natural -> Nat -> Bool
is' n nat = cata alg nat n
 where
    alg :: Maybe (Natural -> Bool) -> (Natural -> Bool)
    alg Nothing = (== 0)
    alg (Just f) = \n' -> n' > 0 && f (n' - 1)
```

This is because `cata` tries to fold the entire structure and won't terminate until it reaches its leaf.
