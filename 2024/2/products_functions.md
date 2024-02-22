# Products and functions

A product `(,)` and a function `(->)` can be functors by fixing their first argument, like `(,) e` and `(->) e`. For instance, `fmap (+1) ('a', 10)` is `('a', 11)`, and `fmap (+1) (+10)` is `(+11)`.

When you compose two functors, you get another functor. You can use [`Data.Functor.Compose`](https://hackage.haskell.org/package/base/docs/Data-Functor-Compose.html#t:Compose) to express a composition of functors.

For example, when you compose two product functors, you'll get `Compose ((,) a) ((,) b)`. If you apply it to `Int`, you'll get `Compose ((,) a) ((,) b) Int` which can be expanded to `(a, (b, Int))`. `getCompose $ fmap (+1) (Compose ('a', (True, 10)))` becomes `('a', (True, 11))`.

The same goes for functions. `Compose ((->) a) ((->) b) c` is expanded to `a -> (b -> c)`. So `(getCompose $ fmap (+1) (Compose (\a b -> a * b))) 5 10` is `51`.

Now, let's try composing a product and a function. There are two ways to compose them. The first one is `Compose ((->) a) ((,) b)` and the second one is `Compose ((,) a) ((->) b)`. To make it simpler, let's think only about when `a` and `b` are the same type; `Compose ((->) e) ((,) e)` and `Compose ((,) e) ((->) e)`.

When you use `a` as a functor's contained type, the first one is `Compose ((->) e) ((,) e) a` which is expanded to `e -> (e, a)`. Applying the same to the second one results in `Compose ((,) e) ((->) e) a` which is expanded to `(e, e -> a)`.

Imagine you have `a`, and you want to get the first one, `e -> (e, a)`, what you need is a function of type `a -> e -> (e, a)`. When you have the second one, `(e, e -> a)`, and you want to get `a`, you need a function of type `(e, e -> a) -> a`.

Implementing these functions is trivial. The first one is this function.

```
unit' :: a -> e -> (e, a)
unit' a e = (e, a)
```

The second one is this function.

```
counit' :: (e, e -> a) -> a
counit' (e, f) = f e
```

When you remember that they're compositions of a product functor and a function functor, you can write them like this.

```
unit :: a -> Compose ((->) e) ((,) e) a
unit a = Compose (\e -> (e, a))

counit :: Compose ((,) e) ((->) e) a -> a
counit (Compose (e, f)) = f e
```

Putting them together, we can say these things.

1. When you have `a`, first apply `(,) e`, then apply `(->) e` to get `Compose ((->) e) ((,) e) a`. You can get the same result by applying `unit` to `a`.
2. When you have `a`, first apply `(->) e`, then `(,) e`, and apply `counit` to the result to get `a` back.

You can say that we have two circles now.

Okay, then, let's give another relationship between a product and a function a look. They're `curry` and `uncurry`.

```
curry :: ((a, b) -> c) -> (a -> b -> c)
curry f a b = f (a, b)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (a, b) = f a b
```

When you flip the first argument of `f` in `curry`, you can get this type.

```
curry' :: ((b, a) -> c) -> (a -> b -> c)
curry' f a b = f (b, a)
```

which can be expanded to

```
curry'' :: (((,) b) a -> c) -> (a -> ((->) b) c)
curry'' f a b = f (b, a)
```

`(,) b` is a product functor and `((->) b)` is a function functor. So you can think this is a relationship between them.

The same goes for `uncurry`. When you expand it, you'll get this type.

```
uncurry' :: (a -> b -> c) -> ((b, a) -> c)
uncurry' f (b, a) = f a b

uncurry'' :: (a -> ((->) b) c) -> (((,) b) a -> c)
uncurry'' f (b, a) = f a b
```

Again, we found another relationship between a product functor and a function functor.

But wait, they are actually the same relationship. Let's see how we can find it. First, let's check the type of `curry'' id`. When you replace `c` in `a -> b -> c` with `(b, a)`, you'll get this.

```
curry'' id :: a -> b -> (b, a)
```

This type is identical to the type of `unit'` above.

Let's do the same with `uncurry''`. When you replace `a` in `((b, a) -> c)` with `b -> c`, you'll get this.

```
uncurry'' id :: (b, b -> c) -> c
```

This type is identical to the type of `counit'` above.

Is it possible to get them the other way? Like getting `curry''` from `unit'` and `uncurry''` from `counit'`?

The type of the first argument of `curry''`, named `f`, is `(b, a) -> c`. When you lift it to any functor, you can get `f (b, a) -> f c`, which is the type of `fmap f`. When you use `((->) e)` as a functor, you'll get `((->) e (b, a)) -> ((->) e c)`, which is `(e -> (b, a)) -> (e -> c)`.

As you remember, the type of `unit'` was `a -> (e -> (e, a))`, so you can compose `fmap f` with `unit'` by unifying `e` with `b` to get `fmap f . unit :: a -> b -> c`. This is a return type of `curry''`.

Next, let's see what we can do with `uncurry''`. The type of the first argument of `uncurry''`, named `f`, is `a -> b -> c`. So the type of `fmap f` is `f a -> f (b -> c)`. Now, use `((,) e)` as a functor and you'll get `((,) e) a -> ((,) e) (b -> c)`, which is expanded to `(e, a) -> (e, b -> c)`.

The type of `counit'` was `(e, e -> a) -> a`, so you can compose `fmap f` with `counit'` by unifying `e` with `b` to get `counit' . fmap f :: (b, a) -> c`. This is a return type of `uncurry''`.

To sum up, we now have these equalities.

```
curry'' f = fmap f . unit'
uncurry'' f = counit' . fmap f
```

By putting everything together and working with `Compose` wrapper, we now have these four relationships.

```
unit :: a -> Compose ((->) e) ((,) e) a
unit a = Compose (\e -> (e, a))

counit :: Compose ((,) e) ((->) e) a -> a
counit (Compose (e, f)) = f e

curry :: (((,) e) a -> b) -> (a -> ((->) e) b)
curry f a b = f (b, a)

uncurry :: (a -> ((->) e) b) -> (((,) e) a -> b)
uncurry f (b, a) = f a b
```

And we can say `curry id = getCompose . unit`, `uncurry id = counit . Compose`, `curry f = fmap f . getCompose . unit` and `uncurry f = counit . Compose . fmap f`.

When you generalize a product functor to any functor `f`, and a function functor to `g`, we can write them like this.

```
unit :: (Functor f, Functor g) => a -> Compose g f a
counit :: (Functor f, Functor g) => Compose f g a -> a
curry :: (Functor f, Functor g) => (f a -> b) -> (a -> g b)
uncurry :: (Functor f, Functor g) => (a -> g b) -> (f a -> b)
```

By removing `Compose` newtype, by converting `Compose f g a` to `f (g a)`, we can get these definitions.

```
unit :: (Functor f, Functor g) => a -> g (f a)
counit :: (Functor f, Functor g) => f (g a)
curry :: (Functor f, Functor g) => (f a -> b) -> (a -> g b)
uncurry :: (Functor f, Functor g) => (a -> g b) -> (f a -> b)
```

where `curry id = unit`, `uncurry id = counit`, `curry f = fmap f . unit` and `uncurry f = counit . fmap f`.

Can we say all combinations of any functors `f` and `g` satisfy them? No, we can't.

A pair of functors that satisfies them is called an adjunction and is expressed with [`Adjunction`](https://hackage.haskell.org/package/adjunctions/docs/Data-Functor-Adjunction.html#t:Adjunction) type class. `curry` is called `leftAdjunct` and `uncurry` is called `rightAdjunct` in it.

Here is a slightly simplified version of `Adjunction`.

```
class (Functor f, Functor g) => Adjunction f g | f -> g, g -> f where
  unit :: a -> g (f a)
  unit = leftAdjunct id

  counit :: f (g a) -> a
  counit = rightAdjunct id

  leftAdjunct :: (f a -> b) -> (a -> g b)
  leftAdjunct f = fmap f . unit

  rightAdjunct :: (a -> g b) -> (f a -> b)
  rightAdjunct f = counit . fmap f
```

This pair of functors have interesting characteristics, but in Haskell, we actually don't have any pairs of functors other than a product and a function that can be an instance of this type class. `Identity` can be an instance like `instance Adjunction Identity Identity`, but `Identity` is same as `((,) ())` and also `((->) ())`. So it's a special case of a product functor and a function functor.
