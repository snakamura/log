# Monoidal functor

I was wondering what it meant when it comes to Xyz functor such as monoidal functor. It turned out that it meant a functor with additional properties in addition to the functor properties. For example, [monoidal functor](https://ncatlab.org/nlab/show/monoidal+functor) is a functor that preserves a monoidal structure.

Let's take an example of product of types. When you have `a` and `b`, you can have their product type `(a, b)`, but this forms a monoidal structure with `()` as an identity. It satisfies `((a, b), c) ≡ (a, (b, c))`, `(a, ()) ≡ a`, and `((), a) ≡ a`.

When a functor preserves this monoidal structure, you can apply `fmap` then create a product, or create a product then apply `fmap` to get the same result.

For example, imagine you have `10` and `"foo"` and apply `Maybe` as the functor. You'll get `(Just 10, Just "foo")` when you first apply `fmap` then create a product, and get `Just (10, "foo")` when you first create a product and apply `fmap` after that.

As you can see, you can define such function like this.

```
f :: (Maybe a, Maybe b) -> Maybe (a, b)
f (Just x, Just y) = Just (x, y)
f _ = Nothing
```

You also need a function to convert a unit to a unit to make it preserve the associativity.

```
e :: () -> Maybe ()
e () = Just ()
```

Let's make it more generic using type classes.

```
{-# LANGUAGE FlexibleInstances,
             InstanceSigs,
             ScopedTypeVariables,
             UndecidableInstances
#-}

class Functor f => Monoidal f where
    mu :: (f a, f b) -> f (a, b)
    epsilon :: () -> f ()
```

This `Monoidal` class represents a functor that preserves a monoidal structure in terms of product. You can use the implementation of `f` and `e` above to make `Maybe` an instance of `Monoidal`.

```
instance Monoidal Maybe where
    mu :: (Maybe a, Maybe b) -> Maybe (a, b)
    mu (Just a, Just b) = Just (a, b)
    mu _ = Nothing

    epsilon :: () -> Maybe ()
    epsilon () = Just ()
```

Interestingly enough, `Monoidal` is identical to `Applicative`, so you can define `Applicative` in terms of `Monoidal`.

```
instance (Functor f, Monoidal f) => Applicative f where
    (<*>) :: forall a b. f (a -> b) -> f a -> f b
    ff <*> fa = let fp :: f (a -> b, a) = mu (ff, fa)
                in fmap (\(f, a) -> f a) fp

    pure :: a -> f a
    pure a = fmap (const a) (epsilon ())
```
