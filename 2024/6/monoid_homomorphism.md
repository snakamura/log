# Expressing monoid homomorphisms in Haskell

We saw what the category of monoid is and what are morphisms in it in [the previous post](./category_of_monoids.html). Then, how can we express monoid homomorphisms in Haskell?

```
{-# LANGUAGE  GHC2024 #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeAbstractions #-}

import Data.Monoid
```

Since monoid homomorphisms are normal functions, we can express them as a newtype around a function.

```
newtype MonoidHomomorphism m1 m2 = Hom (m1 -> m2)
```

Unfortunately, we cannot express the two laws as types, but we can have functions to express them.

```
preserveIdentity ::
  (Monoid m1, Monoid m2, Eq m2) =>
  MonoidHomomorphism m1 m2 ->
  Bool
preserveIdentity @m1 @m2 (Hom f) = f (mempty @m1) == mempty @m2

preserveAppend ::
  (Monoid m1, Monoid m2, Eq m2) =>
  MonoidHomomorphism m1 m2 ->
  m1 ->
  m1 ->
  Bool
preserveAppend (Hom f) a b = f (a <> b) == f a <> f b
```

For example, when we express a monoid homomorphism from `[a]` to `Sum Int` this way,

```
hom :: MonoidHomomorphism [a] (Sum Int)
hom = Hom (Sum . length)
```

we can verify these two laws. But unfortunately at runtime.

```
testPreserveIdentity, testPreserveAppend :: Bool
testPreserveIdentity = preserveIdentity hom
testPreserveAppend = preserveAppend hom ['A', 'B'] ['C', 'D', 'E']
```

`testPreserveIdentity` and `testPreserveAppend` will be `True`, which indicates that `hom` satisfies these two laws.

You can also express this with a type class. The basic idea is to have a type class whose method returns `MonoidHomomorphism m1 m2`, but we can just have a function `m1 -> m2` instead by unwrapping this newtype.

```
class (Monoid m1, Monoid m2) => MonoidHomomorphism' m1 m2 where
  hom' :: m1 -> m2
```

Instead of having a value of `MonoidHomomorphism m1 m2` like `hom` above, well define an instance of `MonoidHomomorphism'`.

```
instance MonoidHomomorphism' [a] (Sum Int) where
  hom' :: [a] -> Sum Int
  hom' = Sum . length
```

As you can see, `hom` and `hom'` are isomorphic.

The laws are expressed as functions as well, but this time, I used visible type applications with [`RequiredTypeArguments`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/required_type_arguments.html) extension. You can do it without `RequiredTypeArguments` but with `AllowAmbiguousTypes` and `TypeApplications`, too.

```
preserveIdentity' ::
  forall m1 ->
  forall m2 ->
  (MonoidHomomorphism' m1 m2, Eq m2) =>
  Bool
preserveIdentity' m1 m2 = hom' (mempty @m1) == mempty @m2

preserveAppend' ::
  m1 ->
  m1 ->
  forall m2 ->
  (MonoidHomomorphism' m1 m2, Eq m2) =>
  Bool
preserveAppend' a b m2 = hom' @_ @m2 (a <> b) == hom' a <> hom' b
```

Finally, you can test these laws.

```
testPreserveIdentity', testPreserveAppend' :: Bool
testPreserveIdentity' = preserveIdentity' (type [Char]) (type (Sum Int))
testPreserveAppend' = preserveAppend' ['A', 'B'] ['C', 'D', 'E'] (type (Sum Int))
```

In addition to it, there is one more important property. It's a fact that `MonoidHomomorphism a b` itself is a monoid. This means that we can make it an instance of `Monoid` (and `Semigroup`).

```
instance Semigroup b => Semigroup (MonoidHomomorphism a b) where
  (Hom f) <> (Hom g) = Hom $ \a -> f a <> g a

instance Monoid b => Monoid (MonoidHomomorphism a b) where
  mempty = Hom (const mempty)
```

These instances have `Semigroup b` and `Monoid b` constraints, but this is obvious because `a` and `b` are an instance of `Monoid` in the first place. I mean, `MonoidHomomorphism a b` represents a morphism from monoid `a` to monoid `b`.

This doesn't look interesting at first, but this becomes interesting when we think about applicative functors. The fact that `MonoidHomomorphism a b` is an instance of `Monoid` means that `MonoidHomomorphism a b` is an object in the category of monoid as well as each function of type `MonoidHomomorphism a b` is a morphism in the same category. It's like a function of type `a -> b` is a morphism in `Hask` while `a -> b` itself is an object (an exponential) in `Hask`.
