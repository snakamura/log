# Expressing monoidal functors in Haskell

We looked into how we could express monoidal categories in Haskell in [Expressing monoidal category in Haskell, part 1](../5/monoidal_category1.html). We defined `MonoidalCategory` type class to express monoidal categories and `MonoidObject` type class to express objects in them. We also defined two instances of `MonoidalCategory` with a product (`(,)`) and a coproduct (`Either`).

Now that we have categories, let's define functors between them. Since they're in `Hask`, functors should implement `Functor`. In addition to that, they need to preserve monoidal structure of the categories to be monoidal functors.

What does it mean to preserve monoidal structure then?

First, it should map a unit of its source monoidal category to a unit of its target category. For example, we saw that `()` was a unit of a product monoidal category, and `Void` was a unit of a coproduct monoidal category. So a monoidal functor from `(,)` to `Either` needs to map `()` to `Void`.

Second, it should map a tensor product in its source monoidal category to a tensor product in its target category. Imagine you have objects `a` and `b` in the source monoidal category. There are two ways of mapping their tensor product.

The first way is to apply the functor to `a` and `b` and create a tensor product in the target category. You can express it as `u (f a) (f b)` where `f` is the functor and `u` is the target category. The second way is to create a tensor product in the source category and apply the functor to it. You can express it as `f (t a b)` where `t` is the source category.

To be a monoidal category, these two need to be isomorphic. So `u (f a) (f b) ≅ f (t a b)`. In other words, we need to have `forward :: u (f a) (f b) -> f (t a b)` and `backward :: f (t a b) -> u (f a) (f b)` where `backward . forward = id` and `forward . backward = id`. But this time, we relax this condition, and think about cases where we only have the former. This monoidal functor is called [a lax monoidal functor](https://ncatlab.org/nlab/show/lax+morphism).

When you apply it to `(,)` and `Either`, for example, we can say that a monoidal functor needs to have `f (a, b) -> Either (f a) (f b)`.

Now, let's make this a type class.

```
class
  ( MonoidalCategory t,
    MonoidalCategory u,
    Functor f
  ) =>
  MonoidalFunctor t u f
  where
  nu :: u (f a) (f b) -> f (t a b)
  unit :: Unit u -> f (Unit t)
```

What kind of instances do we have? First, let's define an instance of this type class to express a monoidal endofunctor from the product monoidal category to the product monoidal category. For example, you can make `Maybe` and `[]` an instance.

```
instance MonoidalFunctor (,) (,) Maybe where
  nu :: (Maybe a, Maybe b) -> Maybe (a, b)
  nu (Just a, Just b) = Just (a, b)
  nu _ = Nothing

  unit :: () -> Maybe ()
  unit () = Just ()

instance MonoidalFunctor (,) (,) [] where
  nu :: ([a], [b]) -> [(a, b)]
  nu (as, bs) = [(a, b) | a <- as, b <- bs]

  unit :: () -> [()]
  unit () = [()]
```

Monoidal endofunctors from the product monoidal category to the product monoidal category is equivalent to `Applicative`, so we can define `Applicative f` from `MonidalFunctor (,) (,) f`. This relationship is interesting, and I plan to look into it a bit more in another post.

```
instance
  ( MonoidalFunctor (,) (,) f,
    Functor f
  ) =>
  Applicative f
  where
  (<*>) :: f (a -> b) -> f a -> f b
  fab <*> fa = (\(ab, a) -> ab a) <$> nu (fab, fa)

  pure :: a -> f a
  pure a = a <$ unit @(,) @(,) ()
```

Next, let's see how we can express a monoidal functor from the coproduct monoidal category to the product monoidal category. Again, we'll make `Maybe` and `[]` its instances.

```
instance MonoidalFunctor Either (,) Maybe where
  nu :: (Maybe a, Maybe b) -> Maybe (Either a b)
  nu (Just a, _) = Just $ Left a
  nu (_, Just b) = Just $ Right b
  nu _ = Nothing

  unit :: () -> Maybe Void
  unit _ = Nothing

instance MonoidalFunctor Either (,) [] where
  nu :: ([a], [b]) -> [Either a b]
  nu (as, bs) = (Left <$> as) <> (Right <$> bs)

  unit :: () -> [Void]
  unit _ = []
```

These instances map objects and morphisms in the coproduct monoidal category to the product monoidal category while preserving their monoidal structure.

Functors from the coproduct monoidal category to the product monoidal category is equivalent to `Alternative`, and you can define `Alternative f` from `MonoidalFunctor Either (,) f` (and `Applicative f`).

```
instance
  ( MonoidalFunctor Either (,) f,
    Functor f,
    Applicative f
  ) =>
  Alternative f
  where
  (<|>) :: f a -> f a -> f a
  fa1 <|> fa2 = either id id <$> nu (fa1, fa2)

  empty :: f a
  empty = absurd <$> unit @Either @(,) ()
```
