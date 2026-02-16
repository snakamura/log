# Strong and closed functors

In Haskell, we call it a strong functor when a functor has this function `strength`.

```
class (Functor f) => ProductStrongFunctor f where
  strength :: (a, f b) -> f (a, b)
```

As you can see, a functor that can lift a value into a pair is a strong functor. For example, you can implement an instance of this typeclass for `Maybe`.

```
instance ProductStrongFunctor Maybe where
  strength :: (a, Maybe b) -> Maybe (a, b)
  strength (a, Just b) = Just (a, b)
  strength (_, Nothing) = Nothing
```

Actually, you can implement this typeclass for any functor.

```
instance (Functor f) => ProductStrongFunctor f where
  strength :: (a, f b) -> f (a, b)
  strength (a, fb) = fmap (\b -> (a, b)) fb
```

It means that any functor in Haskell is a strong functor.

To be more precise, this is left strength because it lifts `a` on the left. There is also right strength, but you can get it by swapping elements in a tuple in Haskell (This isn't always true when you think about a asymmetric tensor in another category). When we talk about a strong functor, it usually means a left strong functor.

Also note that this strong functor is different from strong monoidal functor that satisfies `f (a, b)` and `(f a, f b)` are isomorphic ($F (A \otimes B) \cong (F A \otimes F B)$). We saw it in [Expressing monoidal functors in Haskell](../../2024/6/monoidal_functor.html).

Let's think a bit more about a type of `strength`. Its type is `(a, f b) -> f (a, b)` which is equivalent to `((,) a) (f b) -> f (((,) a) b)`. When you replace `(,) a` with `g`, it'll be `g (f b) -> f (g b)`. As we saw in [`Traversable`, `Lone` and `Distributive`](../../2023/7/traversable_lone_distributive.html), this holds when `g` is `Lone`, or `f` is `Distributive`. Since `(,) a` is `Lone`, we can define `strength` for any functor `f`.

We've defined a strong functor in terms of a pair functor `(,) a`, but you can define it in terms of any tensor products in a monoidal category. For example, you can define it with `Either a`.

```
class (Functor f) => CoproductStrongFunctor f where
  strength :: Either a (f b) -> f (Either a b)
```

`Maybe` is an instance of this type class, too.

```
instance CoproductStrongFunctor Maybe where
  strength :: Either a (Maybe b) -> Maybe (Either a b)
  strength (Left a) = Just (Left a)
  strength (Right fb) = fmap Right fb
```

This time, we cannot say that any functor in Haskell is a strong functor in terms of `Either a` because `Either a` isn't `Lone`. But since `Either a` is `Traversable`, you can implement this typeclass for any applicative functor `f`.

```
instance (Functor f, Applicative f) => CoproductStrongFunctor f where
  strength :: Either a (f b) -> f (Either a b)
  strength (Left a) = pure (Left a)
  strength (Right fb) = fmap Right fb
```

Now, let's take a look at what we can do with other functors. The type of `strength` was `g (f b) -> f (g b)` where `g` was `(,) a`. Then what will it look like when we use `(->) a` as `g` this time? We'll get `(-> a) (f b) -> f (((->) a) b)` which is `(a -> f b) -> f (a -> b)`. Let's call a functor having this function a closed functor.

```
class (Functor f) => ClosedFunctor f where
  closed :: (a -> f b) -> f (a -> b)
```

For example, `(->) r` is an instance of this typeclass.

```
instance ClosedFunctor ((->) r) where
  closed :: (a -> (r -> b)) -> (r -> (a -> b))
  closed a2fb = \r a ->
    let fb = a2fb a
     in fb r
```

Will any functor be an instance of this typeclass? No, but a functor which is an instance of `Distributive` will be a closed functor. As we've just saw above, we have `g (f b) -> f (g b)` when `f` is `Distributive`, and we just picked `(->) a` as `g`.

```
instance (Functor f, Distributive f) => ClosedFunctor f where
  closed :: (a -> f b) -> f (a -> b)
  closed a2fb = distribute a2fb
```

Strictly speaking, `a -> f b` and `f (a -> b)` must be isomorphic for `f` to be a closed functor. It means that there needs to be `unclosed :: f (a -> b) -> (a -> f b)`, where both `unclosed . closed == id` and `closed . unclosed == id` hold. But `unclosed` can be defined for any functors, and a pair of `closed` above and this `unclosed` satisfy these two conditions.

```
unclosed :: (Functor f) => f (a -> b) -> (a -> f b)
unclosed fa2b = \a -> fmap (\a2b -> a2b a) fa2b
```

Note that this closed functor is different from lax closed functor that has `f (a -> b) -> (f a -> f b)`.
